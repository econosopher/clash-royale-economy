#!/usr/bin/env Rscript

# Lightweight CLI wrapper to run a season simulation and optionally export CSV.

suppressWarnings(suppressMessages({
  # Use project-relative sources; assume invoked from repo root via `Rscript R/run_sim.R`
  source("R/sim_config.R")
  source("R/lucky_star.R")
  source("R/sim_economy.R")
}))

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  # Very small flag parser: supports --key value pairs
  out <- list()
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (startsWith(a, "--")) {
      key <- substring(a, 3)
      if ((i + 1L) <= length(args) && !startsWith(args[[i + 1L]], "--")) {
        out[[key]] <- args[[i + 1L]]
        i <- i + 2L
      } else {
        out[[key]] <- TRUE
        i <- i + 1L
      }
    } else i <- i + 1L
  }
  out
}

opts <- parse_args(args)

# Build config from defaults, overriding with provided flags
days <- if (!is.null(opts$days)) as.integer(opts$days) else 30L
matches <- if (!is.null(opts$matches_per_day)) as.integer(opts$matches_per_day) else 15L
seed <- if (!is.null(opts$seed)) as.integer(opts$seed) else 42L

cfg <- default_config(days = days, matches_per_day = matches)

if (!is.null(opts$base_winrate)) cfg$base_winrate <- as.numeric(opts$base_winrate)
if (!is.null(opts$vertical_winrate_bonus)) cfg$vertical_winrate_bonus <- as.numeric(opts$vertical_winrate_bonus)
if (!is.null(opts$bot_rate)) cfg$bot_rate <- as.numeric(opts$bot_rate)
if (!is.null(opts$bot_winrate)) cfg$bot_winrate <- as.numeric(opts$bot_winrate)
if (!is.null(opts$trophies_on_win)) cfg$trophies_on_win <- as.integer(opts$trophies_on_win)
if (!is.null(opts$trophies_on_loss)) cfg$trophies_on_loss <- as.integer(opts$trophies_on_loss)

# Gold configuration: support fixed or range
if (!is.null(opts$gold_fixed_amount)) cfg$gold_drop$fixed_amount <- as.integer(opts$gold_fixed_amount)
if (!is.null(opts$gold_min_amount)) cfg$gold_drop$min_amount <- as.integer(opts$gold_min_amount)
if (!is.null(opts$gold_max_amount)) cfg$gold_drop$max_amount <- as.integer(opts$gold_max_amount)

# Pass configuration
if (!is.null(opts$crowns_per_level)) cfg$pass_config$crowns_per_level <- as.integer(opts$crowns_per_level)
if (!is.null(opts$gold_per_level)) cfg$pass_config$gold_per_level <- as.integer(opts$gold_per_level)

# Lucky Star quick toggles
if (!is.null(opts$lucky_enabled)) cfg$lucky_drop$enabled <- tolower(as.character(opts$lucky_enabled)) %in% c("1","true","yes")
if (!is.null(opts$lucky_wins)) cfg$lucky_drop$wins_with_lucky_drops <- as.integer(opts$lucky_wins)
if (!is.null(opts$lucky_spins)) cfg$lucky_drop$spins <- as.integer(opts$lucky_spins)

res <- simulate_season(cfg, seed = seed)
daily <- res$daily

if (!is.null(opts$csv_out)) {
  # Flatten list-columns for CSV friendliness
  flatten_vec <- function(x, nm) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    paste0(names(x), ":", x, collapse = "|")
  }
  out <- daily
  if ("box_drops_by_rarity" %in% names(out)) out$box_drops_by_rarity <- vapply(out$box_drops_by_rarity, function(x) flatten_vec(x[[1]], NULL), character(1))
  if ("gold_spent_by_rarity" %in% names(out)) out$gold_spent_by_rarity <- vapply(out$gold_spent_by_rarity, flatten_vec, character(1))
  if ("power_gained_by_rarity" %in% names(out)) out$power_gained_by_rarity <- vapply(out$power_gained_by_rarity, flatten_vec, character(1))
  readr::write_csv(out, opts$csv_out)
  cat(sprintf("Wrote daily results to %s\n", opts$csv_out))
} else {
  # Print concise summary to stdout
  cat(sprintf("Simulated %d days at %d matches/day (seed=%d)\n", cfg$days, cfg$matches_per_day, seed))
  last <- daily[nrow(daily), ]
  cat(sprintf("Final trophies: %d, Pass levels: %d, Power: %.1f\n",
              as.integer(last$trophies_end), as.integer(last$pass_levels_total), as.numeric(last$power_total)))
  cat("Day  Wins  WR    Crowns  Gold(match+pass)  Upgrades  PowerGain  Trophies\n")
  show_n <- min(10L, nrow(daily))
  for (i in seq_len(show_n)) {
    r <- daily[i, ]
    cat(sprintf("%3d  %4d  %4.0f%%  %6d    %5d + %-5d   %4d      %7.2f    %6d\n",
                as.integer(r$day), as.integer(r$wins), as.numeric(r$winrate*100), as.integer(r$crowns),
                as.integer(r$gold_from_matches), as.integer(r$gold_from_pass), as.integer(r$upgrades_applied),
                as.numeric(r$power_gained), as.integer(r$trophies_end)))
  }
  if (nrow(daily) > show_n) cat("... (use --csv_out to export full results)\n")
}

