library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

plot_trophies <- function(df) {
  # arena bands by contiguous spans
  spans <- df %>% mutate(arena = arena_end) %>%
    mutate(group = cumsum(c(1, diff(as.integer(factor(arena))) != 0))) %>%
    group_by(group, arena) %>% summarise(xmin = min(day)-0.5, xmax = max(day)+0.5, .groups = 'drop')
  ggplot(df, aes(day, trophies_end)) +
    {if(nrow(spans)>0) geom_rect(data=spans, inherit.aes = FALSE, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), fill="#e2e8f0", alpha=0.18)} +
    geom_line(color = "#2563eb") +
    geom_hline(yintercept = seq(0, max(df$trophies_end, na.rm = TRUE) + 400, by = 400),
               linetype = "dotted", color = "#94a3b8", size = 0.2) +
    labs(title = "Trophies by Day", x = "Day", y = "Trophies") +
    theme_minimal(base_size = 12)
}

plot_gold_flow <- function(df) {
  dd <- df %>% select(day, matches=gold_from_matches, pass=gold_from_pass, spend=gold_spent_upgrades) %>%
    mutate(income = matches + pass,
           bank = cumsum(income - spend)) %>%
    pivot_longer(cols = c(matches, pass), names_to = 'src', values_to = 'gold')
  ggplot(dd, aes(day, gold, fill = src)) +
    geom_area(alpha = 0.7, position = 'stack') +
    geom_line(aes(y = bank, color = 'bank'), size = 0.8, inherit.aes = FALSE) +
    scale_fill_manual(values = c(matches = "#2563eb", pass = "#7c3aed"), name = "Gold") +
    scale_color_manual(values = c(bank = "#111827"), name = " ") +
    labs(title = "Gold Earned per Day + Bank", x = "Day", y = "Gold") +
    theme_minimal(base_size = 12)
}

plot_winrate <- function(df) {
  ggplot(df, aes(day, winrate*100)) +
    geom_line(color = "#10b981") +
    geom_hline(yintercept = 50, linetype = "dotted", color = "#94a3b8") +
    labs(title = "Winrate Over Time", x = "Day", y = "Winrate (%)") +
    theme_minimal(base_size = 12)
}

plot_box_rarity_mix <- function(df) {
  if (!"box_drops_by_rarity" %in% names(df)) return(NULL)
  # Expand list-column
  expand <- function(x) {
    # x may be a named numeric vector or a list containing that vector
    b <- if (is.list(x) && length(x) == 1) x[[1]] else x
    cm <- as.numeric(b["common"]); if (is.na(cm)) cm <- 0
    rr <- as.numeric(b["rare"]); if (is.na(rr)) rr <- 0
    ep <- as.numeric(b["epic"]); if (is.na(ep)) ep <- 0
    lg <- as.numeric(b["legendary"]); if (is.na(lg)) lg <- 0
    tibble::tibble(common = cm, rare = rr, epic = ep, legendary = lg)
  }
  rr <- bind_rows(lapply(df$box_drops_by_rarity, expand)) |>
    mutate(day = df$day, total = pmax(1, common+rare+epic+legendary)) |>
    mutate(across(c(common,rare,epic,legendary), ~ .x/total*100)) |>
    select(day, common, rare, epic, legendary) |>
    pivot_longer(-day, names_to = "rarity", values_to = "share")
  ggplot(rr, aes(day, share, fill = rarity)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = c(common="#94a3b8", rare="#2563eb", epic="#7c3aed", legendary="#f59e0b")) +
    labs(title = "Box Rarity Mix (% per day)", x = "Day", y = "%") +
    theme_minimal(base_size = 12)
}

# Research plots
upgrade_tables <- function() {
  base <- getOption("cr_econ_tables_base", default = normalizePath(file.path("assets","research","gemini"), mustWork = FALSE))
  list(
    Common = file.path(base, "tables", "card_upgrade_costs_common.csv"),
    Rare = file.path(base, "tables", "card_upgrade_costs_rare.csv"),
    Epic = file.path(base, "tables", "card_upgrade_costs_epic.csv"),
    Legendary = file.path(base, "tables", "card_upgrade_costs_legendary.csv"),
    Champion = file.path(base, "tables", "card_upgrade_costs_champion.csv")
  )
}

.to_int <- function(x) as.integer(readr::parse_number(as.character(x)))
.to_level <- function(s) {
  s <- as.character(s); if (grepl("->", s)) as.integer(sub(".*->", "", s)) else if (grepl("-\\>", s)) as.integer(sub(".*-\\>", "", s)) else NA_integer_
}

plot_research_upgrades_cum <- function() {
  tabs <- upgrade_tables()
  all <- purrr::imap_dfr(tabs, function(path, nm){
    if (!file.exists(path)) return(NULL)
    df <- suppressWarnings(readr::read_csv(path, show_col_types = FALSE))
    df %>% mutate(gold_cost = .to_int(`Gold Cost`), to_level = .to_level(Level), rarity = nm) %>%
      mutate(cum_gold_derived = cumsum(replace_na(gold_cost, 0)))
  })
  ggplot(all, aes(to_level, cum_gold_derived, color = rarity)) +
    geom_line() + geom_point(size = 1) +
    scale_color_manual(values = c(Common="#94a3b8", Rare="#2563eb", Epic="#7c3aed", Legendary="#f59e0b", Champion="#10b981")) +
    labs(title = "Card Upgrade Cumulative Gold (Derived)", x = "Target Level", y = "Cumulative Gold", color = "Rarity") +
    theme_minimal(base_size = 12)
}

plot_research_lucky_final <- function() {
  p <- file.path("assets","research","gemini","tables","table_2_2_lucky_drop_final_reward_probabilities_by_final_rarity_arena_16.csv")
  if (!file.exists(p)) return(NULL)
  df <- suppressWarnings(readr::read_csv(p, show_col_types = FALSE)) |>
    pivot_longer(cols = ends_with("Chance"), names_to = "star", values_to = "pct") |>
    mutate(pct = readr::parse_number(pct))
  ggplot(df, aes(star, pct, fill = `Reward Type`)) +
    geom_col(position = "stack") +
    labs(title = "Lucky Star Final Reward Distribution by Star Tier (Arena 16+)", x = "Final Rarity (Star Tier)", y = "%") +
    theme_minimal(base_size = 12)
}

# Effective post-filter reward shares (sim-used)
plot_research_lucky_final_effective <- function() {
  p <- file.path("assets","research","gemini","tables","table_2_2_lucky_drop_final_reward_probabilities_by_final_rarity_arena_16.csv")
  if (!file.exists(p)) return(NULL)
  allowed <- c("Gold","Random Cards","Banners","Emotes")
  df <- suppressWarnings(readr::read_csv(p, show_col_types = FALSE)) |>
    pivot_longer(cols = ends_with("Chance"), names_to = "star", values_to = "pct") |>
    mutate(pct = readr::parse_number(pct)) |>
    filter(`Reward Type` %in% allowed) |>
    group_by(star) |>
    mutate(pct = pct/sum(pct)) |>
    ungroup()
  ggplot(df, aes(star, pct*100, fill = `Reward Type`)) +
    geom_col(position = "stack") +
    labs(title = "Lucky Star Final Reward Distribution (Effective, Post-filter)", x = "Final Rarity (Star Tier)", y = "%") +
    theme_minimal(base_size = 12)
}

plot_research_lucky_star <- function() {
  cfg <- default_config()
  stars <- simulate_lucky_star_stars(cfg, trials = 10000)
  df <- bind_rows(lapply(names(stars), function(w){
    tibble::tibble(win = paste0("Win ", w), star = paste0(1:5, "-Star"), pct = stars[[w]])
  }))
  ggplot(df, aes(star, pct, fill = win)) +
    geom_col(position = "dodge") +
    labs(title = "Lucky Star: Final Star Distribution by Daily Win (Simulated)", x = "Final Star Tier", y = "%") +
    theme_minimal(base_size = 12)
}

plot_pass_progress <- function(df) {
  max_c <- max(df$pass_crowns_total, na.rm = TRUE)
  ticks <- tibble::tibble(y = seq(0, max(0, max_c + 20), by = 20))
  ggplot(df, aes(day, pass_crowns_total)) +
    geom_line(color = "#0ea5e9") +
    geom_hline(data = ticks, aes(yintercept = y), linetype = "dotted", color = "#e5e7eb") +
    labs(title = "Pass Progress (Crowns)", x = "Day", y = "Crowns (cumulative)") +
    theme_minimal(base_size = 12)
}

plot_trophies_drift <- function(df) {
  dd <- df %>% mutate(delta = c(0, diff(trophies_end)))
  ggplot(dd, aes(day, delta)) +
    geom_col(fill = "#f59e0b") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "#94a3b8") +
    labs(title = "Trophies Drift (per day)", x = "Day", y = "Δ Trophies") +
    theme_minimal(base_size = 12)
}

plot_deck_levels <- function(df) {
  if (!all(c('avg_level_common','avg_level_rare','avg_level_epic','avg_level_legendary') %in% names(df))) return(NULL)
  dd <- df %>% select(day, common = avg_level_common, rare = avg_level_rare, epic = avg_level_epic, legendary = avg_level_legendary, overall = avg_level)
  long <- dd %>% pivot_longer(cols = c(common, rare, epic, legendary), names_to = 'rarity', values_to = 'avg_level_r')
  ggplot() +
    geom_line(data = long, aes(day, avg_level_r, color = rarity), size = 0.9) +
    geom_line(data = dd, aes(day, overall, color = 'overall'), size = 1.1, linetype = 'dashed') +
    scale_color_manual(values = c(common="#94a3b8", rare="#2563eb", epic="#7c3aed", legendary="#f59e0b", overall="#111827"), name = "Rarity") +
    labs(title = "Average Card Level by Rarity (with Overall)", x = "Day", y = "Avg Level") +
    theme_minimal(base_size = 12)
}

plot_upgrade_efficiency_daily <- function(df) {
  dd <- df %>% mutate(eff = ifelse(gold_spent_upgrades > 0, power_gained / (gold_spent_upgrades/1000), NA_real_))
  ggplot(dd, aes(day, eff)) +
    geom_line(color = "#16a34a") +
    geom_point(color = "#16a34a", size = 1.8, na.rm = TRUE) +
    labs(title = "Upgrade Efficiency (Daily Power per 1k Gold)", x = "Day", y = "Power / 1k Gold") +
    theme_minimal(base_size = 12)
}

plot_upgrade_efficiency_cum <- function(df) {
  dd <- df %>% mutate(cum_power = cumsum(power_gained), cum_gold = cumsum(gold_spent_upgrades), eff = ifelse(cum_gold > 0, cum_power / (cum_gold/1000), NA_real_))
  ggplot(dd, aes(day, eff)) +
    geom_line(color = "#065f46") +
    labs(title = "Upgrade Efficiency (Cumulative Power per 1k Gold)", x = "Day", y = "Power / 1k Gold") +
    theme_minimal(base_size = 12)
}

plot_spend_by_rarity_range <- function(df) {
  if (!"gold_spent_by_rarity" %in% names(df)) return(NULL)
  # Build a tidy frame with guaranteed columns for each rarity
  mk_row <- function(x) {
    v <- if (is.list(x) && length(x) == 1) x[[1]] else x
    cm <- as.numeric(v["common"]); if (is.na(cm)) cm <- 0
    rr <- as.numeric(v["rare"]); if (is.na(rr)) rr <- 0
    ep <- as.numeric(v["epic"]); if (is.na(ep)) ep <- 0
    lg <- as.numeric(v["legendary"]); if (is.na(lg)) lg <- 0
    tibble::tibble(common = cm, rare = rr, epic = ep, legendary = lg)
  }
  gr <- dplyr::bind_rows(lapply(df$gold_spent_by_rarity, mk_row))
  if (nrow(gr) == 0) return(NULL)
  tot <- colSums(gr, na.rm = TRUE)
  rarities <- c('common','rare','epic','legendary')
  tt <- tibble::tibble(rarity = factor(stringr::str_to_title(rarities), levels = stringr::str_to_title(rarities)),
                       gold_spent = as.numeric(tot[rarities]))
  ggplot(tt, aes(rarity, gold_spent, fill = rarity)) +
    geom_col() +
    scale_fill_manual(values = c(Common="#94a3b8", Rare="#2563eb", Epic="#7c3aed", Legendary="#f59e0b"), guide = 'none') +
    labs(title = "Gold Spent by Rarity (Selected Range)", x = "Rarity", y = "Gold") +
    theme_minimal(base_size = 12)
}

plot_power_by_rarity_range <- function(df) {
  if (!"power_gained_by_rarity" %in% names(df)) return(NULL)
  mk_row <- function(x) {
    v <- if (is.list(x) && length(x) == 1) x[[1]] else x
    cm <- as.numeric(v["common"]); if (is.na(cm)) cm <- 0
    rr <- as.numeric(v["rare"]); if (is.na(rr)) rr <- 0
    ep <- as.numeric(v["epic"]); if (is.na(ep)) ep <- 0
    lg <- as.numeric(v["legendary"]); if (is.na(lg)) lg <- 0
    tibble::tibble(common = cm, rare = rr, epic = ep, legendary = lg)
  }
  pr <- dplyr::bind_rows(lapply(df$power_gained_by_rarity, mk_row))
  if (nrow(pr) == 0) return(NULL)
  tot <- colSums(pr, na.rm = TRUE)
  rarities <- c('common','rare','epic','legendary')
  tt <- tibble::tibble(rarity = factor(stringr::str_to_title(rarities), levels = stringr::str_to_title(rarities)),
                       power_gained = as.numeric(tot[rarities]))
  ggplot(tt, aes(rarity, power_gained, fill = rarity)) +
    geom_col() +
    scale_fill_manual(values = c(Common="#94a3b8", Rare="#2563eb", Epic="#7c3aed", Legendary="#f59e0b"), guide = 'none') +
    labs(title = "Power Gained by Rarity (Selected Range)", x = "Rarity", y = "Power") +
    theme_minimal(base_size = 12)
}
