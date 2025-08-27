set.seed(11)

# Generate a tiny sim and confirm plotting helpers return ggplot objects (if ggplot2 is available)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cfg <- default_config(days = 5, matches_per_day = 6)
  cfg$lucky_drop$enabled <- FALSE  # reduce IO/noise for tests
  d <- simulate_season(cfg, seed = 5)$daily
  stopifnot(inherits(plot_trophies(d), "gg"))
  stopifnot(inherits(plot_gold_flow(d), "gg"))
  stopifnot(inherits(plot_winrate(d), "gg"))
  stopifnot(inherits(plot_box_rarity_mix(d), "gg") || is.null(plot_box_rarity_mix(d)))
  stopifnot(inherits(plot_upgrade_efficiency_daily(d), "gg"))
  stopifnot(inherits(plot_upgrade_efficiency_cum(d), "gg"))
  stopifnot(inherits(plot_pass_progress(d), "gg"))
  stopifnot(inherits(plot_trophies_drift(d), "gg"))
  stopifnot(inherits(plot_spend_by_rarity_range(d), "gg") || is.null(plot_spend_by_rarity_range(d)))
  stopifnot(inherits(plot_power_by_rarity_range(d), "gg") || is.null(plot_power_by_rarity_range(d)))
}
