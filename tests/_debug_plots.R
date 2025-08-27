source('R/sim_config.R'); source('R/lucky_star.R'); source('R/sim_economy.R'); source('R/plots.R')
cfg <- default_config(days = 5, matches_per_day = 6)
d <- simulate_season(cfg, seed = 5)$daily
fns <- list(
  plot_trophies=plot_trophies,
  plot_gold_flow=plot_gold_flow,
  plot_winrate=plot_winrate,
  plot_box_rarity_mix=plot_box_rarity_mix,
  plot_upgrade_efficiency_daily=plot_upgrade_efficiency_daily,
  plot_upgrade_efficiency_cum=plot_upgrade_efficiency_cum,
  plot_pass_progress=plot_pass_progress,
  plot_trophies_drift=plot_trophies_drift,
  plot_spend_by_rarity_range=plot_spend_by_rarity_range,
  plot_power_by_rarity_range=plot_power_by_rarity_range
)
for (nm in names(fns)) {
  cat('Calling', nm, '...\n')
  res <- try(fns[[nm]](d), silent = TRUE)
  if (inherits(res, 'try-error')) {
    cat('  ERROR:', attr(res, 'condition')$message, '\n')
  } else if (!is.null(res)) {
    cat('  OK class=', paste(class(res), collapse=','), '\n')
  } else {
    cat('  NULL returned\n')
  }
}
