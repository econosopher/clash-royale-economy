#!/usr/bin/env Rscript

suppressWarnings(suppressMessages({
  library(jsonlite)
  library(ggplot2)
  library(dplyr)
}))

source("R/sim_config.R")
source("R/sim_economy.R")
source("R/plots.R")

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default=NULL){
  if (flag %in% args) {
    i <- match(flag, args)
    if (!is.na(i) && i < length(args)) return(args[[i+1]])
  }
  default
}

cfg_file <- arg("--config", "runs/last_config.json")
seed <- as.integer(arg("--seed", "42"))
out_dir <- arg("--out", file.path("runs","screenshots"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

load_cfg <- function(path){
  if (!is.null(path) && file.exists(path)) {
    x <- jsonlite::read_json(path, simplifyVector = FALSE)
    list(cfg = x$cfg %||% default_config(days=360, matches_per_day=15), seed = as.integer(x$seed %||% seed))
  } else list(cfg = default_config(days=360, matches_per_day=15), seed = seed)
}

`%||%` <- function(a,b) if (is.null(a)) b else a

info <- load_cfg(cfg_file)
cfg <- info$cfg
seed <- info$seed

res <- tryCatch(simulate_season(cfg, seed = seed), error = function(e) NULL)
if (is.null(res)) {
  # try fallback json daily
  cand <- c('runs/522a576248c3/1/daily.json','runs/1620f947eb91/1/daily.json')
  p <- cand[file.exists(cand)][1]
  if (!is.na(p) && nzchar(p)) {
    daily <- jsonlite::fromJSON(p, simplifyVector = TRUE)
    res <- list(daily = tibble::as_tibble(daily))
  } else {
    stop("No simulation results or fallback daily available.")
  }
}

df <- res$daily

save_plot <- function(plot_obj, filename, width=10, height=6, dpi=144){
  if (is.null(plot_obj)) return(invisible(NULL))
  ggplot2::ggsave(filename = file.path(out_dir, filename), plot = plot_obj, width = width, height = height, dpi = dpi)
}

save_plot(plot_trophies(df), 'trophies.png')
save_plot(plot_gold_flow(df), 'gold_flow.png')
save_plot(plot_winrate(df), 'winrate.png')
save_plot(plot_box_rarity_mix(df), 'box_rarity_mix.png')
save_plot(plot_spend_by_rarity_range(df), 'spend_by_rarity.png')
save_plot(plot_power_by_rarity_range(df), 'power_by_rarity.png')
save_plot(plot_upgrade_efficiency_daily(df), 'upgrade_efficiency_daily.png')
save_plot(plot_upgrade_efficiency_cum(df), 'upgrade_efficiency_cum.png')
save_plot(plot_pass_progress(df), 'pass_progress.png')
save_plot(plot_trophies_drift(df), 'trophies_drift.png')

cat(sprintf("Saved PNG charts to %s\n", normalizePath(out_dir, mustWork = FALSE)))

