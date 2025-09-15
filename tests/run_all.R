#!/usr/bin/env Rscript

cat("== Clash Economy: Full Test + Snapshot Runner ==\n")

suppressWarnings(suppressMessages({
  library(jsonlite)
  library(tools)
}))

root <- normalizePath(".")
ok <- TRUE

run <- function(cmd){
  cat("\n$", cmd, "\n", sep=" ")
  st <- system(cmd)
  if (st != 0) ok <<- FALSE
  st
}

run_soft <- function(cmd){
  cat("\n$", cmd, "\n", sep=" ")
  st <- system(cmd)
  if (st != 0) cat("[warn] Command failed (non-fatal):", cmd, "\n")
  st
}

exists_file <- function(p){
  if (!file.exists(p)) { cat("Missing:", p, "\n"); ok <<- FALSE; FALSE } else TRUE
}

# 1) Unit tests (base-R tests)
run("Rscript tests/run_tests.R")

# 2) Warm cache with a deterministic sim
suppressWarnings(suppressMessages({
  source("R/sim_config.R"); source("R/sim_economy.R")
}))
cfg <- default_config(days = 60, matches_per_day = 15)
seed <- 42L
res <- tryCatch(simulate_season(cfg, seed = seed), error = function(e) NULL)
if (!is.null(res)) {
  dir.create(file.path("runs","cache"), recursive = TRUE, showWarnings = FALSE)
  write_json(list(cfg = cfg, seed = seed), file.path("runs","last_config.json"), auto_unbox = TRUE, pretty = TRUE)
}

# 3) (Removed) Chart snapshot export — use the app directly or headless walkthrough

# 4) Headless walkthrough (multi-tab screenshots) if chromote is available
have_chromote <- requireNamespace("chromote", quietly = TRUE) && requireNamespace("processx", quietly = TRUE)
if (have_chromote) {
  run_soft("Rscript R/snap_walkthrough.R")
  exists_file(file.path("runs","screenshots","01_daily_table.png"))
  exists_file(file.path("runs","screenshots","02_visualizations.png"))
  exists_file(file.path("runs","screenshots","03_compare.png"))
  exists_file(file.path("runs","screenshots","04_data_editor.png"))
  exists_file(file.path("runs","screenshots","05_assumptions.png"))
  
  # New: headless E2E assertions to catch UI loading issues
  run_soft("Rscript tests/e2e_headless.R")
} else {
  cat("\n[skip] chromote/processx not installed; skipping headless walkthrough.\n")
}

cat("\n== Summary ==\n")
cat(if (ok) "All checks passed.\n" else "Failures detected — see logs above.\n")
quit(save = "no", status = if (ok) 0 else 1)
