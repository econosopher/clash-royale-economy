#!/usr/bin/env Rscript

# Minimal test runner with base R only (no external deps required).
# Usage: Rscript tests/run_tests.R (run from repo root)

# Assume working directory is repo root when invoking via path tests/run_tests.R
root <- normalizePath(".", mustWork = FALSE)
setwd(root)

# Load project code once
source("R/sim_config.R")
source("R/lucky_star.R")
source("R/sim_economy.R")
source("R/plots.R")
source("R/cache_manager.R")
source("R/config_store.R")

green <- function(x) paste0("\033[32m", x, "\033[39m")
red   <- function(x) paste0("\033[31m", x, "\033[39m")
grey  <- function(x) paste0("\033[90m", x, "\033[39m")

test_files <- list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE)
if (length(test_files) == 0) {
  cat(grey("No tests found.\n"))
  quit(save = "no", status = 0)
}

failures <- list()
passes <- 0L

for (f in sort(test_files)) {
  cat(grey(sprintf("Running %s ...\n", basename(f))))
  ok <- tryCatch({
    sys.source(f, envir = new.env(parent = globalenv()))
    TRUE
  }, error = function(e) {
    cat(red(sprintf("  Error: %s\n", conditionMessage(e))))
    failures[[basename(f)]] <<- conditionMessage(e)
    FALSE
  })
  if (ok) {
    cat(green("  Passed\n"))
    passes <- passes + 1L
  }
}

cat("\n")
cat(green(sprintf("Passed: %d\n", passes)))
cat(if (length(failures) == 0) green("Failed: 0\n") else red(sprintf("Failed: %d\n", length(failures))))
if (length(failures) > 0) {
  cat("Failures:\n")
  for (nm in names(failures)) cat(sprintf("- %s: %s\n", nm, failures[[nm]]))
}

quit(save = "no", status = if (length(failures) > 0) 1 else 0)
