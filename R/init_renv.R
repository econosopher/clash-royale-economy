#!/usr/bin/env Rscript

# Initialize renv for this project and record current packages.

has_pkg <- function(p) p %in% rownames(installed.packages())

if (!has_pkg("renv")) {
  message("Installing renv (first-time setup)...")
  install.packages("renv", repos = getOption("repos", "https://cloud.r-project.org"))
}

suppressPackageStartupMessages(library(renv))

if (!file.exists("renv.lock")) {
  message("Initializing renv...")
  renv::init(bare = TRUE)
} else {
  message("renv already initialized; skipping init.")
}

message("Snapshotting dependencies...")
renv::snapshot(prompt = FALSE)

message("Done. Use renv::restore() on other machines to reproduce the library.")

