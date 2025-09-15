#!/usr/bin/env Rscript

# Minimal dependency installer for the Clash Economy project

pkgs <- c(
  # Core packages
  "shiny","bslib","ggplot2","dplyr","tidyr","purrr","readr","DT",
  "stringr","jsonlite","digest","tibble",
  # New dependencies for enhanced features
  "R6",        # OOP support
  "cachem",    # File-based caching
  "httpuv",    # WebSocket support
  "later",     # Async operations
  "rlang",     # Error handling
  "checkmate"  # Input validation
)
optional <- c(
  "chromote",   # Headless browser testing
  "processx",   # Process management
  "roxygen2",   # Documentation generation
  "testthat"    # Unit testing
)

install_if_missing <- function(names) {
  to_install <- setdiff(names, rownames(installed.packages()))
  if (length(to_install)) {
    message("Installing: ", paste(to_install, collapse=", "))
    install.packages(to_install, repos = getOption("repos", "https://cloud.r-project.org"))
  } else {
    message("All core packages already installed.")
  }
}

install_if_missing(pkgs)

if (length(setdiff(optional, rownames(installed.packages()))) > 0) {
  message("(Optional) You can also install: ", paste(optional, collapse=", "))
}

message("Done.")

