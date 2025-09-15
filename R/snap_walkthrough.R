#!/usr/bin/env Rscript

suppressWarnings(suppressMessages({
  library(processx)
  library(chromote)
  library(jsonlite)
}))

url <- "http://127.0.0.1:7789/"
out_dir <- file.path("runs","screenshots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Launch app
cat("Starting app...\n")
p <- processx::process$new("Rscript", c("app.R"), stdout = "|", stderr = "|")
on.exit({ if (p$is_alive()) p$kill() }, add = TRUE)

# Wait for port
ready <- FALSE
for (i in 1:60) {
  con <- try(url(url), silent = TRUE)
  if (!inherits(con, "try-error")) { close(con); ready <- TRUE; break } else Sys.sleep(1)
}
if (!ready) stop("App did not start in time")

session <- ChromoteSession$new()
session$Page$navigate(url)

# Wait until the document is fully loaded (readyState == 'complete')
# Some chromote versions don't have wait_for_load(); poll instead.
for (i in 1:200) { # up to ~20s
  res <- try(session$Runtime$evaluate("document.readyState"), silent = TRUE)
  if (!inherits(res, "try-error")) {
    state <- tryCatch(res$result$value, error = function(e) NULL)
    if (is.character(state) && identical(state, "complete")) break
  }
  Sys.sleep(0.1)
}
Sys.sleep(0.5) # tiny settle

snap <- function(name) {
  f <- file.path(out_dir, paste0(name, ".png"))
  session$screenshot(filename = f, selector = "body")
  cat("Saved", f, "\n")
}

# Daily Table
session$Runtime$evaluate("Shiny.setInputValue('tabs','Daily Table', {priority: 'event'})")
Sys.sleep(1.2)
snap("01_daily_table")

# Visualizations + export charts
session$Runtime$evaluate("Shiny.setInputValue('tabs','Visualizations', {priority: 'event'})")
Sys.sleep(1.2)
session$Runtime$evaluate("document.getElementById('export_pngs')?.click()")
Sys.sleep(0.8)
snap("02_visualizations")

# Compare: select Defaults and run
session$Runtime$evaluate("Shiny.setInputValue('tabs','Compare', {priority: 'event'})")
Sys.sleep(1.2)
session$Runtime$evaluate("Shiny.setInputValue('cmp_select',['Defaults'], {priority: 'event'})")
Sys.sleep(0.5)
session$Runtime$evaluate("document.getElementById('cmp_run')?.click()")
Sys.sleep(1.2)
snap("03_compare")

# Data Editor: just open and snap
session$Runtime$evaluate("Shiny.setInputValue('tabs','Data Editor', {priority: 'event'})")
Sys.sleep(1.2)
snap("04_data_editor")

# Assumptions: open Core Settings accordion and snap
session$Runtime$evaluate("Shiny.setInputValue('tabs','Assumptions Editor', {priority: 'event'})")
Sys.sleep(1.0)
session$Runtime$evaluate("document.querySelector('[data-bs-toggle=\"collapse\"]')?.click()")
Sys.sleep(0.8)
snap("05_assumptions")

cat("Walkthrough screenshots saved to ", normalizePath(out_dir, mustWork = FALSE), "\n", sep = "")
