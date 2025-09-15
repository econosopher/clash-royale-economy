#!/usr/bin/env Rscript

suppressWarnings(suppressMessages({
  library(jsonlite)
  library(digest)
  library(readr)
}))

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

classify_line <- function(line) {
  if (!nzchar(line)) return(NULL)
  sev <- NULL
  if (grepl("^(Warning: Error|Error:|Error in )", line)) sev <- "error"
  else if (grepl("^Warning", line)) sev <- "warning"
  else if (grepl("Caused by error|rlang::abort|cli::cli_abort", line)) sev <- "error"
  else return(NULL)
  list(severity = sev, message = line)
}

write_event <- function(out_path, event) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  cat(toJSON(event, auto_unbox = TRUE), file = out_path, append = TRUE)
  cat("\n", file = out_path, append = TRUE)
}

watch_log <- function(log_path = file.path('runs','shiny.log'), out_path = file.path('runs','logs','events.jsonl'),
                      dedupe_seconds = 10, duration = Inf, quiet = FALSE) {
  last_emit <- new.env(parent = emptyenv())
  started <- Sys.time()
  pos <- 0L
  if (!file.exists(log_path)) {
    if (!quiet) cat("[log-agent] Waiting for log file:", log_path, "\n")
  }
  repeat {
    if (!file.exists(log_path)) { Sys.sleep(1); if (difftime(Sys.time(), started, units='secs') > duration) break; next }
    lines <- try(read_lines(log_path), silent = TRUE)
    if (inherits(lines, 'try-error')) { Sys.sleep(1); next }
    n <- length(lines)
    if (pos < n) {
      for (i in seq.int(pos + 1L, n)) {
        ln <- lines[[i]]
        cls <- classify_line(ln)
        if (!is.null(cls)) {
          key <- digest(cls$message)
          now <- Sys.time()
          last <- last_emit[[key]]
          if (is.null(last) || difftime(now, last, units='secs') > dedupe_seconds) {
            evt <- list(ts = now_iso(), severity = cls$severity, line = i, message = cls$message)
            write_event(out_path, evt)
            if (!quiet) cat(sprintf("[%s] %s: %s\n", evt$ts, toupper(evt$severity), evt$message))
            last_emit[[key]] <- now
          }
        }
      }
      pos <- n
    }
    if (difftime(Sys.time(), started, units='secs') > duration) break
    Sys.sleep(1)
  }
}

# CLI
args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  if (flag %in% args) {
    i <- match(flag, args)
    if (!is.na(i) && i < length(args)) return(args[[i+1]])
    else return(TRUE)
  }
  default
}

if (sys.nframe() == 0) {
  log <- arg("--log", file.path('runs','shiny.log'))
  out <- arg("--out", file.path('runs','logs','events.jsonl'))
  dur <- as.numeric(arg("--duration", Inf))
  quiet <- isTRUE(arg("--quiet", FALSE))
  dedup <- as.numeric(arg("--dedupe", 10))
  watch_log(log, out, dedupe_seconds = dedup, duration = dur, quiet = quiet)
}
