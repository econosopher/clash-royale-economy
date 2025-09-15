source('R/log_agent.R')

# basic classification of log lines
stopifnot(is.null(classify_line("Listening on http://127.0.0.1:7789")))
stopifnot(identical(classify_line("Warning: Error in geom_line")$severity, 'error'))
stopifnot(identical(classify_line("Warning message: something something")$severity, 'warning'))
stopifnot(identical(classify_line("Error in `compute_geom_1()`: ...")$severity, 'error'))

