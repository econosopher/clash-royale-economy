#' Null-coalescing operator
#' @param a The value to check for null.
#' @param b The value to return if a is null.
#' @return b if a is null, otherwise a.
`%||%` <- function(a, b) if (is.null(a)) b else a
