#' Null-coalescing operator
#' @param a The value to check for null.
#' @param b The value to return if a is null.
#' @return b if a is null, otherwise a.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Helper to normalise rarity keyed vectors/lists into named numeric vectors
rarity_vector <- function(x, keys = c("common", "rare", "epic", "legendary")) {
  base <- setNames(numeric(length(keys)), keys)
  if (is.null(x)) return(base)
  vec <- x
  if (is.list(vec) && length(vec) == 1 && (is.numeric(vec[[1]]) || is.integer(vec[[1]]))) {
    vec <- vec[[1]]
  }
  if (is.null(vec)) return(base)
  if (is.data.frame(vec)) {
    # Convert first row if data frame is provided
    vec <- vec[1, , drop = TRUE]
  }
  nm <- tolower(names(vec))
  if (length(nm) != length(vec)) return(base)
  for (i in seq_along(vec)) {
    key <- nm[[i]]
    if (key %in% keys) {
      base[[key]] <- base[[key]] + as.numeric(vec[[i]])
    }
  }
  base
}

rarity_sum <- function(entries, keys = c("common", "rare", "epic", "legendary")) {
  total <- setNames(numeric(length(keys)), keys)
  if (length(entries) == 0) return(total)
  for (item in entries) {
    total <- total + rarity_vector(item, keys)
  }
  total
}

rarity_rows <- function(entries, keys = c("common", "rare", "epic", "legendary")) {
  if (length(entries) == 0) return(tibble::tibble())
  mats <- lapply(entries, function(item) {
    vals <- rarity_vector(item, keys)
    tibble::as_tibble_row(as.list(vals))
  })
  dplyr::bind_rows(mats)
}
