set.seed(1)

# Ensure bundled sample daily loads and is valid
sample_path <- file.path('runs','examples','sample_daily.json')
if (file.exists(sample_path)) {
  d <- jsonlite::fromJSON(sample_path, simplifyVector = TRUE)
  stopifnot(is.data.frame(d))
  df <- tibble::as_tibble(d)
  # minimal required columns
  req_cols <- c('day','matches','wins','losses','winrate','trophies_end')
  stopifnot(all(req_cols %in% names(df)))
  # plotting helpers accept it
  if (requireNamespace('ggplot2', quietly = TRUE)) {
    stopifnot(inherits(plot_trophies(df), 'gg'))
    stopifnot(inherits(plot_gold_flow(df), 'gg'))
    stopifnot(inherits(plot_box_rarity_mix(df), 'gg') || is.null(plot_box_rarity_mix(df)))
  }
}
