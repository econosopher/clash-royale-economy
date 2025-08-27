tables_dir <- function() {
  # Allow app to set an active tables directory via options
  td <- getOption("cr_econ_tables_dir", default = NULL)
  if (!is.null(td)) return(td)
  root <- normalizePath(".", winslash = "/", mustWork = FALSE)
  file.path(root, "assets", "research", "gemini", "tables")
}

load_star_upgrade <- function() {
  p <- file.path(tables_dir(), "table_2_1_lucky_drop_upgrade_probabilities_per_spin.csv")
  if (!file.exists(p)) {
    return(list(`2`=data.frame(target=c(3,4,5), p=c(.24,.20,.06)), `3`=data.frame(target=c(4,5), p=c(.20,.02)), `4`=data.frame(target=5, p=.04)))
  }
  df <- suppressWarnings(readr::read_csv(p, show_col_types = FALSE))
  out <- list()
  for (i in seq_len(nrow(df))) {
    cur <- df$`Current Tier`[i]
    cs <- if (grepl("2", cur)) 2 else if (grepl("3", cur)) 3 else if (grepl("4", cur)) 4 else NA
    if (is.na(cs)) next
    items <- tibble::tibble(
      target = c(3L,4L,5L),
      p = c(df$`Chance to Upgrade to 3-Star`[i], df$`Chance to Upgrade to 4-Star`[i], df$`Chance to Upgrade to 5-Star`[i])
    ) |>
      dplyr::mutate(p = readr::parse_number(as.character(p))/100) |>
      dplyr::filter(!is.na(p) & p > 0)
    out[[as.character(cs)]] <- items
  }
  if (length(out) == 0) return(list(`2`=data.frame(target=c(3,4,5), p=c(.24,.20,.06)), `3`=data.frame(target=c(4,5), p=c(.20,.02)), `4`=data.frame(target=5, p=.04)))
  out
}

load_final_reward <- function(allowed = c("Gold","Random Cards","Banners","Emotes")) {
  p <- file.path(tables_dir(), "table_2_2_lucky_drop_final_reward_probabilities_by_final_rarity_arena_16.csv")
  stars <- 1:5
  if (!file.exists(p)) {
    return(setNames(rep(list(tibble::tibble(label=c("Gold","Random Cards"), p=c(.3,.7))), 5), as.character(stars)))
  }
  df <- suppressWarnings(readr::read_csv(p, show_col_types = FALSE))
  out <- lapply(stars, function(s){
    col <- sprintf("%d-Star Chance", s)
    tibble::tibble(label = df$`Reward Type`, p = readr::parse_number(df[[col]])/100) |>
      dplyr::filter(!is.na(p), label %in% allowed) |>
      dplyr::mutate(p = p/sum(p))
  })
  names(out) <- as.character(stars)
  out
}

lucky_star_roll_star <- function(start, spins, star_up) {
  s <- start
  for (i in seq_len(max(0, spins))) {
    if (s >= 5) break
    opts <- star_up[[as.character(s)]]
    if (is.null(opts) || nrow(opts) == 0) next
    p_up <- sum(opts$p)
    r <- runif(1)
    if (r > p_up) next
    # weighted pick
    idx <- sample(seq_len(nrow(opts)), size = 1, prob = opts$p)
    s <- opts$target[idx]
  }
  max(1, min(5, s))
}

lucky_star_roll_final <- function(star, final_tbl) {
  items <- final_tbl[[as.character(star)]]
  if (is.null(items) || nrow(items) == 0) return("Random Cards")
  sample(items$label, size = 1, prob = items$p)
}

simulate_lucky_star_stars <- function(cfg, trials = 10000) {
  star_up <- load_star_upgrade()
  spins <- cfg$lucky_drop$spins
  res <- list()
  for (w in 1:3) {
    start <- if (w == 1) 2 else 1
    stars <- replicate(trials, lucky_star_roll_star(start, spins, star_up))
    res[[as.character(w)]] <- as.integer(table(factor(stars, levels = 1:5))) / trials * 100
  }
  res
}
