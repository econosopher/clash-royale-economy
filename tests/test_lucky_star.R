set.seed(99)

# star upgrade table loads (fallback to defaults if CSV missing)
up <- load_star_upgrade()
stopifnot(is.list(up), all(c("2","3","4") %in% names(up)))

# final reward table loads (fallback if CSV missing)
fr <- load_final_reward()
stopifnot(is.list(fr), all(as.character(1:5) %in% names(fr)))

# roll star a bunch of times and ensure bounds [1..5]
stars <- replicate(100, lucky_star_roll_star(start = 2L, spins = 4L, star_up = up))
stopifnot(all(stars >= 1 & stars <= 5))

# final label is among allowed when table present; fallback ensures non-empty string
labs <- c("Gold","Random Cards","Banners","Emotes")
lab <- lucky_star_roll_final(star = 3L, final_tbl = fr)
stopifnot(is.character(lab), nchar(lab) > 0)
if (all(vapply(fr, nrow, integer(1)) > 0)) {
  stopifnot(lab %in% labs)
}

