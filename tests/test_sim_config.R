set.seed(123)

# default_config basics
cfg <- default_config(days = 30, matches_per_day = 15)
stopifnot(is.list(cfg))
stopifnot(cfg$days == 30L, cfg$matches_per_day == 15L)
stopifnot(cfg$base_winrate >= 0 && cfg$base_winrate <= 1)

# effective winrate bounded in [0,1]
ew <- effective_winrate(cfg)
stopifnot(is.numeric(ew), ew >= 0, ew <= 1)

# mm_adjust_winrate pulls toward target more strongly at high trophies
intrinsic <- 0.8
w0 <- mm_adjust_winrate(cfg, intrinsic, trophies = 0)
wH <- mm_adjust_winrate(cfg, intrinsic, trophies = 4000)
target <- (1 - cfg$bot_rate) * 0.5 + cfg$bot_rate * cfg$bot_winrate
stopifnot(abs(wH - target) < abs(w0 - target))
stopifnot(w0 >= 0 && w0 <= 1 && wH >= 0 && wH <= 1)

# sampling helper returns integer crowns from given pmf keys
cc <- sample_from_pmf(cfg$crown_dist$on_win)
stopifnot(is.integer(cc) || is.numeric(cc))
stopifnot(cc %in% as.integer(names(cfg$crown_dist$on_win)))

