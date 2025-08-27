set.seed(42)

message('stage: build cfg and season')
cfg <- default_config(days = 10, matches_per_day = 12)
cfg$lucky_drop$enabled <- FALSE
res <- simulate_season(cfg, seed = 7)

message('stage: check structure and sizes')
stopifnot(is.list(res), is.data.frame(res$daily))
df <- res$daily
stopifnot(nrow(df) == cfg$days)

message('stage: check winrate invariants')
stopifnot(all.equal(df$winrate, df$wins / df$matches, tolerance = 1e-12) == TRUE)
stopifnot(all(df$winrate >= 0 & df$winrate <= 1))

message('stage: check gold invariants')
stopifnot(all(df$gold_total == df$gold_from_matches + df$gold_from_pass))

message('stage: check crowns cumulative')
stopifnot(tail(df$pass_crowns_total, 1) == sum(df$crowns))

message('stage: trophies finiteness')
stopifnot(all(is.finite(df$trophies_end)))

message('stage: list-columns present')
stopifnot("gold_spent_by_rarity" %in% names(df))
stopifnot("power_gained_by_rarity" %in% names(df))

message('stage: simulate_match sanity')
st <- list(trophies = 0L)
mm <- simulate_match(st, cfg)
stopifnot(is.logical(mm$win), is.numeric(mm$crowns), is.numeric(mm$gold))

message('stage: simulate_day keys present')
st2 <- list(trophies = 0L, pass_crowns = 0L, pass_levels = 0L, gold_bank = 0L,
            inventory_by_rarity = list(), inventory_by_card = list(), cards = list(),
            gold_today = 0L, power_score = 0.0)
dres <- simulate_day(st2, cfg, day_index = 1)
d1 <- dres$summary
must_have <- c("matches","wins","losses","crowns","gold_from_matches","gold_from_pass",
               "trophies_end","arena_end","gold_spent_upgrades","upgrades_applied","power_total")
stopifnot(all(must_have %in% names(d1)))
