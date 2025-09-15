source("R/utils.R")

default_config <- function(days = 30, matches_per_day = 15) {
  list(
    days = as.integer(days),
    matches_per_day = as.integer(matches_per_day),
    jitter_matches = 0L,
    base_winrate = 0.50,
    bot_rate = 0.10,
    bot_winrate = 0.90,
    vertical_winrate_bonus = 0.00,
    mm_convergence_per_arena = 0.15,
    trophies_on_win = 30L,
    trophies_on_loss = -29L,
    gold_drop = list(fixed_amount = NA_integer_, min_amount = 10L, max_amount = 10L),
    caps = list(daily_mystery_boxes = 4L),
    pass_config = list(crowns_per_level = 20L, gold_per_level = 50L),
    crown_dist = list(on_win = c(`0`=0.0,`1`=0.20,`2`=0.50,`3`=0.30), on_loss = c(`0`=0.40,`1`=0.40,`2`=0.20,`3`=0.0)),
    lucky_drop = list(enabled = TRUE, wins_with_lucky_drops = 3L, spins = 4L,
                      gold_reward_by_star = c(`1`=0L,`2`=0L,`3`=0L,`4`=0L,`5`=0L)),
    progression = default_progression_rules(),
    power_to_winrate = list(slope = 0.0005, max_bonus = 0.20),
    arenas = default_arena_schedule()
  )
}

effective_winrate <- function(cfg, dynamic_bonus = 0) {
  base <- cfg$base_winrate
  blended <- (1.0 - cfg$bot_rate) * base + cfg$bot_rate * cfg$bot_winrate
  vertical <- cfg$vertical_winrate_bonus %||% 0
  bonus <- dynamic_bonus %||% 0
  pmin(1.0, pmax(0.0, blended + vertical + bonus))
}

# Adjusts the winrate based on the matchmaking system.
# This function models the convergence of winrate towards 50% as a player
# climbs through arenas. The `factor` calculation uses an exponential decay
# to model this convergence.
mm_adjust_winrate <- function(cfg, intrinsic, trophies) {
  target <- (1.0 - cfg$bot_rate) * 0.5 + cfg$bot_rate * cfg$bot_winrate
  arena_index <- max(0L, floor(trophies / 400))
  conv <- cfg$mm_convergence_per_arena %||% 0.15
  factor <- exp(-conv * arena_index)
  pmin(1.0, pmax(0.0, target + (intrinsic - target) * factor))
}

sample_from_pmf <- function(pmf) {
  ks <- as.integer(names(pmf))
  probs <- pmf
  sample(ks, size = 1, prob = probs)
}

# Progression rules (simplified, aligned with Python defaults)
rarity_rank <- function(r) {
  ranks <- c(common=0L, rare=1L, epic=2L, legendary=3L)
  ranks[[tolower(r)]] %||% 99L
}

default_progression_rules <- function() {
  list(
    by_rarity = list(
      common = list(
        copies_per_level = c(2,4,10,20,50,100,200,400,800,1000),
        gold_cost_per_level = c(20,50,150,400,1000,2000,4000,8000,20000,50000),
        power_per_level_step = 1.0
      ),
      rare = list(
        copies_per_level = c(2,4,10,20,50,100,200,400),
        gold_cost_per_level = c(50,150,400,1000,2000,5000,10000,20000),
        power_per_level_step = 2.5
      ),
      epic = list(
        copies_per_level = c(1,2,4,10,20,50),
        gold_cost_per_level = c(400,1000,2000,5000,10000,20000),
        power_per_level_step = 6.0
      ),
      legendary = list(
        copies_per_level = c(1,1,2,4,10),
        gold_cost_per_level = c(2000,5000,10000,20000,50000),
        power_per_level_step = 15.0
      )
    )
  )
}

default_arena_schedule <- function() {
  tibble::tibble(
    name = paste("Arena", 1:12),
    min_trophies = (0:11) * 400
  )
}

current_arena <- function(arenas, trophies) {
  arenas %>% dplyr::filter(min_trophies <= trophies) %>% dplyr::arrange(dplyr::desc(min_trophies)) %>% dplyr::slice(1)
}

power_bonus_from_score <- function(cfg, power_score) {
  slope <- cfg$power_to_winrate$slope; cap <- cfg$power_to_winrate$max_bonus
  pmin(cap, pmax(0.0, slope * power_score))
}

rarity_weights_for_trophies <- function(trophies) {
  # simple schedule: bias towards higher rarities at higher arenas
  idx <- floor(trophies / 400)
  if (idx >= 10) return(c(common=.60, rare=.30, epic=.075, legendary=.025))
  if (idx >= 8) return(c(common=.62, rare=.30, epic=.065, legendary=.015))
  if (idx >= 5) return(c(common=.65, rare=.28, epic=.055, legendary=.015))
  c(common=.70, rare=.25, epic=.04, legendary=.01)
}
