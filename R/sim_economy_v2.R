#' @title Enhanced Economy Simulator
#' @description Main simulation engine using modular architecture
#' @import dplyr
#' @import tibble
#' @import purrr

# Load all modules
source("R/constants.R")
source("R/classes/GameState.R")
source("R/modules/match_simulator.R")
source("R/modules/progression_system.R")
source("R/modules/card_management.R")
source("R/config_validator.R")
source("R/error_handler.R")
source("R/cache_manager.R")
source("R/vectorized_sim.R")

#' @export
#' @description Run a complete season simulation
#' @param cfg Validated configuration object
#' @param seed Random seed for reproducibility
#' @param use_cache Whether to use caching
#' @param progress_callback Optional callback for progress updates
#' @return Simulation results with daily metrics
simulate_season <- function(cfg, seed = 42, use_cache = TRUE, progress_callback = NULL) {
  # Validate configuration
  cfg <- safe_execute(
    validate_config(sanitize_config(cfg)),
    error_type = "configuration",
    context = "Configuration validation"
  )
  
  # Check cache if enabled
  if (use_cache) {
    cache_mgr <- get_cache_manager()
    key <- cache_mgr$generate_key(cfg, seed)
    cached <- cache_mgr$get(key)
    if (!is.null(cached)) {
      if (!is.null(progress_callback)) {
        progress_callback(list(type = "cached", progress = 100))
      }
      return(cached)
    }
  }
  
  set.seed(seed)
  
  # Initialize game state using R6 class
  state <- GameState$new(
    initial_trophies = 0L,
    initial_gold = 0L
  )
  
  # Pre-generate match schedule for vectorization
  match_schedule <- generate_match_schedule_vectorized(
    cfg$days,
    cfg$matches_per_day,
    cfg$jitter_matches
  )
  
  daily_results <- vector("list", cfg$days)
  
  for (day in seq_len(cfg$days)) {
    # Progress callback
    if (!is.null(progress_callback) && day %% max(1, cfg$days %/% 20) == 0) {
      progress_callback(list(
        type = "progress",
        day = day,
        total_days = cfg$days,
        progress = round(100 * day / cfg$days)
      ))
    }
    
    # Reset daily counters
    state$reset_daily()
    
    # Simulate all matches for the day (vectorized)
    n_matches <- match_schedule[day]
    if (n_matches > 0) {
      day_result <- safe_execute(
        simulate_day_vectorized(state, cfg, n_matches),
        error_type = "simulation",
        context = sprintf("Day %d simulation", day)
      )
      
      state <- day_result$state
      daily_results[[day]] <- day_result$summary
    } else {
      daily_results[[day]] <- create_empty_day_summary(day, state)
    }
    
    # Validate state consistency
    validate_state(state)
  }
  
  # Compile results
  result <- list(
    daily = bind_rows(daily_results),
    final_state = state$as_list(),
    config = cfg,
    seed = seed,
    metadata = list(
      version = packageVersion("ClashRoyaleEconomy"),
      timestamp = Sys.time()
    )
  )
  
  # Cache result if enabled
  if (use_cache) {
    cache_mgr$set(key, result)
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(list(type = "complete", progress = 100))
  }
  
  return(result)
}

#' @export
#' @description Simulate a single day with vectorized operations
#' @param state GameState object
#' @param cfg Configuration object
#' @param n_matches Number of matches to simulate
#' @return List with updated state and day summary
simulate_day_vectorized <- function(state, cfg, n_matches) {
  # Calculate current win probability
  power_bonus <- power_bonus_from_score(cfg, state$power_score)
  cfg$vertical_winrate_bonus <- power_bonus
  intrinsic <- effective_winrate(cfg)
  win_prob <- mm_adjust_winrate(cfg, intrinsic, state$trophies)
  
  # Simulate all matches at once
  matches <- simulate_matches_vectorized(
    n_matches,
    win_prob,
    cfg$crown_dist$on_win,
    cfg$crown_dist$on_loss
  )
  
  # Aggregate match results
  wins <- sum(matches$win)
  losses <- n_matches - wins
  total_crowns <- sum(matches$crowns)
  
  # Update trophies
  trophy_change <- wins * cfg$trophies_on_win + losses * cfg$trophies_on_loss
  state$add_trophies(trophy_change)
  
  # Process gold from wins
  gold_from_matches <- 0L
  if (wins > 0) {
    if (!is.na(cfg$gold_drop$fixed_amount)) {
      gold_from_matches <- wins * cfg$gold_drop$fixed_amount
    } else {
      gold_from_matches <- sum(sample(
        seq(cfg$gold_drop$min_amount, cfg$gold_drop$max_amount),
        wins,
        replace = TRUE
      ))
    }
    state$add_gold(gold_from_matches)
  }
  
  # Process pass rewards
  pass_levels_gained <- state$add_crowns(total_crowns, cfg$pass_config$crowns_per_level)
  gold_from_pass <- pass_levels_gained * cfg$pass_config$gold_per_level
  if (gold_from_pass > 0) {
    state$add_gold(gold_from_pass)
  }
  
  # Process mystery boxes (vectorized)
  boxes_earned <- min(wins, cfg$caps$daily_mystery_boxes)
  box_drops <- list()
  
  if (boxes_earned > 0) {
    drops <- roll_mystery_boxes_vectorized(boxes_earned, state$trophies)
    
    for (i in seq_len(nrow(drops))) {
      state$add_card(
        drops$card[i],
        drops$rarity[i],
        drops$quantity[i]
      )
    }
    
    # Aggregate drops by rarity
    box_drops <- drops %>%
      group_by(rarity) %>%
      summarise(total = sum(quantity), .groups = "drop") %>%
      deframe() %>%
      as.list()
  }
  
  # Apply upgrades (vectorized where possible)
  upgrade_result <- greedy_apply_upgrades(state, cfg)
  state <- upgrade_result$state
  
  # Check for level-ups
  if (state$add_account_points(0)) {  # Check without adding
    # Award level-up chest
    chest_drop <- roll_levelup_chest()
    state$add_card(chest_drop$card, chest_drop$rarity, chest_drop$quantity)
  }
  
  # Create day summary
  summary <- tibble(
    day = as.integer(state$current_day),
    matches = n_matches,
    wins = wins,
    losses = losses,
    winrate = wins / max(1, n_matches),
    crowns = total_crowns,
    mystery_boxes = boxes_earned,
    gold_from_matches = gold_from_matches,
    gold_from_pass = gold_from_pass,
    gold_total = gold_from_matches + gold_from_pass,
    gold_spent_upgrades = upgrade_result$gold_spent,
    upgrades_applied = upgrade_result$upgrades,
    power_bonus = power_bonus,
    power_gained = upgrade_result$power_gained,
    power_total = state$power_score,
    pass_level = state$pass_level,
    trophies_start = state$trophies - trophy_change,
    trophies_end = state$trophies,
    arena_end = state$get_current_arena(cfg$arenas)$name,
    box_drops_by_rarity = list(box_drops),
    account_level = state$account_level,
    tower_level = state$tower_level
  )
  
  list(state = state, summary = summary)
}

#' @export
#' @description Create empty day summary when no matches played
#' @param day Day number
#' @param state Current game state
#' @return Day summary tibble
create_empty_day_summary <- function(day, state) {
  tibble(
    day = as.integer(day),
    matches = 0L,
    wins = 0L,
    losses = 0L,
    winrate = 0.0,
    crowns = 0L,
    mystery_boxes = 0L,
    gold_from_matches = 0L,
    gold_from_pass = 0L,
    gold_total = 0L,
    gold_spent_upgrades = 0L,
    upgrades_applied = 0L,
    power_bonus = 0.0,
    power_gained = 0.0,
    power_total = state$power_score,
    pass_level = state$pass_level,
    trophies_start = state$trophies,
    trophies_end = state$trophies,
    arena_end = state$get_current_arena(default_arena_schedule())$name,
    box_drops_by_rarity = list(list()),
    account_level = state$account_level,
    tower_level = state$tower_level
  )
}