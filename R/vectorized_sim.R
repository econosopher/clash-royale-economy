#' @title Vectorized Simulation Functions
#' @description Performance-optimized vectorized simulation functions
#' @import dplyr

source("R/constants.R")

#' @export
#' @param n Number of matches to simulate
#' @param win_prob Win probability
#' @param crown_dist_win Crown distribution on win
#' @param crown_dist_loss Crown distribution on loss
#' @return Data frame with match results
simulate_matches_vectorized <- function(n, win_prob, crown_dist_win, crown_dist_loss) {
  # Vectorized win/loss determination
  wins <- runif(n) < win_prob
  
  # Pre-calculate crown probabilities
  win_crowns <- sample(
    x = as.integer(names(crown_dist_win)),
    size = sum(wins),
    prob = crown_dist_win,
    replace = TRUE
  )
  
  loss_crowns <- sample(
    x = as.integer(names(crown_dist_loss)),
    size = sum(!wins),
    prob = crown_dist_loss,
    replace = TRUE
  )
  
  # Combine results
  crowns <- integer(n)
  crowns[wins] <- win_crowns
  crowns[!wins] <- loss_crowns
  
  data.frame(
    match_id = seq_len(n),
    win = wins,
    crowns = crowns
  )
}

#' @export
#' @param n Number of boxes to roll
#' @param trophies Trophy count for rarity weights
#' @return Data frame with box drops
roll_mystery_boxes_vectorized <- function(n, trophies) {
  weights <- rarity_weights_for_trophies(trophies)
  
  # Vectorized rarity sampling
  rarities <- sample(
    x = names(weights),
    size = n,
    prob = weights / sum(weights),
    replace = TRUE
  )
  
  # Pre-generate card pools
  card_pools <- list(
    common = paste0("C", sprintf("%02d", 1:COMMON_CARD_COUNT)),
    rare = paste0("R", sprintf("%02d", 1:RARE_CARD_COUNT)),
    epic = paste0("E", sprintf("%02d", 1:EPIC_CARD_COUNT)),
    legendary = paste0("L", sprintf("%02d", 1:LEGENDARY_CARD_COUNT))
  )
  
  # Vectorized card and quantity assignment
  cards <- character(n)
  quantities <- integer(n)
  
  for (rarity in unique(rarities)) {
    idx <- which(rarities == rarity)
    pool <- card_pools[[tolower(rarity)]]
    cards[idx] <- sample(pool, length(idx), replace = TRUE)
    
    qty_range <- switch(
      tolower(rarity),
      common = COMMON_QUANTITY_RANGE,
      rare = RARE_QUANTITY_RANGE,
      epic = EPIC_QUANTITY_RANGE,
      legendary = LEGENDARY_QUANTITY_RANGE,
      c(1L, 1L)
    )
    
    quantities[idx] <- sample(
      seq(qty_range[1], qty_range[2]),
      length(idx),
      replace = TRUE
    )
  }
  
  data.frame(
    box_id = seq_len(n),
    rarity = rarities,
    card = cards,
    quantity = quantities,
    stringsAsFactors = FALSE
  )
}

#' @export
#' @param states List of game states
#' @param cfg Configuration object
#' @return Updated states with applied upgrades
apply_upgrades_vectorized <- function(states, cfg) {
  # Extract upgrade candidates from all states at once
  all_candidates <- lapply(seq_along(states), function(i) {
    state <- states[[i]]
    rules <- cfg$progression$by_rarity
    
    candidates <- do.call(rbind, lapply(names(state$cards), function(card_name) {
      card <- state$cards[[card_name]]
      rkey <- tolower(card$rarity)
      rr <- rules[[rkey]]
      
      if (is.null(rr)) return(NULL)
      
      lvl <- card$level
      max_level <- length(rr$gold_cost_per_level) + 1L
      if (lvl >= max_level) return(NULL)
      
      idx <- lvl
      copies_needed <- rr$copies_per_level[[idx]]
      gold_cost <- rr$gold_cost_per_level[[idx]]
      
      if (is.null(copies_needed) || is.null(gold_cost)) return(NULL)
      if (card$copies < copies_needed || state$gold_bank < gold_cost) return(NULL)
      
      data.frame(
        state_id = i,
        card_name = card_name,
        rarity = card$rarity,
        copies_needed = copies_needed,
        gold_cost = gold_cost,
        power_gain = rr$power_per_level_step,
        ratio = rr$power_per_level_step / max(1, gold_cost),
        stringsAsFactors = FALSE
      )
    }))
    
    candidates
  })
  
  # Combine and sort all candidates
  all_candidates_df <- do.call(rbind, all_candidates)
  
  if (nrow(all_candidates_df) == 0) {
    return(states)
  }
  
  # Sort by value ratio
  all_candidates_df <- all_candidates_df[order(
    -all_candidates_df$ratio,
    all_candidates_df$rarity,
    all_candidates_df$card_name
  ), ]
  
  # Apply upgrades in order
  for (i in seq_len(nrow(all_candidates_df))) {
    row <- all_candidates_df[i, ]
    state <- states[[row$state_id]]
    
    # Check if upgrade is still valid
    card <- state$cards[[row$card_name]]
    if (card$copies >= row$copies_needed && state$gold_bank >= row$gold_cost) {
      # Apply upgrade
      card$copies <- card$copies - row$copies_needed
      card$level <- card$level + 1L
      state$cards[[row$card_name]] <- card
      state$gold_bank <- state$gold_bank - row$gold_cost
      state$power_score <- state$power_score + row$power_gain
      states[[row$state_id]] <- state
    }
  }
  
  return(states)
}

#' @export
#' @param days Number of days
#' @param matches_per_day Matches per day
#' @param jitter Optional jitter for matches
#' @return Vector of match counts per day
generate_match_schedule_vectorized <- function(days, matches_per_day, jitter = 0) {
  if (jitter == 0) {
    rep(matches_per_day, days)
  } else {
    pmax(0, matches_per_day + sample(-jitter:jitter, days, replace = TRUE))
  }
}

#' @export
#' @param trophies Vector of trophy counts
#' @return Matrix of rarity weights
get_rarity_weights_vectorized <- function(trophies) {
  n <- length(trophies)
  weights <- matrix(0, nrow = n, ncol = 4)
  colnames(weights) <- c("common", "rare", "epic", "legendary")
  
  idx_0_1400 <- trophies < 1400
  idx_1400_2800 <- trophies >= 1400 & trophies < 2800
  idx_2800_4200 <- trophies >= 2800 & trophies < 4200
  idx_4200_5600 <- trophies >= 4200 & trophies < 5600
  idx_5600_plus <- trophies >= 5600
  
  weights[idx_0_1400, ] <- matrix(
    rep(RARITY_WEIGHTS_0_1400, sum(idx_0_1400)),
    nrow = sum(idx_0_1400),
    byrow = TRUE
  )
  
  weights[idx_1400_2800, ] <- matrix(
    rep(RARITY_WEIGHTS_1400_2800, sum(idx_1400_2800)),
    nrow = sum(idx_1400_2800),
    byrow = TRUE
  )
  
  weights[idx_2800_4200, ] <- matrix(
    rep(RARITY_WEIGHTS_2800_4200, sum(idx_2800_4200)),
    nrow = sum(idx_2800_4200),
    byrow = TRUE
  )
  
  weights[idx_4200_5600, ] <- matrix(
    rep(RARITY_WEIGHTS_4200_5600, sum(idx_4200_5600)),
    nrow = sum(idx_4200_5600),
    byrow = TRUE
  )
  
  weights[idx_5600_plus, ] <- matrix(
    rep(RARITY_WEIGHTS_5600_PLUS, sum(idx_5600_plus)),
    nrow = sum(idx_5600_plus),
    byrow = TRUE
  )
  
  weights
}