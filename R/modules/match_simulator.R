#' @title Match Simulator Module
#' @description Handles individual match simulation logic
#' @import dplyr

#' @export
#' @param state Current game state
#' @param cfg Configuration object
#' @return List containing match results
simulate_match <- function(state, cfg, power_bonus = 0) {
  intrinsic <- effective_winrate(cfg, power_bonus)
  win_prob <- mm_adjust_winrate(cfg, intrinsic, state$trophies)
  win <- runif(1) < win_prob
  crowns <- sample_from_pmf(if (win) cfg$crown_dist$on_win else cfg$crown_dist$on_loss)
  state$trophies <- state$trophies + if (win) cfg$trophies_on_win else cfg$trophies_on_loss
  
  if (win) {
    if (!is.na(cfg$gold_drop$fixed_amount)) {
      gold <- cfg$gold_drop$fixed_amount
    } else {
      gold <- sample(seq(cfg$gold_drop$min_amount, cfg$gold_drop$max_amount), 1)
    }
  } else {
    gold <- 0L
  }
  
  list(win = win, crowns = crowns, gold = gold)
}

#' @export
#' @param cfg Configuration object
#' @return Effective win rate after bot and vertical adjustments
effective_winrate <- function(cfg, dynamic_bonus = 0) {
  base <- cfg$base_winrate
  blended <- (1.0 - cfg$bot_rate) * base + cfg$bot_rate * cfg$bot_winrate
  vertical <- if (!is.null(cfg$vertical_winrate_bonus)) cfg$vertical_winrate_bonus else 0
  bonus <- if (!is.null(dynamic_bonus)) dynamic_bonus else 0
  pmin(1.0, pmax(0.0, blended + vertical + bonus))
}

#' @export
#' @param cfg Configuration object  
#' @param intrinsic Base win rate
#' @param trophies Current trophy count
#' @return Adjusted win rate after matchmaking
mm_adjust_winrate <- function(cfg, intrinsic, trophies) {
  target <- (1.0 - cfg$bot_rate) * 0.5 + cfg$bot_rate * cfg$bot_winrate
  arena_index <- max(0L, floor(trophies / 400))
  conv <- cfg$mm_convergence_per_arena %||% 0.15
  factor <- exp(-conv * arena_index)
  pmin(1.0, pmax(0.0, target + (intrinsic - target) * factor))
}

#' @export
#' @param pmf Probability mass function
#' @return Sampled value from PMF
sample_from_pmf <- function(pmf) {
  ks <- as.integer(names(pmf))
  probs <- pmf
  sample(ks, size = 1, prob = probs)
}
