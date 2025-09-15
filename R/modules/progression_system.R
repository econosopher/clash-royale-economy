#' @title Progression System Module
#' @description Handles card upgrades, power calculation, and progression logic
#' @import dplyr
#' @import tibble
#' @import purrr

#' @export
#' @param state Current game state
#' @param drop Card drop to apply
#' @return Updated game state
apply_card_drop <- function(state, drop) {
  rar <- drop$rarity
  card <- drop$card
  qty <- drop$quantity
  
  if (is.null(state$inventory_by_rarity[[rar]])) {
    state$inventory_by_rarity[[rar]] <- 0L
  }
  state$inventory_by_rarity[[rar]] <- state$inventory_by_rarity[[rar]] + qty
  state$inventory_by_card[[card]] <- (state$inventory_by_card[[card]] %||% 0L) + qty
  
  if (is.null(state$cards[[card]])) {
    state$cards[[card]] <- list(name = card, rarity = rar, level = 1L, copies = 0L)
  }
  state$cards[[card]]$copies <- state$cards[[card]]$copies + qty
  
  state
}

#' @export
#' @param state Current game state
#' @param cfg Configuration object
#' @return List with updated state and upgrade statistics
greedy_apply_upgrades <- function(state, cfg) {
  rules <- cfg$progression$by_rarity
  gold_spent <- 0L
  upgrades <- 0L
  power_gained <- 0.0
  
  MAX_UPGRADE_ITERATIONS <- 10000L
  safety <- 0L
  
  steps <- tibble::tibble(
    rarity = character(),
    gold_spent = double(),
    power_gained = double()
  )
  
  repeat {
    safety <- safety + 1L
    if (safety > MAX_UPGRADE_ITERATIONS) {
      warning("Exceeded max upgrade iterations. Breaking.")
      break
    }
    
    candidates <- purrr::imap_dfr(state$cards, function(c, nm) {
      rkey <- tolower(c$rarity)
      rr <- rules[[rkey]]
      if (is.null(rr)) return(NULL)
      
      lvl <- c$level
      max_level <- length(rr$gold_cost_per_level) + 1L
      if (lvl >= max_level) return(NULL)
      
      idx <- lvl
      copies_needed <- rr$copies_per_level[[idx]] %||% NA
      gold_cost <- rr$gold_cost_per_level[[idx]] %||% NA
      if (is.na(copies_needed) || is.na(gold_cost)) return(NULL)
      
      if (c$copies < copies_needed || state$gold_bank < gold_cost) return(NULL)
      
      value <- rr$power_per_level_step
      ratio <- value / max(1, gold_cost)
      tibble::tibble(
        name = nm,
        rarity = c$rarity,
        copies_needed = copies_needed,
        gold_cost = gold_cost,
        value = value,
        ratio = ratio,
        rarity_rank = rarity_rank(c$rarity)
      )
    })
    
    if (nrow(candidates) == 0) break
    
    best <- candidates %>%
      dplyr::mutate(ratio = round(ratio, 8)) %>%
      dplyr::arrange(dplyr::desc(ratio), rarity_rank, name) %>%
      dplyr::slice(1)
    
    card <- state$cards[[best$name]]
    card$copies <- card$copies - best$copies_needed
    card$level <- card$level + 1L
    state$cards[[best$name]] <- card
    state$gold_bank <- state$gold_bank - best$gold_cost
    gold_spent <- gold_spent + best$gold_cost
    upgrades <- upgrades + 1L
    power_gained <- power_gained + best$value
    state$power_score <- (state$power_score %||% 0.0) + best$value
    
    rkey <- tolower(best$rarity)
    acc_pts <- (rules[[rkey]]$account_points_per_level[[card$level - 1L]] %||% 0L)
    state$account_points <- (state$account_points %||% 0L) + acc_pts
    
    steps <- dplyr::bind_rows(
      steps,
      tibble::tibble(
        rarity = best$rarity,
        gold_spent = best$gold_cost,
        power_gained = best$value
      )
    )
  }
  
  list(
    state = state,
    gold_spent = gold_spent,
    upgrades = upgrades,
    power_gained = power_gained,
    steps = steps
  )
}

#' @export
#' @param r Rarity string
#' @return Numeric rank for rarity
rarity_rank <- function(r) {
  ranks <- c(common = 0L, rare = 1L, epic = 2L, legendary = 3L)
  ranks[[tolower(r)]] %||% 99L
}

#' @export
#' @param cfg Configuration object
#' @param power_score Current power score
#' @return Win rate bonus from power
power_bonus_from_score <- function(cfg, power_score) {
  slope <- cfg$power_to_winrate$slope
  cap <- cfg$power_to_winrate$max_bonus
  pmin(cap, pmax(0.0, slope * power_score))
}