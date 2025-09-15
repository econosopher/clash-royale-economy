#' @title Card Management Module
#' @description Handles card drops, mystery boxes, and lucky star mechanics
#' @import dplyr
#' @import tibble

#' @export
#' @param trophies Current trophy count
#' @return Rarity weights for current trophy range
rarity_weights_for_trophies <- function(trophies) {
  if (trophies < 1400) {
    c(common = 70, rare = 25, epic = 4, legendary = 1)
  } else if (trophies < 2800) {
    c(common = 60, rare = 30, epic = 8, legendary = 2)
  } else if (trophies < 4200) {
    c(common = 50, rare = 35, epic = 12, legendary = 3)
  } else if (trophies < 5600) {
    c(common = 40, rare = 38, epic = 17, legendary = 5)
  } else {
    c(common = 30, rare = 40, epic = 22, legendary = 8)
  }
}

#' @export
#' @param weights Rarity weights
#' @return Sampled rarity
sample_rarity <- function(weights) {
  rarities <- names(weights)
  probs <- weights / sum(weights)
  sample(rarities, size = 1, prob = probs)
}

#' @export
#' @param rarity Card rarity
#' @return Sampled card of given rarity
sample_card_of_rarity <- function(rarity) {
  pool <- switch(
    tolower(rarity),
    common = paste0("C", sprintf("%02d", 1:28)),
    rare = paste0("R", sprintf("%02d", 1:28)),
    epic = paste0("E", sprintf("%02d", 1:28)),
    legendary = paste0("L", sprintf("%02d", 1:17)),
    character(0)
  )
  if (length(pool) == 0) return(NULL)
  sample(pool, 1)
}

#' @export
#' @param rarity Card rarity
#' @return Quantity range for rarity
quantity_range_for_rarity <- function(rarity) {
  switch(
    tolower(rarity),
    common = c(5, 15),
    rare = c(2, 6),
    epic = c(1, 2),
    legendary = c(1, 1),
    c(1, 1)
  )
}

#' @export
#' @param trophies Current trophy count
#' @return Card drop from mystery box
roll_mystery_box <- function(trophies) {
  weights <- rarity_weights_for_trophies(trophies)
  rarity <- sample_rarity(weights)
  card <- sample_card_of_rarity(rarity)
  rng <- quantity_range_for_rarity(rarity)
  quantity <- sample(rng[1]:rng[2], 1)
  list(rarity = rarity, card = card, quantity = quantity)
}

#' @export
#' @param cfg Configuration object
#' @return Level-up chest configuration
levelup_chest_config <- function(cfg = NULL) {
  list(
    weights = c(common = 50, rare = 30, epic = 15, legendary = 5),
    quantities = list(
      common = c(20, 40),
      rare = c(10, 20),
      epic = c(2, 5),
      legendary = c(1, 2)
    )
  )
}

#' @export
#' @return Card drop from level-up chest
roll_levelup_chest <- function() {
  config <- levelup_chest_config()
  rarity <- sample_rarity(config$weights)
  card <- sample_card_of_rarity(rarity)
  rng <- config$quantities[[tolower(rarity)]]
  quantity <- sample(rng[1]:rng[2], 1)
  list(rarity = rarity, card = card, quantity = quantity)
}