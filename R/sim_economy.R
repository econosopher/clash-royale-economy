library(dplyr)
library(tibble)
library(purrr)

source("R/sim_config.R")
source("R/lucky_star.R")

simulate_match <- function(state, cfg, power_bonus = 0) {
  intrinsic <- effective_winrate(cfg, power_bonus)
  win_prob <- mm_adjust_winrate(cfg, intrinsic, state$trophies)
  win <- runif(1) < win_prob
  crowns <- sample_from_pmf(if (win) cfg$crown_dist$on_win else cfg$crown_dist$on_loss)
  state$trophies <- max(0L, state$trophies + if (win) cfg$trophies_on_win else cfg$trophies_on_loss)
  if (win) {
    if (!is.na(cfg$gold_drop$fixed_amount)) {
      gold <- cfg$gold_drop$fixed_amount
    } else {
      gold <- sample(min(cfg$gold_drop$min_amount, cfg$gold_drop$max_amount):max(cfg$gold_drop$min_amount, cfg$gold_drop$max_amount), 1)
    }
  } else {
    gold <- 0L
  }
  list(win = win, crowns = crowns, gold = gold, state = state)
}

apply_card_drop <- function(state, drop) {
  rar <- drop$rarity; card <- drop$card; qty <- drop$quantity
  if (is.null(state$inventory_by_rarity[[rar]])) state$inventory_by_rarity[[rar]] <- 0L
  state$inventory_by_rarity[[rar]] <- state$inventory_by_rarity[[rar]] + qty
  state$inventory_by_card[[card]] <- (state$inventory_by_card[[card]] %||% 0L) + qty
  # Track copies only; upgrades omitted in first pass
  if (is.null(state$cards[[card]])) state$cards[[card]] <- list(name=card, rarity=rar, level=1L, copies=0L)
  state$cards[[card]]$copies <- state$cards[[card]]$copies + qty
  state
}



# Greedy upgrades with deterministic tie-breaking.
# This function iterates through all possible card upgrades and applies the one
# with the highest "value" (power gain per gold spent). This process is repeated
# until no more upgrades can be afforded.
greedy_apply_upgrades <- function(state, cfg) {
  rules <- cfg$progression$by_rarity
  gold_spent <- 0L; upgrades <- 0L; power_gained <- 0.0
  
  # Safety break to prevent infinite loops in case of unexpected behavior.
  MAX_UPGRADE_ITERATIONS <- 10000L
  safety <- 0L
  
  steps <- tibble::tibble(rarity=character(), gold_spent=double(), power_gained=double())
  repeat {
    safety <- safety + 1L
    if (safety > MAX_UPGRADE_ITERATIONS) {
      warning("Exceeded max upgrade iterations. Breaking.")
      break
    }
    
    # Collect all possible upgrades that can be afforded.
    candidates <- purrr::imap_dfr(state$cards, function(c, nm){
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
      tibble::tibble(name = nm, rarity = c$rarity, copies_needed = copies_needed,
                     gold_cost = gold_cost, value = value, ratio = ratio,
                     rarity_rank = rarity_rank(c$rarity))
    })
    
    if (nrow(candidates) == 0) break
    
    # Apply the best upgrade based on value/gold ratio, then rarity, then name.
    best <- candidates %>%
      dplyr::mutate(ratio = round(ratio, 8)) %>%
      dplyr::arrange(dplyr::desc(ratio), rarity_rank, name) %>%
      dplyr::slice(1)
    
    # Update state after applying the upgrade.
    card <- state$cards[[best$name]]
    card$copies <- card$copies - best$copies_needed
    card$level <- card$level + 1L
    state$cards[[best$name]] <- card
    state$gold_bank <- state$gold_bank - best$gold_cost
    gold_spent <- gold_spent + best$gold_cost
    upgrades <- upgrades + 1L
    power_gained <- power_gained + best$value
    state$power_score <- (state$power_score %||% 0.0) + best$value
    steps <- dplyr::bind_rows(steps, tibble::tibble(rarity = as.character(best$rarity), gold_spent = as.numeric(best$gold_cost), power_gained = as.numeric(best$value)))
  }
  list(state = state, gold_spent = gold_spent, upgrades = upgrades, power_gained = power_gained, steps = steps)
}

simulate_day <- function(state, cfg, day_index) {
  matches <- cfg$matches_per_day
  wins <- 0L; losses <- 0L
  crowns_today <- 0L
  gold_from_matches <- 0L
  gold_from_pass_today <- 0L
  boxes_earned_today <- 0L
  box_rarity_counts <- c(common=0L, rare=0L, epic=0L, legendary=0L)
  gold_spent_today <- 0L
  upgrades_today <- 0L
  power_gained_today <- 0.0

  lucky <- if (cfg$lucky_drop$enabled) list(star=load_star_upgrade(), final=load_final_reward()) else NULL
  gold_by_rarity <- c(common=0.0, rare=0.0, epic=0.0, legendary=0.0)
  power_by_rarity <- c(common=0.0, rare=0.0, epic=0.0, legendary=0.0)
  daily_box_cap <- cfg$caps$daily_mystery_boxes
  if (is.null(daily_box_cap) || !is.finite(daily_box_cap)) {
    daily_box_cap <- Inf
  } else {
    daily_box_cap <- max(0L, as.integer(daily_box_cap))
  }

  for (m in seq_len(matches)) {
    power_bonus <- power_bonus_from_score(cfg, state$power_score %||% 0.0)
    res <- simulate_match(state, cfg, power_bonus)
    state <- res$state
    win <- res$win
    crowns <- as.integer(res$crowns)
    gold <- as.integer(res$gold)

    state$pass_crowns <- state$pass_crowns + crowns
    crowns_today <- crowns_today + crowns

    if (win) {
      next_win_index <- wins + 1L
      reward_label <- NULL
      final_star <- NULL
      if (!is.null(lucky) && boxes_earned_today < daily_box_cap &&
          next_win_index <= cfg$lucky_drop$wins_with_lucky_drops) {
        start <- if (next_win_index == 1L) 2L else 1L
        final_star <- lucky_star_roll_star(start, cfg$lucky_drop$spins, lucky$star)
        reward_label <- lucky_star_roll_final(final_star, lucky$final)
      }
      if (boxes_earned_today < daily_box_cap) {
        boxes_earned_today <- boxes_earned_today + 1L
        reward <- reward_label %||% "Random Cards"
        if (identical(reward, "Gold")) {
          gextra <- cfg$lucky_drop$gold_reward_by_star[as.character(final_star)] %||% 0L
          if (gextra > 0) {
            state$gold_bank <- state$gold_bank + gextra
            state$gold_today <- state$gold_today + gextra
            gold_from_matches <- gold_from_matches + gextra
          }
        } else {
          pr <- rarity_weights_for_trophies(state$trophies)
          rarity <- sample(names(pr), 1, prob = pr)
          card <- paste0("Card_", rarity, "_", sample(1:10,1))
          qty <- if (rarity=="common") sample(5:10,1) else if (rarity=="rare") sample(2:4,1) else 1L
          state <- apply_card_drop(state, list(rarity=rarity, card=card, quantity=qty))
          box_rarity_counts[rarity] <- box_rarity_counts[rarity] + 1L
        }
      } else if (!is.null(reward_label) && identical(reward_label, "Random Cards")) {
        pr <- rarity_weights_for_trophies(state$trophies)
        rarity <- sample(names(pr), 1, prob = pr)
        card <- paste0("Card_", rarity, "_", sample(1:10,1))
        qty <- if (rarity=="common") sample(5:10,1) else if (rarity=="rare") sample(2:4,1) else 1L
        state <- apply_card_drop(state, list(rarity=rarity, card=card, quantity=qty))
      }
    }

    if (gold > 0) {
      state$gold_today <- state$gold_today + gold
      state$gold_bank <- state$gold_bank + gold
      gold_from_matches <- gold_from_matches + gold
    }

    if (win) wins <- wins + 1L else losses <- losses + 1L

    # Pass levels and gold
    levels_before <- state$pass_levels
    levels_after <- state$pass_crowns %/% cfg$pass_config$crowns_per_level
    if (levels_after > levels_before) {
      gained <- levels_after - levels_before
      state$pass_levels <- levels_after
      gained_gold <- gained * cfg$pass_config$gold_per_level
      state$gold_bank <- state$gold_bank + gained_gold
      gold_from_pass_today <- gold_from_pass_today + gained_gold
    }

    # Try upgrades after rewards this match
    up <- greedy_apply_upgrades(state, cfg)
    state <- up$state
    if (up$gold_spent > 0) gold_spent_today <- gold_spent_today + up$gold_spent
    if (up$upgrades > 0) upgrades_today <- upgrades_today + up$upgrades
    if (up$power_gained > 0) power_gained_today <- power_gained_today + up$power_gained
    if (nrow(up$steps) > 0) {
      agg <- up$steps %>% dplyr::group_by(rarity = tolower(rarity)) %>% dplyr::summarise(gold = sum(gold_spent), pow = sum(power_gained), .groups='drop')
      for (i in seq_len(nrow(agg))) {
        r <- agg$rarity[i]
        if (r %in% names(gold_by_rarity)) {
          gold_by_rarity[[r]] <- gold_by_rarity[[r]] + agg$gold[i]
          power_by_rarity[[r]] <- power_by_rarity[[r]] + agg$pow[i]
        }
      }
    }
  }

  # Levels by rarity and averages at end of day
  levels_tbl <- NULL
  if (length(state$cards) > 0) {
    levels_tbl <- purrr::map_dfr(state$cards, ~as.data.frame(.x)) %>%
      dplyr::group_by(rarity) %>% dplyr::summarise(avg_level = mean(level), .groups = 'drop')
  }
  scalar_avg <- function(tbl, rar) {
    if (is.null(tbl) || nrow(tbl) == 0) return(NA_real_)
    val <- tbl$avg_level[tolower(tbl$rarity) == tolower(rar)]
    if (length(val) == 0) NA_real_ else as.numeric(val[[1]])
  }
  avg_level <- if (!is.null(levels_tbl) && nrow(levels_tbl) > 0) mean(levels_tbl$avg_level) else NA_real_
  avg_level_common <- scalar_avg(levels_tbl, 'common')
  avg_level_rare <- scalar_avg(levels_tbl, 'rare')
  avg_level_epic <- scalar_avg(levels_tbl, 'epic')
  avg_level_legendary <- scalar_avg(levels_tbl, 'legendary')

  arena <- current_arena(cfg$arenas, state$trophies)
  pbonus <- power_bonus_from_score(cfg, state$power_score %||% 0.0)
  out <- list(
    day = day_index,
    matches = matches,
    wins = wins,
    losses = losses,
    winrate = if (matches>0) wins/matches else 0,
    crowns = crowns_today,
    boxes_earned = boxes_earned_today,
    gold_from_matches = gold_from_matches,
    gold_from_pass = gold_from_pass_today,
    gold_total = gold_from_matches + gold_from_pass_today,
    pass_levels_total = state$pass_levels,
    pass_crowns_total = state$pass_crowns,
    trophies_end = state$trophies,
    box_drops_by_rarity = list(box_rarity_counts),
    gold_spent_upgrades = gold_spent_today,
    upgrades_applied = upgrades_today,
    power_gained = power_gained_today,
    power_total = state$power_score %||% 0.0,
    power_bonus = pbonus,
    arena_end = arena$name[1],
    boxes_common = box_rarity_counts[['common']] %||% 0L,
    boxes_rare = box_rarity_counts[['rare']] %||% 0L,
    boxes_epic = box_rarity_counts[['epic']] %||% 0L,
    boxes_legendary = box_rarity_counts[['legendary']] %||% 0L,
    gold_spent_common = gold_by_rarity[['common']] %||% 0,
    gold_spent_rare = gold_by_rarity[['rare']] %||% 0,
    gold_spent_epic = gold_by_rarity[['epic']] %||% 0,
    gold_spent_legendary = gold_by_rarity[['legendary']] %||% 0,
    power_gained_common = power_by_rarity[['common']] %||% 0,
    power_gained_rare = power_by_rarity[['rare']] %||% 0,
    power_gained_epic = power_by_rarity[['epic']] %||% 0,
    power_gained_legendary = power_by_rarity[['legendary']] %||% 0,
    avg_level = avg_level,
    avg_level_common = avg_level_common,
    avg_level_rare = avg_level_rare,
    avg_level_epic = avg_level_epic,
    avg_level_legendary = avg_level_legendary,
    gold_spent_by_rarity = list(gold_by_rarity),
    power_gained_by_rarity = list(power_by_rarity)
  )
  list(summary = out, state = state)
}

simulate_season <- function(cfg, seed = 42, progress = NULL, should_cancel = NULL) {
  set.seed(seed)
  state <- list(
    day = 0L, trophies = 0L, pass_crowns = 0L, pass_levels = 0L, gold_bank = 0L,
    inventory_by_rarity = list(), inventory_by_card = list(), cards = list(), gold_today = 0L,
    power_score = 0.0
  )
  daily <- vector("list", cfg$days)
  for (d in seq_len(cfg$days)) {
    if (is.function(progress)) {
      # report absolute progress [0..1] with day context
      try(progress(d, cfg$days), silent = TRUE)
    }
    if (is.function(should_cancel)) {
      sc <- try(should_cancel(), silent = TRUE)
      if (!inherits(sc, 'try-error') && isTRUE(sc)) break
    }
    state$gold_today <- 0L
    res <- simulate_day(state, cfg, d)
    state <- res$state
    daily[[d]] <- res$summary
  }
  # Robust row-binding with protection for vector-valued fields
  box_vectors <- function(x) {
    if (is.atomic(x) && length(x) != 1) list(x) else x
  }
  # Filter out any NULL days (e.g., on cancellation)
  daily <- Filter(Negate(is.null), daily)
  daily_df <- dplyr::bind_rows(lapply(daily, function(row){
    # ensure any vector-valued fields become list-cols, then coerce to 1-row tibble
    row2 <- lapply(row, box_vectors)
    tibble::as_tibble_row(row2)
  }))
  list(daily = daily_df, state = state)
}
