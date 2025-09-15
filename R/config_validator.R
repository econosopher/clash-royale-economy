#' @title Configuration Validator
#' @description Validates and sanitizes simulation configuration parameters
#' @import checkmate

#' @export
#' @param cfg Configuration list to validate
#' @return Validated configuration or stops with error
validate_config <- function(cfg) {
  errors <- character()
  warnings <- character()
  
  # Core parameters
  if (!checkmate::test_int(cfg$days, lower = 1, upper = 1000)) {
    errors <- c(errors, "days must be an integer between 1 and 1000")
  }
  
  if (!checkmate::test_int(cfg$matches_per_day, lower = 0, upper = 100)) {
    errors <- c(errors, "matches_per_day must be an integer between 0 and 100")
  }
  
  # Win rates
  if (!checkmate::test_number(cfg$base_winrate, lower = 0, upper = 1)) {
    errors <- c(errors, "base_winrate must be between 0 and 1")
  }
  
  if (!checkmate::test_number(cfg$bot_rate, lower = 0, upper = 1)) {
    errors <- c(errors, "bot_rate must be between 0 and 1")
  }
  
  if (!checkmate::test_number(cfg$bot_winrate, lower = 0, upper = 1)) {
    errors <- c(errors, "bot_winrate must be between 0 and 1")
  }
  
  if (!checkmate::test_number(cfg$vertical_winrate_bonus, lower = -1, upper = 1)) {
    errors <- c(errors, "vertical_winrate_bonus must be between -1 and 1")
  }
  
  # Trophy changes
  if (!checkmate::test_int(cfg$trophies_on_win, lower = 0)) {
    errors <- c(errors, "trophies_on_win must be a non-negative integer")
  }
  
  if (!checkmate::test_int(cfg$trophies_on_loss, upper = 0)) {
    errors <- c(errors, "trophies_on_loss must be a non-positive integer")
  }
  
  # Gold drops
  if (!is.na(cfg$gold_drop$fixed_amount)) {
    if (!checkmate::test_int(cfg$gold_drop$fixed_amount, lower = 0)) {
      errors <- c(errors, "gold_drop$fixed_amount must be a non-negative integer or NA")
    }
  } else {
    if (!checkmate::test_int(cfg$gold_drop$min_amount, lower = 0)) {
      errors <- c(errors, "gold_drop$min_amount must be a non-negative integer")
    }
    if (!checkmate::test_int(cfg$gold_drop$max_amount, lower = 0)) {
      errors <- c(errors, "gold_drop$max_amount must be a non-negative integer")
    }
    if (cfg$gold_drop$min_amount > cfg$gold_drop$max_amount) {
      errors <- c(errors, "gold_drop$min_amount cannot exceed gold_drop$max_amount")
    }
  }
  
  # Pass configuration
  if (!checkmate::test_int(cfg$pass_config$crowns_per_level, lower = 1)) {
    errors <- c(errors, "pass_config$crowns_per_level must be a positive integer")
  }
  
  if (!checkmate::test_int(cfg$pass_config$gold_per_level, lower = 0)) {
    errors <- c(errors, "pass_config$gold_per_level must be a non-negative integer")
  }
  
  # Crown distributions
  validate_crown_dist <- function(dist, name) {
    if (!is.numeric(dist) || length(dist) != 4) {
      return(paste0(name, " must be a numeric vector of length 4"))
    }
    if (any(dist < 0) || any(dist > 1)) {
      return(paste0(name, " probabilities must be between 0 and 1"))
    }
    total <- sum(dist)
    if (abs(total - 1.0) > 0.001) {
      return(paste0(name, " probabilities must sum to 1 (currently ", round(total, 3), ")")
    }
    return(NULL)
  }
  
  win_error <- validate_crown_dist(cfg$crown_dist$on_win, "crown_dist$on_win")
  if (!is.null(win_error)) errors <- c(errors, win_error)
  
  loss_error <- validate_crown_dist(cfg$crown_dist$on_loss, "crown_dist$on_loss")
  if (!is.null(loss_error)) errors <- c(errors, loss_error)
  
  # Power to winrate conversion
  if (!checkmate::test_number(cfg$power_to_winrate$slope, lower = 0, upper = 0.01)) {
    warnings <- c(warnings, "power_to_winrate$slope outside typical range [0, 0.01]")
  }
  
  if (!checkmate::test_number(cfg$power_to_winrate$max_bonus, lower = 0, upper = 0.5)) {
    warnings <- c(warnings, "power_to_winrate$max_bonus outside typical range [0, 0.5]")
  }
  
  # Matchmaking convergence
  if (!checkmate::test_number(cfg$mm_convergence_per_arena, lower = 0, upper = 1)) {
    errors <- c(errors, "mm_convergence_per_arena must be between 0 and 1")
  }
  
  # Lucky star configuration (if enabled)
  if (isTRUE(cfg$lucky_drop$enabled)) {
    if (!checkmate::test_int(cfg$lucky_drop$wins_with_lucky_drops, lower = 1, upper = 10)) {
      errors <- c(errors, "lucky_drop$wins_with_lucky_drops must be between 1 and 10")
    }
    if (!checkmate::test_int(cfg$lucky_drop$spins, lower = 1, upper = 10)) {
      errors <- c(errors, "lucky_drop$spins must be between 1 and 10")
    }
  }
  
  # Daily caps
  if (!checkmate::test_int(cfg$caps$daily_mystery_boxes, lower = 0, upper = 100)) {
    errors <- c(errors, "caps$daily_mystery_boxes must be between 0 and 100")
  }
  
  # Report errors and warnings
  if (length(errors) > 0) {
    stop("Configuration validation failed:\n", paste("  -", errors, collapse = "\n"))
  }
  
  if (length(warnings) > 0) {
    for (w in warnings) {
      warning("Configuration warning: ", w)
    }
  }
  
  # Normalize crown distributions
  cfg$crown_dist$on_win <- cfg$crown_dist$on_win / sum(cfg$crown_dist$on_win)
  cfg$crown_dist$on_loss <- cfg$crown_dist$on_loss / sum(cfg$crown_dist$on_loss)
  
  return(cfg)
}

#' @export
#' @param cfg Configuration list
#' @return Configuration with any missing fields filled with defaults
sanitize_config <- function(cfg) {
  defaults <- default_config()
  
  # Recursively merge configs
  merge_configs <- function(base, override) {
    if (!is.list(base)) return(override)
    if (!is.list(override)) return(base)
    
    for (key in names(base)) {
      if (key %in% names(override)) {
        base[[key]] <- merge_configs(base[[key]], override[[key]])
      }
    }
    
    # Add any keys from override not in base
    for (key in names(override)) {
      if (!(key %in% names(base))) {
        base[[key]] <- override[[key]]
      }
    }
    
    return(base)
  }
  
  return(merge_configs(defaults, cfg))
}

#' @export
#' @param cfg Configuration list
#' @return TRUE if valid, FALSE otherwise
is_valid_config <- function(cfg) {
  tryCatch({
    validate_config(cfg)
    TRUE
  }, error = function(e) {
    FALSE
  })
}