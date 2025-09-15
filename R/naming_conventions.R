#' @title Naming Convention Guidelines
#' @description Standard naming conventions for consistency across the codebase

# NAMING CONVENTIONS:
# 
# Variables and Functions: snake_case
# - gold_bank (not goldBank)
# - calculate_win_rate() (not calculateWinRate)
# 
# Constants: UPPER_SNAKE_CASE
# - MAX_ARENA_INDEX (not maxArenaIndex)
# - DEFAULT_BOT_RATE (not defaultBotRate)
#
# R6 Classes: PascalCase
# - GameState (not game_state)
# - CacheManager (not cache_manager)
#
# R6 Methods: snake_case
# - add_gold() (not addGold)
# - get_status() (not getStatus)
#
# Configuration Keys: snake_case
# - base_winrate (not baseWinrate)
# - matches_per_day (not matchesPerDay)
#
# File Names: snake_case
# - match_simulator.R (not matchSimulator.R)
# - config_validator.R (not configValidator.R)

#' @export
#' @description Convert camelCase to snake_case
#' @param x String to convert
#' @return Converted string
camel_to_snake <- function(x) {
  # Insert underscore before uppercase letters
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  # Convert to lowercase
  tolower(x)
}

#' @export
#' @description Convert snake_case to camelCase
#' @param x String to convert
#' @return Converted string
snake_to_camel <- function(x) {
  # Split by underscore
  parts <- strsplit(x, "_")[[1]]
  if (length(parts) == 1) return(x)
  
  # Capitalize first letter of each part except first
  parts[-1] <- paste0(toupper(substring(parts[-1], 1, 1)), 
                      substring(parts[-1], 2))
  paste0(parts, collapse = "")
}

#' @export
#' @description Standardize variable names in a list/data.frame
#' @param obj Object with names to standardize
#' @return Object with standardized names
standardize_names <- function(obj) {
  if (is.data.frame(obj) || is.list(obj)) {
    names(obj) <- sapply(names(obj), camel_to_snake)
  }
  return(obj)
}

#' @export
#' @description Check if name follows convention
#' @param name Name to check
#' @param type Type of name (variable, constant, class)
#' @return TRUE if follows convention
follows_convention <- function(name, type = "variable") {
  switch(type,
    variable = grepl("^[a-z][a-z0-9_]*$", name),
    constant = grepl("^[A-Z][A-Z0-9_]*$", name),
    class = grepl("^[A-Z][a-zA-Z0-9]*$", name),
    function = grepl("^[a-z][a-z0-9_]*$", name),
    FALSE
  )
}

#' @export
#' @description Validate all names in a file follow conventions
#' @param file_path Path to R file
#' @return List of non-conforming names
check_file_conventions <- function(file_path) {
  lines <- readLines(file_path)
  issues <- list()
  
  # Check function definitions
  func_pattern <- "^\\s*([a-zA-Z0-9_.]+)\\s*<-\\s*function"
  func_lines <- grep(func_pattern, lines)
  
  for (i in func_lines) {
    func_name <- sub(func_pattern, "\\1", lines[i])
    if (!follows_convention(func_name, "function")) {
      issues <- append(issues, list(list(
        line = i,
        name = func_name,
        type = "function",
        suggestion = camel_to_snake(func_name)
      )))
    }
  }
  
  # Check variable assignments
  var_pattern <- "^\\s*([a-zA-Z0-9_.]+)\\s*<-"
  var_lines <- setdiff(grep(var_pattern, lines), func_lines)
  
  for (i in var_lines) {
    var_name <- sub(var_pattern, "\\1", lines[i])
    # Skip if it looks like a constant (all caps)
    if (toupper(var_name) == var_name) {
      if (!follows_convention(var_name, "constant")) {
        issues <- append(issues, list(list(
          line = i,
          name = var_name,
          type = "constant",
          suggestion = toupper(gsub("\\.", "_", var_name))
        )))
      }
    } else {
      if (!follows_convention(var_name, "variable")) {
        issues <- append(issues, list(list(
          line = i,
          name = var_name,
          type = "variable",
          suggestion = camel_to_snake(var_name)
        )))
      }
    }
  }
  
  return(issues)
}