#' @title Error Handler Module
#' @description Centralized error handling with informative messages
#' @import rlang

#' @export
#' @description Custom error class for simulation errors
SimulationError <- R6::R6Class("SimulationError",
  public = list(
    type = NULL,
    message = NULL,
    context = NULL,
    timestamp = NULL,
    
    initialize = function(type, message, context = NULL) {
      self$type <- type
      self$message <- message
      self$context <- context
      self$timestamp <- Sys.time()
    },
    
    format = function() {
      paste0(
        "[", format(self$timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
        "SimulationError (", self$type, "): ", self$message,
        if (!is.null(self$context)) paste0("\nContext: ", self$context) else ""
      )
    }
  )
)

#' @export
#' @description Safe execution wrapper with error handling
#' @param expr Expression to execute
#' @param error_type Type of error for categorization
#' @param context Additional context for debugging
#' @param default Default value to return on error
#' @return Result of expression or default value
safe_execute <- function(expr, error_type = "general", context = NULL, default = NULL) {
  tryCatch({
    expr
  }, error = function(e) {
    err <- SimulationError$new(error_type, e$message, context)
    
    # Log error
    log_error(err)
    
    # Return default value
    if (!is.null(default)) {
      warning(err$format())
      return(default)
    } else {
      stop(err$format())
    }
  })
}

#' @export
#' @description Validate input with informative error messages
#' @param value Value to validate
#' @param validator Validation function
#' @param param_name Parameter name for error message
#' @param expected Description of expected value
validate_input <- function(value, validator, param_name, expected) {
  if (!validator(value)) {
    stop(SimulationError$new(
      "validation",
      sprintf("Invalid %s: expected %s, got %s", 
              param_name, expected, 
              ifelse(is.null(value), "NULL", class(value)[1])),
      sprintf("Value: %s", deparse(value, nlines = 1))
    )$format())
  }
  invisible(value)
}

#' @export
#' @description Wrap function with error handling
#' @param fn Function to wrap
#' @param error_type Error type for this function
with_error_handling <- function(fn, error_type = "function") {
  function(...) {
    args <- list(...)
    context <- sprintf("Function: %s, Args: %s", 
                      deparse(substitute(fn)), 
                      substr(deparse(args, nlines = 1), 1, 100))
    
    safe_execute(
      fn(...),
      error_type = error_type,
      context = context
    )
  }
}

#' @export
#' @description Log error to file
#' @param error SimulationError object
#' @param log_file Path to log file
log_error <- function(error, log_file = "runs/error.log") {
  tryCatch({
    # Ensure log directory exists
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }
    
    # Append error to log
    cat(error$format(), "\n", file = log_file, append = TRUE)
  }, error = function(e) {
    # If logging fails, just print to console
    message("Failed to log error: ", e$message)
    message(error$format())
  })
}

#' @export
#' @description Assert condition with custom error message
#' @param condition Condition to check
#' @param message Error message if condition is FALSE
#' @param error_type Type of error
assert_that <- function(condition, message, error_type = "assertion") {
  if (!isTRUE(condition)) {
    stop(SimulationError$new(error_type, message)$format())
  }
  invisible(TRUE)
}

#' @export
#' @description Retry operation with exponential backoff
#' @param expr Expression to retry
#' @param max_attempts Maximum number of attempts
#' @param initial_delay Initial delay in seconds
#' @param backoff_factor Multiplier for delay after each failure
retry_with_backoff <- function(expr, max_attempts = 3, initial_delay = 0.1, backoff_factor = 2) {
  delay <- initial_delay
  
  for (i in seq_len(max_attempts)) {
    result <- tryCatch({
      list(success = TRUE, value = expr)
    }, error = function(e) {
      list(success = FALSE, error = e)
    })
    
    if (result$success) {
      return(result$value)
    }
    
    if (i < max_attempts) {
      message(sprintf("Attempt %d failed, retrying in %0.1f seconds...", i, delay))
      Sys.sleep(delay)
      delay <- delay * backoff_factor
    }
  }
  
  stop(SimulationError$new(
    "retry_exhausted",
    sprintf("Operation failed after %d attempts", max_attempts),
    result$error$message
  )$format())
}

#' @export
#' @description Validate simulation state consistency
#' @param state Game state object
validate_state <- function(state) {
  errors <- character()
  
  # Check basic invariants
  if (state$trophies < 0) {
    errors <- c(errors, "Trophies cannot be negative")
  }
  
  if (state$gold_bank < 0) {
    errors <- c(errors, "Gold bank cannot be negative")
  }
  
  if (state$power_score < 0) {
    errors <- c(errors, "Power score cannot be negative")
  }
  
  if (state$account_level < 1) {
    errors <- c(errors, "Account level must be at least 1")
  }
  
  if (state$tower_level != state$account_level) {
    errors <- c(errors, "Tower level must match account level")
  }
  
  if (state$pass_level < 0) {
    errors <- c(errors, "Pass level cannot be negative")
  }
  
  # Check card consistency
  for (card_name in names(state$cards)) {
    card <- state$cards[[card_name]]
    
    if (card$level < 1) {
      errors <- c(errors, sprintf("Card %s level must be at least 1", card_name))
    }
    
    if (card$copies < 0) {
      errors <- c(errors, sprintf("Card %s copies cannot be negative", card_name))
    }
  }
  
  if (length(errors) > 0) {
    stop(SimulationError$new(
      "state_validation",
      paste(errors, collapse = "; "),
      "State validation failed"
    )$format())
  }
  
  invisible(TRUE)
}

#' @export
#' @description Create error boundary for Shiny app
#' @param ui UI element to wrap
#' @param error_ui UI to show on error
error_boundary <- function(ui, error_ui = NULL) {
  if (is.null(error_ui)) {
    error_ui <- div(
      class = "alert alert-danger",
      h4("An error occurred"),
      p("Please refresh the page or contact support if the problem persists."),
      verbatimTextOutput("error_details")
    )
  }
  
  tryCatch({
    ui
  }, error = function(e) {
    error_ui
  })
}

#' @export
#' @description Format error for user display
#' @param error Error object
#' @param technical Include technical details
format_user_error <- function(error, technical = FALSE) {
  if (inherits(error, "SimulationError")) {
    if (technical) {
      error$format()
    } else {
      switch(error$type,
        validation = paste("Invalid input:", error$message),
        configuration = paste("Configuration error:", error$message),
        simulation = paste("Simulation error:", error$message),
        paste("An error occurred:", error$message)
      )
    }
  } else {
    if (technical) {
      paste("Error:", error$message, "\nCall:", deparse(error$call))
    } else {
      paste("An unexpected error occurred. Please try again.")
    }
  }
}