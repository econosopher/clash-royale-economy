#' @title WebSocket Server for Real-time Updates
#' @description Provides WebSocket support for real-time simulation progress
#' @import httpuv
#' @import jsonlite
#' @import later

source("R/constants.R")

#' @export
SimulationWebSocket <- R6::R6Class("SimulationWebSocket",
  public = list(
    #' @field ws WebSocket connection object
    ws = NULL,
    
    #' @field session Shiny session object
    session = NULL,
    
    #' @field status Current simulation status
    status = "idle",
    
    #' @field progress Current progress (0-100)
    progress = 0,
    
    #' @field current_day Current simulation day
    current_day = 0,
    
    #' @field total_days Total days to simulate
    total_days = 0,
    
    #' @field start_time Simulation start time
    start_time = NULL,
    
    #' @field messages Queue of messages to send
    messages = list(),
    
    #' @description Initialize WebSocket handler
    #' @param session Shiny session object
    initialize = function(session = NULL) {
      self$session <- session
      self$messages <- list()
      invisible(self)
    },
    
    #' @description Start simulation with progress updates
    #' @param cfg Configuration object
    #' @param seed Random seed
    #' @param callback Optional callback function
    start_simulation = function(cfg, seed, callback = NULL) {
      self$status <- "running"
      self$progress <- 0
      self$current_day <- 0
      self$total_days <- cfg$days
      self$start_time <- Sys.time()
      
      # Send initial status
      self$send_update(list(
        type = "start",
        status = self$status,
        progress = self$progress,
        total_days = self$total_days,
        config = list(
          matches_per_day = cfg$matches_per_day,
          base_winrate = cfg$base_winrate,
          bot_rate = cfg$bot_rate
        )
      ))
      
      # Run simulation with periodic updates
      tryCatch({
        result <- self$run_with_updates(cfg, seed)
        
        # Send completion
        self$status <- "completed"
        self$progress <- 100
        self$send_update(list(
          type = "complete",
          status = self$status,
          progress = 100,
          duration = as.numeric(Sys.time() - self$start_time, units = "secs"),
          summary = list(
            final_trophies = tail(result$daily$trophies_end, 1),
            total_gold = sum(result$daily$gold_total),
            avg_winrate = mean(result$daily$winrate)
          )
        ))
        
        if (!is.null(callback)) {
          callback(result)
        }
        
        return(result)
      }, error = function(e) {
        self$status <- "error"
        self$send_update(list(
          type = "error",
          status = self$status,
          message = e$message
        ))
        stop(e)
      })
    },
    
    #' @description Run simulation with progress updates
    #' @param cfg Configuration object
    #' @param seed Random seed
    #' @return Simulation results
    run_with_updates = function(cfg, seed) {
      set.seed(seed)
      
      # Initialize state
      state <- list(
        trophies = 0L,
        gold_bank = 0L,
        power_score = 0.0,
        account_points = 0L,
        account_level = 1L,
        tower_level = 1L,
        pass_crowns = 0L,
        pass_level = 0L,
        cards = list(),
        inventory_by_rarity = list(),
        inventory_by_card = list()
      )
      
      daily_results <- list()
      update_interval <- max(1, floor(cfg$days / 20))  # Update 20 times
      
      for (day in seq_len(cfg$days)) {
        # Simulate day
        day_result <- simulate_day(state, cfg, day)
        state <- day_result$state
        daily_results[[day]] <- day_result$summary
        
        # Update progress
        self$current_day <- day
        self$progress <- round(100 * day / cfg$days)
        
        # Send update at intervals
        if (day %% update_interval == 0 || day == cfg$days) {
          self$send_update(list(
            type = "progress",
            status = self$status,
            progress = self$progress,
            current_day = day,
            total_days = cfg$days,
            current_stats = list(
              trophies = state$trophies,
              gold = state$gold_bank,
              power = round(state$power_score, 2),
              pass_level = state$pass_level
            )
          ))
          
          # Allow UI to update
          Sys.sleep(0.01)
        }
      }
      
      return(list(
        daily = do.call(rbind, daily_results),
        final_state = state
      ))
    },
    
    #' @description Send update message
    #' @param message Message to send
    send_update = function(message) {
      if (!is.null(self$session)) {
        # Send via Shiny custom message
        self$session$sendCustomMessage(
          "simulation_update",
          message
        )
      }
      
      # Store in message queue
      self$messages <- append(self$messages, list(message))
      
      # Trim message queue
      if (length(self$messages) > 100) {
        self$messages <- tail(self$messages, 50)
      }
      
      invisible(self)
    },
    
    #' @description Get current status
    #' @return Status object
    get_status = function() {
      list(
        status = self$status,
        progress = self$progress,
        current_day = self$current_day,
        total_days = self$total_days,
        duration = if (!is.null(self$start_time)) {
          as.numeric(Sys.time() - self$start_time, units = "secs")
        } else NULL
      )
    },
    
    #' @description Cancel running simulation
    cancel = function() {
      self$status <- "cancelled"
      self$send_update(list(
        type = "cancelled",
        status = self$status,
        progress = self$progress
      ))
      invisible(self)
    },
    
    #' @description Reset state
    reset = function() {
      self$status <- "idle"
      self$progress <- 0
      self$current_day <- 0
      self$total_days <- 0
      self$start_time <- NULL
      self$messages <- list()
      invisible(self)
    }
  )
)

#' @export
#' @description Create WebSocket handler for Shiny
create_websocket_handler <- function() {
  function(ws) {
    sim_ws <- SimulationWebSocket$new()
    
    ws$onMessage(function(binary, message) {
      msg <- jsonlite::fromJSON(message)
      
      if (msg$type == "start") {
        # Start simulation in background
        later::later(function() {
          sim_ws$start_simulation(
            cfg = msg$config,
            seed = msg$seed,
            callback = function(result) {
              ws$send(jsonlite::toJSON(list(
                type = "result",
                data = result
              ), auto_unbox = TRUE))
            }
          )
        }, 0.1)
      } else if (msg$type == "status") {
        ws$send(jsonlite::toJSON(
          sim_ws$get_status(),
          auto_unbox = TRUE
        ))
      } else if (msg$type == "cancel") {
        sim_ws$cancel()
      }
    })
    
    ws$onClose(function() {
      sim_ws$reset()
    })
  }
}

#' @export
#' @description Add WebSocket support to Shiny app
add_websocket_support <- function(session) {
  # Add custom message handler
  session$sendCustomMessage("websocket_init", list(
    url = paste0("ws://", DEFAULT_SHINY_HOST, ":", DEFAULT_SHINY_PORT, "/websocket")
  ))
  
  # JavaScript for WebSocket client
  js_code <- '
  Shiny.addCustomMessageHandler("websocket_init", function(msg) {
    var ws = new WebSocket(msg.url);
    
    ws.onopen = function() {
      console.log("WebSocket connected");
      Shiny.setInputValue("ws_connected", true);
    };
    
    ws.onmessage = function(event) {
      var data = JSON.parse(event.data);
      Shiny.setInputValue("ws_message", data, {priority: "event"});
    };
    
    ws.onclose = function() {
      console.log("WebSocket disconnected");
      Shiny.setInputValue("ws_connected", false);
    };
    
    // Store WebSocket for sending messages
    window.simulationWS = ws;
  });
  
  Shiny.addCustomMessageHandler("simulation_update", function(msg) {
    // Update progress bar
    if (msg.progress !== undefined) {
      $("#simulation-progress").css("width", msg.progress + "%");
      $("#simulation-progress-text").text(msg.progress + "%");
    }
    
    // Update status
    if (msg.status) {
      $("#simulation-status").text(msg.status);
    }
    
    // Update current stats
    if (msg.current_stats) {
      $("#current-trophies").text(msg.current_stats.trophies);
      $("#current-gold").text(msg.current_stats.gold);
      $("#current-power").text(msg.current_stats.power);
    }
  });
  '
  
  session$sendCustomMessage("eval", js_code)
}