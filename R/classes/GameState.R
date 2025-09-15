#' @title GameState R6 Class
#' @description Manages the complete game state using R6 OOP
#' @import R6
#' @export

GameState <- R6::R6Class("GameState",
  public = list(
    #' @field trophies Current trophy count
    trophies = 0L,
    
    #' @field gold_bank Current gold balance
    gold_bank = 0L,
    
    #' @field power_score Total power score
    power_score = 0.0,
    
    #' @field account_points Total account points
    account_points = 0L,
    
    #' @field account_level Current account level
    account_level = 1L,
    
    #' @field tower_level Current tower level
    tower_level = 1L,
    
    #' @field pass_crowns Crowns earned for pass
    pass_crowns = 0L,
    
    #' @field pass_level Current pass level
    pass_level = 0L,
    
    #' @field daily_boxes_earned Mystery boxes earned today
    daily_boxes_earned = 0L,
    
    #' @field cards List of all cards with levels and copies
    cards = list(),
    
    #' @field inventory_by_rarity Card inventory grouped by rarity
    inventory_by_rarity = list(),
    
    #' @field inventory_by_card Card inventory by specific card
    inventory_by_card = list(),
    
    #' @field lucky_state Lucky star state
    lucky_state = NULL,
    
    #' @description Initialize a new GameState
    #' @param initial_trophies Starting trophy count
    #' @param initial_gold Starting gold amount
    initialize = function(initial_trophies = 0L, initial_gold = 0L) {
      self$trophies <- as.integer(initial_trophies)
      self$gold_bank <- as.integer(initial_gold)
      self$cards <- list()
      self$inventory_by_rarity <- list(
        common = 0L,
        rare = 0L, 
        epic = 0L,
        legendary = 0L
      )
      self$inventory_by_card <- list()
      self$lucky_state <- private$init_lucky_state()
      invisible(self)
    },
    
    #' @description Add gold to bank
    #' @param amount Gold amount to add
    add_gold = function(amount) {
      self$gold_bank <- self$gold_bank + as.integer(amount)
      invisible(self)
    },
    
    #' @description Spend gold from bank
    #' @param amount Gold amount to spend
    #' @return TRUE if successful, FALSE if insufficient funds
    spend_gold = function(amount) {
      amount <- as.integer(amount)
      if (self$gold_bank >= amount) {
        self$gold_bank <- self$gold_bank - amount
        return(TRUE)
      }
      return(FALSE)
    },
    
    #' @description Add trophies
    #' @param amount Trophy amount to add (can be negative)
    add_trophies = function(amount) {
      self$trophies <- max(0L, self$trophies + as.integer(amount))
      invisible(self)
    },
    
    #' @description Add power score
    #' @param amount Power to add
    add_power = function(amount) {
      self$power_score <- self$power_score + amount
      invisible(self)
    },
    
    #' @description Add account points and check for level up
    #' @param points Points to add
    #' @return TRUE if leveled up, FALSE otherwise
    add_account_points = function(points) {
      self$account_points <- self$account_points + as.integer(points)
      thresholds <- private$account_level_thresholds()
      new_level <- sum(self$account_points >= thresholds) + 1L
      
      if (new_level > self$account_level) {
        self$account_level <- new_level
        self$tower_level <- new_level
        return(TRUE)
      }
      return(FALSE)
    },
    
    #' @description Add crowns and check for pass level up
    #' @param crowns Number of crowns to add
    #' @param crowns_per_level Crowns needed per pass level
    #' @return Number of new pass levels gained
    add_crowns = function(crowns, crowns_per_level = 20L) {
      self$pass_crowns <- self$pass_crowns + as.integer(crowns)
      new_level <- floor(self$pass_crowns / crowns_per_level)
      levels_gained <- max(0L, new_level - self$pass_level)
      self$pass_level <- new_level
      return(levels_gained)
    },
    
    #' @description Record a mystery box earned
    #' @return TRUE if box was earned, FALSE if daily cap reached
    earn_mystery_box = function(daily_cap = 4L) {
      if (self$daily_boxes_earned < daily_cap) {
        self$daily_boxes_earned <- self$daily_boxes_earned + 1L
        return(TRUE)
      }
      return(FALSE)
    },
    
    #' @description Reset daily counters
    reset_daily = function() {
      self$daily_boxes_earned <- 0L
      invisible(self)
    },
    
    #' @description Add card to inventory
    #' @param card_name Card identifier
    #' @param rarity Card rarity
    #' @param quantity Number of copies to add
    add_card = function(card_name, rarity, quantity) {
      # Update rarity inventory
      rarity_lower <- tolower(rarity)
      if (is.null(self$inventory_by_rarity[[rarity_lower]])) {
        self$inventory_by_rarity[[rarity_lower]] <- 0L
      }
      self$inventory_by_rarity[[rarity_lower]] <- 
        self$inventory_by_rarity[[rarity_lower]] + as.integer(quantity)
      
      # Update card inventory
      if (is.null(self$inventory_by_card[[card_name]])) {
        self$inventory_by_card[[card_name]] <- 0L
      }
      self$inventory_by_card[[card_name]] <- 
        self$inventory_by_card[[card_name]] + as.integer(quantity)
      
      # Update card data
      if (is.null(self$cards[[card_name]])) {
        self$cards[[card_name]] <- list(
          name = card_name,
          rarity = rarity,
          level = 1L,
          copies = 0L
        )
      }
      self$cards[[card_name]]$copies <- 
        self$cards[[card_name]]$copies + as.integer(quantity)
      
      invisible(self)
    },
    
    #' @description Upgrade a card
    #' @param card_name Card to upgrade
    #' @param copies_cost Copies required
    #' @param gold_cost Gold required
    #' @param power_gain Power gained from upgrade
    #' @return TRUE if upgrade successful, FALSE otherwise
    upgrade_card = function(card_name, copies_cost, gold_cost, power_gain) {
      card <- self$cards[[card_name]]
      if (is.null(card)) return(FALSE)
      if (card$copies < copies_cost) return(FALSE)
      if (!self$spend_gold(gold_cost)) return(FALSE)
      
      card$copies <- card$copies - copies_cost
      card$level <- card$level + 1L
      self$cards[[card_name]] <- card
      self$add_power(power_gain)
      
      return(TRUE)
    },
    
    #' @description Get current arena based on trophies
    #' @param arena_schedule Arena configuration
    get_current_arena = function(arena_schedule) {
      arena_index <- max(0L, floor(self$trophies / 400))
      arena_index <- min(arena_index, nrow(arena_schedule) - 1L)
      return(arena_schedule[arena_index + 1L, ])
    },
    
    #' @description Clone the current state
    #' @return Deep copy of GameState
    clone_state = function() {
      self$clone(deep = TRUE)
    },
    
    #' @description Export state as list
    #' @return List representation of state
    as_list = function() {
      list(
        trophies = self$trophies,
        gold_bank = self$gold_bank,
        power_score = self$power_score,
        account_points = self$account_points,
        account_level = self$account_level,
        tower_level = self$tower_level,
        pass_crowns = self$pass_crowns,
        pass_level = self$pass_level,
        daily_boxes_earned = self$daily_boxes_earned,
        cards = self$cards,
        inventory_by_rarity = self$inventory_by_rarity,
        inventory_by_card = self$inventory_by_card,
        lucky_state = self$lucky_state
      )
    }
  ),
  
  private = list(
    #' @description Initialize lucky star state
    init_lucky_state = function() {
      list(
        spins_today = 0L,
        current_stars = 0L,
        wins_with_lucky = 0L
      )
    },
    
    #' @description Get account level thresholds
    account_level_thresholds = function() {
      c(0, 20, 50, 100, 200, 400, 800, 1600, 3200, 6400,
        12800, 25600, 51200, 102400, 204800)
    }
  )
)