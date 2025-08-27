library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(DT)
library(stringr)
library(jsonlite)
library(digest)

`%||%` <- function(a,b) if (is.null(a)) b else a

# Small helper for Bootstrap 5 tooltips on info icons
help_icon <- function(text) {
  tagList(tags$span(class = "ms-1 text-muted",
                    `data-bs-toggle` = "tooltip", title = text,
                    shiny::icon("circle-info")))
}

# Bind to the README's port if not set by user
if (is.null(getOption("shiny.port"))) options(shiny.port = 7789)
if (is.null(getOption("shiny.host"))) options(shiny.host = "127.0.0.1")

source("R/sim_config.R")
source("R/lucky_star.R")
source("R/sim_economy.R")
source("R/plots.R")
source("R/config_store.R")

# Custom theme with modern gaming aesthetic
custom_theme <- bs_theme(
  version = 5,
  bg = "#f8f9fa",
  fg = "#212529",
  primary = "#5865F2",
  secondary = "#EB459E",
  success = "#57F287",
  info = "#00D9FF",
  warning = "#FEE75C",
  danger = "#ED4245",
  font_scale = 1.0,
  `enable-rounded` = TRUE
)

ui <- page_fluid(
  theme = custom_theme,
  tags$head(
    tags$title("Clash Economy Simulator"),
    tags$style(HTML("
      body { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); min-height: 100vh; }
      .main-container { background: rgba(255,255,255,0.95); border-radius: 12px; padding: 20px; margin: 20px auto; max-width: 1400px; box-shadow: 0 10px 40px rgba(0,0,0,0.1); }
      .sidebar-card { background: white; border-radius: 8px; padding: 12px; margin-bottom: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
      .sidebar-card .form-group { margin-bottom: 6px; }
      .sidebar-card hr { margin: 8px 0; }
      .sidebar-header { font-weight: 600; color: #5865F2; margin-bottom: 10px; padding-bottom: 8px; border-bottom: 2px solid #5865F2; }
      .btn-run { background: linear-gradient(90deg, #5865F2 0%, #764ba2 100%); border: none; font-weight: 600; padding: 12px; transition: all 0.3s; }
      .btn-run:hover { transform: translateY(-2px); box-shadow: 0 5px 15px rgba(88,101,242,0.4); }
      .nav-pills .nav-link { border-radius: 8px; margin: 0 5px; font-weight: 500; }
      .nav-pills .nav-link.active { background: linear-gradient(90deg, #5865F2 0%, #764ba2 100%); }
      .table-container { background: white; border-radius: 8px; padding: 15px; }
      .plot-container { background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.05); }
      .metric-card { background: white; border-radius: 8px; padding: 20px; text-align: center; box-shadow: 0 2px 8px rgba(0,0,0,0.05); }
      .metric-value { font-size: 2em; font-weight: 700; color: #5865F2; }
      .metric-label { color: #6c757d; font-size: 0.9em; margin-top: 5px; }
      .thin-sep { border: 0; border-top: 1px solid #e5e7eb; margin: 12px 0 16px 0; }
    "))
    ,
    # Initialize Bootstrap tooltips
    tags$script(HTML("document.addEventListener('DOMContentLoaded', function(){var tt=[].slice.call(document.querySelectorAll('[data-bs-toggle=\\"tooltip\\"]')); tt.map(function(el){return new bootstrap.Tooltip(el);});});"))
  ),
  
  div(class = "main-container",
    h1("Clash Economy Simulator", style = "text-align: center; margin-bottom: 30px; color: #5865F2;"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        class = "sidebar-content",
        
        div(class = "sidebar-card",
          h4(class = "sidebar-header", "Data & View"),
          selectInput("data_version",
            label = "Data Version",
            choices = c("Base"), 
            selected = "Base",
            width = "100%"
          ),
          checkboxInput("use_cache", "Use cached results (if unchanged)", value = TRUE),
          uiOutput('cache_status'),
          hr(),
          span("Day Range", style = "font-weight: 500; color: #495057;"),
          sliderInput("day_range", 
            label = NULL,
            min = 1, max = 360, 
            value = c(1, 180),
            width = "100%"
          ),
          checkboxInput("show_smooth",
            label = "Smooth lines (LOESS)",
            value = FALSE
          )
        )
      ),
      
      navset_pill(
        id = "tabs",
        
        nav_panel("Daily Table",
          br(),
          textOutput("range_label_table"),
          br(),
          div(class = "table-container",
            DTOutput("tbl_daily")
          ),
          br(),
          h4("Upgrade Efficiency Summary", style = "color: #5865F2;"),
          div(class = "table-container",
            DTOutput("tbl_upg_summary")
          )
        ),
        
        nav_panel("Visualizations",
          br(),
          fluidRow(
            column(12,
              actionButton("export_pngs", label = tagList(icon("download"), " Save All Charts (PNG)"), class = "btn btn-secondary", width = "100%")
            )
          ),
          br(),
          textOutput("range_label"),
          br(),
          fluidRow(
            column(4,
              div(class = "metric-card",
                div(class = "metric-value", textOutput("metric_trophies")),
                div(class = "metric-label", "Final Trophies")
              )
            ),
            column(4,
              div(class = "metric-card",
                div(class = "metric-value", textOutput("metric_gold")),
                div(class = "metric-label", "Total Gold Earned")
              )
            ),
            column(4,
              div(class = "metric-card",
                div(class = "metric-value", textOutput("metric_winrate")),
                div(class = "metric-label", "Average Win Rate")
              )
            )
          ),
          tags$hr(class = "thin-sep"),
          br(),
          div(class = "plot-container", plotOutput("plt_trophies")),
          div(class = "plot-container", plotOutput("plt_gold")),
          div(class = "plot-container", plotOutput("plt_winrate")),
          div(class = "plot-container", plotOutput("plt_boxmix")),
          div(class = "plot-container", plotOutput("plt_spend_rarity")),
          div(class = "plot-container", plotOutput("plt_power_rarity")),
          div(class = "plot-container", plotOutput("plt_eff_daily")),
          div(class = "plot-container", plotOutput("plt_eff_cum")),
          div(class = "plot-container", plotOutput("plt_pass")),
          div(class = "plot-container", plotOutput("plt_trophies_drift"))
        ),
        
        nav_panel("Compare",
          br(),
          fluidRow(
            column(6,
              selectInput("cmp_select", 
                "Select Configs to Compare",
                choices = names(load_configs()),
                multiple = TRUE,
                width = "100%"
              )
            ),
            column(6,
              br(),
              actionButton("cmp_run",
                "Run Comparison",
                class = "btn btn-primary",
                width = "100%"
              )
            )
          ),
          br(),
          div(class = "plot-container", plotOutput("cmp_trophies")),
          div(class = "plot-container", plotOutput("cmp_gold")),
          div(class = "plot-container", plotOutput("cmp_winrate")),
          br(),
          h4("Upgrade Efficiency Comparison", style = "color: #5865F2;"),
          div(class = "table-container", DTOutput("cmp_upg_summary"))
        ),
        
        
        
        nav_panel("Data Editor",
          br(),
          fluidRow(
            column(6,
              selectInput("edit_table",
                "Select Table to Edit",
                choices = c(
                  "Lucky Star: Upgrade per spin" = "table_2_1_lucky_drop_upgrade_probabilities_per_spin.csv",
                  "Lucky Star: Final rewards (Arena 16+)" = "table_2_2_lucky_drop_final_reward_probabilities_by_final_rarity_arena_16.csv",
                  "Upgrades: Common" = "card_upgrade_costs_common.csv",
                  "Upgrades: Rare" = "card_upgrade_costs_rare.csv",
                  "Upgrades: Epic" = "card_upgrade_costs_epic.csv",
                  "Upgrades: Legendary" = "card_upgrade_costs_legendary.csv",
                  "Upgrades: Champion" = "card_upgrade_costs_champion.csv"
                ),
                width = "100%"
              )
            ),
            column(6,
              textInput("edit_version",
                "Save to Version",
                placeholder = "my-economy-v1",
                width = "100%"
              ),
              br(),
              actionButton("edit_save",
                "Save Table",
                class = "btn btn-success",
                width = "100%"
              )
            )
          ),
          br(),
          div(class = "table-container", DTOutput("edit_table_view"))
        ),
        
        nav_panel("Assumptions",
          br(),
          fluidRow(
            column(6,
              selectInput("cfg_select",
                "Load Saved Config",
                choices = names(load_configs()),
                selected = "Defaults",
                width = "100%"
              ),
              br(),
              actionButton("cfg_apply",
                "Load Selected Config",
                class = "btn btn-info",
                width = "100%"
              )
            ),
            column(6,
              textInput("cfg_name",
                "Save Config As",
                placeholder = "MyConfig",
                width = "100%"
              ),
              br(),
              actionButton("cfg_save",
                "Save Current Config",
                class = "btn btn-success",
                width = "100%"
              )
            )
          ),
          br(),
          actionButton("cfg_reset", label = tagList(icon('undo'), ' Reset to Defaults'), class = 'btn btn-light'),
          br(),
          accordion(open = if (file.exists('runs/last_config.json')) character(0) else 'Core Settings', multiple = TRUE,
            accordion_panel(tagList(icon('cog'), ' Core Settings'),
              fluidRow(
            column(4,
              numericInput("cfg_seed", label = tagList("Seed", help_icon("Used for reproducible randomness.")), value = 42, min = 1),
              tags$small(class = "text-muted", "Random seed for reproducible results.")
            ),
            column(4,
              numericInput("cfg_matches", label = tagList("Matches/day", help_icon("Daily match count (fixed). Impacts pacing of progress.")), value = 15, min = 0, step = 1),
              tags$small(class = "text-muted", "Fixed number of matches played per day.")
            ),
            column(4,
              numericInput("cfg_base_wr", label = tagList("Base win rate", help_icon("Win chance vs equal opponents before bots/power. Example: 0.10 = 10%.")), value = 0.5, min = 0, max = 1, step = 0.01),
              tags$small(class = "text-muted", "Win chance vs. equal opponents (before bots/power).")
            )
          ),
          fluidRow(
            column(4,
              numericInput("cfg_vbonus", label = tagList("Vertical bonus", help_icon("Additive win-rate bonus from progression/power. Example: 0.05 = +5%.")), value = 0.0, min = -1, max = 1, step = 0.01),
              tags$small(class = "text-muted", "Additive win-rate bonus from progression/power.")
            ),
            column(4,
              numericInput("cfg_bot_rate", label = tagList("Bot rate", help_icon("Share of matches vs bots. Example: 0.10 = 10%.")), value = 0.1, min = 0, max = 1, step = 0.01),
              tags$small(class = "text-muted", "Share of matches vs. bots.")
            ),
            column(4,
              numericInput("cfg_bot_wr", label = tagList("Bot win rate", help_icon("Your win chance against a bot. Example: 0.90 = 90%.")), value = 0.9, min = 0, max = 1, step = 0.01),
              tags$small(class = "text-muted", "Your win chance against a bot.")
            )
          ),
          fluidRow(
            column(4,
              numericInput("cfg_trophy_win", label = tagList("Trophies on win", help_icon("Trophies gained on a win. Typical values ~30.")), value = 30, min = 0),
              tags$small(class = "text-muted", "Trophies gained on a win.")
            ),
            column(4,
              numericInput("cfg_trophy_loss", label = tagList("Trophies on loss", help_icon("Trophies lost on a defeat. Typical values around −29.")), value = -29, max = 0),
              tags$small(class = "text-muted", "Trophies lost on a defeat.")
            ),
            column(4,
              numericInput("cfg_gold_min", label = tagList("Gold min (per win)", help_icon("Lower bound for random gold per win. Displayed with thousands separators (e.g., 12,500).")), value = 10, min = 0),
              tags$small(class = "text-muted", "Lower bound for random gold per win.")
            )
          ),
          fluidRow(
            column(4,
              numericInput("cfg_gold_max", label = tagList("Gold max (per win)", help_icon("Upper bound for random gold per win. Displayed with thousands separators (e.g., 25,000).")), value = 10, min = 0),
              tags$small(class = "text-muted", "Upper bound for random gold per win.")
            )
          )
        ),
        accordion_panel(tagList(icon('award'), ' Pass Settings'),
          fluidRow(
            column(6,
              numericInput("cfg_pass_crowns", label = tagList("Crowns per level", help_icon("Crowns required to advance one Pass level.")), value = 20, min = 1)
            ),
            column(6,
              numericInput("cfg_pass_gold", label = tagList("Gold per level", help_icon("Gold awarded at each Pass level. Displayed with thousands separators.")), value = 50, min = 0)
            )
          )
        ),
        accordion_panel(tagList(icon('chess'), ' Crown Distribution'),
          fluidRow(
            column(6,
              h5(tagList("On Win", help_icon("Probability of getting 0–3 crowns when you win. Sums to 100%.")), style = "color: #57F287;"),
              numericInput("cfg_crown_win_0", "0 crowns", value = 0.0, min = 0, max = 1, step = 0.01),
              numericInput("cfg_crown_win_1", "1 crown", value = 0.2, min = 0, max = 1, step = 0.01),
              numericInput("cfg_crown_win_2", "2 crowns", value = 0.5, min = 0, max = 1, step = 0.01),
              numericInput("cfg_crown_win_3", "3 crowns", value = 0.3, min = 0, max = 1, step = 0.01)
            ),
            column(6,
              h5(tagList("On Loss", help_icon("Probability of getting 0–3 crowns when you lose. Sums to 100%.")), style = "color: #ED4245;"),
              numericInput("cfg_crown_loss_0", "0 crowns", value = 0.4, min = 0, max = 1, step = 0.01),
              numericInput("cfg_crown_loss_1", "1 crown", value = 0.4, min = 0, max = 1, step = 0.01),
              numericInput("cfg_crown_loss_2", "2 crowns", value = 0.2, min = 0, max = 1, step = 0.01),
              numericInput("cfg_crown_loss_3", "3 crowns", value = 0.0, min = 0, max = 1, step = 0.01)
            )
          ),
          tags$small(class = 'text-muted', "Use decimals (0.0–1.0). Sums normalize automatically.")
        )
          ),
          br(),
          uiOutput("assumptions_warnings")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Initial run
  vals <- reactiveValues(
    daily_base = NULL,
    cmp_results = list(),
    meta = list(status = "", time = NA_character_)
  )
  
  # Load last run config if present
  observe({
    p <- file.path('runs','last_config.json')
    if (file.exists(p)) {
      lr <- tryCatch(jsonlite::read_json(p, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(lr) && !is.null(lr$cfg)) {
        cfg <- lr$cfg
        updateNumericInput(session, "cfg_seed", value = lr$seed %||% 42)
        updateNumericInput(session, "cfg_matches", value = cfg$matches_per_day %||% 15)
        updateNumericInput(session, "cfg_base_wr", value = cfg$base_winrate %||% 0.5)
        updateNumericInput(session, "cfg_vbonus", value = cfg$vertical_winrate_bonus %||% 0.0)
        updateNumericInput(session, "cfg_bot_rate", value = cfg$bot_rate %||% 0.1)
        updateNumericInput(session, "cfg_bot_wr", value = cfg$bot_winrate %||% 0.9)
        updateNumericInput(session, "cfg_trophy_win", value = cfg$trophies_on_win %||% 30)
        updateNumericInput(session, "cfg_trophy_loss", value = cfg$trophies_on_loss %||% -29)
        updateNumericInput(session, "cfg_gold_min", value = cfg$gold_drop$min_amount %||% 10)
        updateNumericInput(session, "cfg_gold_max", value = cfg$gold_drop$max_amount %||% 10)
        updateNumericInput(session, "cfg_pass_crowns", value = cfg$pass_config$crowns_per_level %||% 20)
        updateNumericInput(session, "cfg_pass_gold", value = cfg$pass_config$gold_per_level %||% 50)
        if (!is.null(cfg$crown_dist)) {
          updateNumericInput(session, "cfg_crown_win_0", value = cfg$crown_dist$on_win[["0"]] %||% 0)
          updateNumericInput(session, "cfg_crown_win_1", value = cfg$crown_dist$on_win[["1"]] %||% 0)
          updateNumericInput(session, "cfg_crown_win_2", value = cfg$crown_dist$on_win[["2"]] %||% 0)
          updateNumericInput(session, "cfg_crown_win_3", value = cfg$crown_dist$on_win[["3"]] %||% 0)
          updateNumericInput(session, "cfg_crown_loss_0", value = cfg$crown_dist$on_loss[["0"]] %||% 0)
          updateNumericInput(session, "cfg_crown_loss_1", value = cfg$crown_dist$on_loss[["1"]] %||% 0)
          updateNumericInput(session, "cfg_crown_loss_2", value = cfg$crown_dist$on_loss[["2"]] %||% 0)
          updateNumericInput(session, "cfg_crown_loss_3", value = cfg$crown_dist$on_loss[["3"]] %||% 0)
        }
      }
    }
  })
  
  # Build config from Assumptions inputs
  build_cfg <- reactive({
    cfg <- default_config(days = 360, matches_per_day = input$cfg_matches %||% 15)
    cfg$base_winrate <- input$cfg_base_wr %||% 0.5
    cfg$vertical_winrate_bonus <- input$cfg_vbonus %||% 0.0
    cfg$bot_rate <- input$cfg_bot_rate %||% 0.1
    cfg$bot_winrate <- input$cfg_bot_wr %||% 0.9
    cfg$trophies_on_win <- input$cfg_trophy_win %||% 30
    cfg$trophies_on_loss <- input$cfg_trophy_loss %||% -29
    cfg$gold_drop$fixed_amount <- NA_integer_
    cfg$gold_drop$min_amount <- input$cfg_gold_min %||% 10
    cfg$gold_drop$max_amount <- input$cfg_gold_max %||% 10
    # Normalize crown PMFs defensively to sum to 1
    w_win <- c(`0`=input$cfg_crown_win_0 %||% 0, `1`=input$cfg_crown_win_1 %||% 0.2, `2`=input$cfg_crown_win_2 %||% 0.5, `3`=input$cfg_crown_win_3 %||% 0.3)
    w_loss <- c(`0`=input$cfg_crown_loss_0 %||% 0.4, `1`=input$cfg_crown_loss_1 %||% 0.4, `2`=input$cfg_crown_loss_2 %||% 0.2, `3`=input$cfg_crown_loss_3 %||% 0)
    sw <- sum(w_win); sl <- sum(w_loss)
    if (is.finite(sw) && sw > 0) w_win <- w_win / sw
    if (is.finite(sl) && sl > 0) w_loss <- w_loss / sl
    cfg$crown_dist$on_win <- w_win
    cfg$crown_dist$on_loss <- w_loss
    cfg$pass_config$crowns_per_level <- input$cfg_pass_crowns %||% 20
    cfg$pass_config$gold_per_level <- input$cfg_pass_gold %||% 50
    cfg
  })

  # Validation banner for key inputs
  output$assumptions_warnings <- renderUI({
    ws <- c()
    b <- input$cfg_base_wr %||% 0.5
    brt <- input$cfg_bot_rate %||% 0.1
    bwr <- input$cfg_bot_wr %||% 0.9
    if (is.null(b) || b < 0 || b > 1) ws <- c(ws, "Base win rate must be in [0,1].")
    if (is.null(brt) || brt < 0 || brt > 1) ws <- c(ws, "Bot rate must be in [0,1].")
    if (is.null(bwr) || bwr < 0 || bwr > 1) ws <- c(ws, "Bot win rate must be in [0,1].")
    if ((input$cfg_trophy_win %||% 30) <= 0) ws <- c(ws, "Trophies on win should be > 0.")
    if ((input$cfg_trophy_loss %||% -29) >= 0) ws <- c(ws, "Trophies on loss should be < 0.")
    if ((input$cfg_gold_min %||% 10) > (input$cfg_gold_max %||% 10)) ws <- c(ws, "Gold min cannot exceed max.")
    sw <- sum(c(input$cfg_crown_win_0 %||% 0, input$cfg_crown_win_1 %||% 0, input$cfg_crown_win_2 %||% 0, input$cfg_crown_win_3 %||% 0))
    sl <- sum(c(input$cfg_crown_loss_0 %||% 0, input$cfg_crown_loss_1 %||% 0, input$cfg_crown_loss_2 %||% 0, input$cfg_crown_loss_3 %||% 0))
    if (abs(sw - 1) > 1e-6) ws <- c(ws, sprintf("Win crown probabilities sum to %.3f, not 1.000.", sw))
    if (abs(sl - 1) > 1e-6) ws <- c(ws, sprintf("Loss crown probabilities sum to %.3f, not 1.000.", sl))
    if (length(ws) == 0) {
      div(class = "alert alert-success", tags$b("Inputs look good"))
    } else {
      div(class = "alert alert-warning",
          tags$b("Please review settings"),
          tags$ul(lapply(ws, tags$li)))
    }
  })

  # Simple disk cache by cfg+seed hash
  cache_dir <- normalizePath(file.path('runs','cache'), mustWork = FALSE)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_key <- reactive({
    json <- jsonlite::toJSON(build_cfg(), auto_unbox = TRUE)
    digest::digest(list(json, input$cfg_seed %||% 42, input$data_version %||% 'Base'))
  })
  # Fallback loader from included JSON daily if needed
  load_default_daily <- function(){
    cand <- c(
      'runs/522a576248c3/1/daily.json',
      'runs/1620f947eb91/1/daily.json'
    )
    path <- cand[file.exists(cand)][1]
    if (!is.na(path) && nzchar(path)) {
      df <- jsonlite::fromJSON(path, simplifyVector = TRUE)
      as_tibble(df)
    } else NULL
  }
  load_cached <- function(key){
    p <- file.path(cache_dir, paste0(key, '.rds'))
    if (file.exists(p)) readRDS(p) else NULL
  }
  save_cached <- function(key, obj){
    p <- file.path(cache_dir, paste0(key, '.rds'))
    saveRDS(obj, p)
  }

  # Reactive simulation with optional caching
  observe({
    cfg <- build_cfg(); sd <- input$cfg_seed %||% 42
    key <- cache_key()
    res <- if (isTRUE(input$use_cache)) load_cached(key) else NULL
    if (!is.null(res)) {
      vals$meta <- list(status = "Loaded from cache", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    } else {
      # try to compute; on error, use default JSON and still cache
      res <- tryCatch({
        withProgress(message = 'Simulating season...', value = 0, {
          # simple single-step progress
          incProgress(0.3)
          out <- simulate_season(cfg, seed = sd)
          incProgress(0.7)
          out
        })
      }, error = function(e) NULL)
      if (is.null(res)) {
        df <- load_default_daily()
        if (!is.null(df)) {
          res <- list(daily = df, state = list())
        }
      }
      if (!is.null(res)) {
        save_cached(key, res)
        write_json(list(cfg = cfg, seed = sd), file.path('runs','last_config.json'), auto_unbox = TRUE, pretty = TRUE)
        vals$meta <- list(status = if (isTRUE(input$use_cache)) "Computed and cached" else "Computed", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      }
    }
    req(res)
    vals$daily_base <- res$daily %>% mutate(data_version = input$data_version)
  })

  # Cache/compute status in sidebar
  output$cache_status <- renderUI({
    m <- vals$meta
    if (!is.null(m$status) && nzchar(m$status)) {
      div(class = 'alert alert-secondary', style = 'margin-top:8px; padding:6px 10px; font-size:12px;',
          tags$b(m$status), tags$span(paste0(' @ ', m$time)))
    } else NULL
  })
  
  # Add metric outputs
  output$metric_trophies <- renderText({
    req(vals$daily_base)
    df <- vals$daily_base %>%
      filter(day >= input$day_range[1] & day <= input$day_range[2])
    if(nrow(df) > 0) {
      format(round(tail(df$trophies_end, 1)), big.mark = ",")
    } else {
      "0"
    }
  })
  
  output$metric_gold <- renderText({
    req(vals$daily_base)
    df <- vals$daily_base %>%
      filter(day >= input$day_range[1] & day <= input$day_range[2])
    if(nrow(df) > 0) {
      format(round(sum(df$gold_total)), big.mark = ",")
    } else {
      "0"
    }
  })
  
  output$metric_winrate <- renderText({
    req(vals$daily_base)
    df <- vals$daily_base %>%
      filter(day >= input$day_range[1] & day <= input$day_range[2])
    if(nrow(df) > 0) {
      paste0(round(mean(df$winrate) * 100, 1), "%")
    } else {
      "0%"
    }
  })
  
  # Daily table
  output$tbl_daily <- renderDT({
    req(vals$daily_base)
    df <- vals$daily_base %>%
      filter(day >= input$day_range[1] & day <= input$day_range[2]) %>%
      select(-data_version)
    
    datatable(df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      class = 'table-striped table-hover'
    ) %>%
      formatRound(columns = c("winrate", "power_bonus"), digits = 3) %>%
      formatCurrency(columns = c("gold_total", "gold_from_matches", "gold_from_pass", "gold_spent_upgrades"), currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  # Range label above visuals
  output$range_label <- renderText({
    paste0("Current range: Days ", input$day_range[1], "–", input$day_range[2])
  })
  output$range_label_table <- renderText({
    paste0("Current range: Days ", input$day_range[1], "–", input$day_range[2])
  })
  
  # Upgrade summary
  output$tbl_upg_summary <- renderDT({
    req(vals$daily_base)
    df <- vals$daily_base %>%
      filter(day >= input$day_range[1] & day <= input$day_range[2])
    
    if (nrow(df) > 0 && "gold_spent_upgrades" %in% names(df) && 
        "upgrades_applied" %in% names(df)) {
      summary <- data.frame(
        Metric = c("Total Gold Spent", "Total Upgrades", "Avg Gold per Upgrade"),
        Value = c(
          sum(df$gold_spent_upgrades, na.rm = TRUE),
          sum(df$upgrades_applied, na.rm = TRUE),
          ifelse(sum(df$upgrades_applied, na.rm = TRUE) > 0,
                 round(sum(df$gold_spent_upgrades, na.rm = TRUE) / 
                       sum(df$upgrades_applied, na.rm = TRUE), 1),
                 0)
        )
      )
    datatable(summary, 
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE,
      class = 'table-striped'
    ) %>%
        formatCurrency(columns = "Value", currency = "", interval = 3, mark = ",", digits = 0)
    }
  })
  
  # Plots
  output$plt_trophies <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    p <- plot_trophies(df)
    if (isTRUE(input$show_smooth)) p <- p + geom_smooth(se = FALSE, method = 'loess', span = 0.3, color = '#94a3b8')
    p
  })
  
  output$plt_gold <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plot_gold_flow(df)
  })
  
  output$plt_winrate <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    p <- plot_winrate(df)
    if (isTRUE(input$show_smooth)) p <- p + geom_smooth(se = FALSE, method = 'loess', span = 0.3, color = '#94a3b8')
    p
  })
  
  output$plt_boxmix <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plot_box_rarity_mix(df)
  })
  
  output$plt_spend_rarity <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plot_spend_by_rarity_range(df)
  })
  
  output$plt_power_rarity <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plot_power_by_rarity_range(df)
  })
  
  output$plt_eff_daily <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    p <- plot_upgrade_efficiency_daily(df)
    if (isTRUE(input$show_smooth)) p <- p + geom_smooth(se = FALSE, method = 'loess', span = 0.3, color = '#16a34a')
    p
  })
  
  output$plt_eff_cum <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    p <- plot_upgrade_efficiency_cum(df)
    if (isTRUE(input$show_smooth)) p <- p + geom_smooth(se = FALSE, method = 'loess', span = 0.3, color = '#065f46')
    p
  })
  
  output$plt_pass <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plot_pass_progress(df)
  })
  
  output$plt_trophies_drift <- renderPlot({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plot_trophies_drift(df)
  })

  # Export all current charts to PNG files under runs/screenshots
  observeEvent(input$export_pngs, {
    req(vals$daily_base)
    out_dir <- normalizePath(file.path('runs','screenshots'), mustWork = FALSE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    save_plot <- function(plot_obj, filename, width=10, height=6, dpi=144){
      if (is.null(plot_obj)) return(invisible(NULL))
      try(ggplot2::ggsave(filename = file.path(out_dir, filename), plot = plot_obj, width = width, height = height, dpi = dpi), silent = TRUE)
    }
    save_plot(plot_trophies(df), 'trophies.png')
    save_plot(plot_gold_flow(df), 'gold_flow.png')
    save_plot(plot_winrate(df), 'winrate.png')
    save_plot(plot_box_rarity_mix(df), 'box_rarity_mix.png')
    save_plot(plot_spend_by_rarity_range(df), 'spend_by_rarity.png')
    save_plot(plot_power_by_rarity_range(df), 'power_by_rarity.png')
    save_plot(plot_upgrade_efficiency_daily(df), 'upgrade_efficiency_daily.png')
    save_plot(plot_upgrade_efficiency_cum(df), 'upgrade_efficiency_cum.png')
    save_plot(plot_pass_progress(df), 'pass_progress.png')
    save_plot(plot_trophies_drift(df), 'trophies_drift.png')
    showNotification(sprintf('Saved charts to %s', out_dir), type = 'message')
  })
  
  # (Research tab removed)
  
  # Comparison
  observeEvent(input$cmp_run, {
    req(input$cmp_select)
    configs <- load_configs()
    results <- list()
    
    for (cfg_name in input$cmp_select) {
      if (cfg_name %in% names(configs)) {
        cfg <- configs[[cfg_name]]
        result <- simulate_season(cfg, seed = input$cfg_seed %||% 42)
        results[[cfg_name]] <- result$daily %>%
          mutate(config = cfg_name)
      }
    }
    vals$cmp_results <- results
  })
  
  output$cmp_trophies <- renderPlot({
    req(length(vals$cmp_results) > 0)
    df <- bind_rows(vals$cmp_results)
    dd <- df %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplot(dd, aes(day, trophies_end, color = config)) + geom_line() +
      labs(title = "Trophies Comparison", x = "Day", y = "Trophies") + theme_minimal(base_size = 12)
  })
  
  output$cmp_gold <- renderPlot({
    req(length(vals$cmp_results) > 0)
    df <- bind_rows(vals$cmp_results)
    dd <- df %>% filter(day >= input$day_range[1] & day <= input$day_range[2]) %>%
      group_by(config) %>% arrange(day, .by_group = TRUE) %>% mutate(cum_gold = cumsum(gold_total))
    ggplot(dd, aes(day, cum_gold, color = config)) + geom_line() +
      labs(title = "Cumulative Gold Comparison", x = "Day", y = "Gold (cumulative)") + theme_minimal(base_size = 12)
  })
  
  output$cmp_winrate <- renderPlot({
    req(length(vals$cmp_results) > 0)
    df <- bind_rows(vals$cmp_results)
    dd <- df %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplot(dd, aes(day, winrate*100, color = config)) + geom_line() +
      labs(title = "Winrate Comparison", x = "Day", y = "Winrate (%)") + theme_minimal(base_size = 12)
  })
  
  output$cmp_upg_summary <- renderDT({
    req(length(vals$cmp_results) > 0)
    
    summary_list <- lapply(names(vals$cmp_results), function(cfg_name) {
      df <- vals$cmp_results[[cfg_name]]
      data.frame(
        Config = cfg_name,
        `Gold Spent` = sum(df$gold_spent_upgrades, na.rm = TRUE),
        Upgrades = sum(df$upgrades_applied, na.rm = TRUE),
        `Avg Gold/Upgrade` = ifelse(
          sum(df$upgrades_applied, na.rm = TRUE) > 0,
          round(sum(df$gold_spent_upgrades, na.rm = TRUE) / 
                sum(df$upgrades_applied, na.rm = TRUE), 1),
          0
        ),
        `Final Trophies` = tail(df$trophies_end, 1),
        `Final Power` = tail(df$power_total, 1),
        check.names = FALSE
      )
    })
    
    summary_df <- bind_rows(summary_list)
    
    datatable(summary_df, 
      options = list(pageLength = 10),
      rownames = FALSE,
      class = 'table-striped table-hover'
    ) %>%
      formatCurrency(columns = c("Gold Spent"), currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c("Upgrades", "Final Trophies", "Final Power"), digits = 0) %>%
      formatRound(columns = "Avg Gold/Upgrade", digits = 1)
  })
  
  # Data Editor
  output$edit_table_view <- renderDT({
    req(input$edit_table)
    tables_dir()
    path <- file.path(tables_dir(), input$edit_table)
    
    if (file.exists(path)) {
      df <- read_csv(path, show_col_types = FALSE)
      datatable(df, 
        editable = TRUE, 
        options = list(pageLength = 15),
        class = 'table-striped'
      )
    }
  })
  
  # Config management
  observeEvent(input$cfg_apply, {
    req(input$cfg_select)
    configs <- load_configs()
    if (input$cfg_select %in% names(configs)) {
      cfg <- configs[[input$cfg_select]]
      updateNumericInput(session, "cfg_matches", value = cfg$matches_per_day)
      updateNumericInput(session, "cfg_base_wr", value = cfg$base_winrate)
      updateNumericInput(session, "cfg_vbonus", value = cfg$vertical_winrate_bonus)
      updateNumericInput(session, "cfg_bot_rate", value = cfg$bot_rate)
      updateNumericInput(session, "cfg_bot_wr", value = cfg$bot_winrate)
      updateNumericInput(session, "cfg_trophy_win", value = cfg$trophies_on_win)
      updateNumericInput(session, "cfg_trophy_loss", value = cfg$trophies_on_loss)
      updateNumericInput(session, "cfg_gold_min", value = cfg$gold_drop$min_amount)
      updateNumericInput(session, "cfg_gold_max", value = cfg$gold_drop$max_amount)
      updateNumericInput(session, "cfg_pass_crowns", value = cfg$pass_config$crowns_per_level)
      updateNumericInput(session, "cfg_pass_gold", value = cfg$pass_config$gold_per_level)
      
      # Update crown distributions
      updateNumericInput(session, "cfg_crown_win_0", value = cfg$crown_dist$on_win[["0"]])
      updateNumericInput(session, "cfg_crown_win_1", value = cfg$crown_dist$on_win[["1"]])
      updateNumericInput(session, "cfg_crown_win_2", value = cfg$crown_dist$on_win[["2"]])
      updateNumericInput(session, "cfg_crown_win_3", value = cfg$crown_dist$on_win[["3"]])
      updateNumericInput(session, "cfg_crown_loss_0", value = cfg$crown_dist$on_loss[["0"]])
      updateNumericInput(session, "cfg_crown_loss_1", value = cfg$crown_dist$on_loss[["1"]])
      updateNumericInput(session, "cfg_crown_loss_2", value = cfg$crown_dist$on_loss[["2"]])
      updateNumericInput(session, "cfg_crown_loss_3", value = cfg$crown_dist$on_loss[["3"]])
    }
  })
  
  # Reset to defaults
  observeEvent(input$cfg_reset, {
    cfg <- default_config(days = 360, matches_per_day = 15)
    updateNumericInput(session, "cfg_seed", value = 42)
    updateNumericInput(session, "cfg_matches", value = cfg$matches_per_day)
    updateNumericInput(session, "cfg_base_wr", value = cfg$base_winrate)
    updateNumericInput(session, "cfg_vbonus", value = cfg$vertical_winrate_bonus)
    updateNumericInput(session, "cfg_bot_rate", value = cfg$bot_rate)
    updateNumericInput(session, "cfg_bot_wr", value = cfg$bot_winrate)
    updateNumericInput(session, "cfg_trophy_win", value = cfg$trophies_on_win)
    updateNumericInput(session, "cfg_trophy_loss", value = cfg$trophies_on_loss)
    updateNumericInput(session, "cfg_gold_min", value = cfg$gold_drop$min_amount)
    updateNumericInput(session, "cfg_gold_max", value = cfg$gold_drop$max_amount)
    updateNumericInput(session, "cfg_pass_crowns", value = cfg$pass_config$crowns_per_level)
    updateNumericInput(session, "cfg_pass_gold", value = cfg$pass_config$gold_per_level)
    updateNumericInput(session, "cfg_crown_win_0", value = cfg$crown_dist$on_win[["0"]])
    updateNumericInput(session, "cfg_crown_win_1", value = cfg$crown_dist$on_win[["1"]])
    updateNumericInput(session, "cfg_crown_win_2", value = cfg$crown_dist$on_win[["2"]])
    updateNumericInput(session, "cfg_crown_win_3", value = cfg$crown_dist$on_win[["3"]])
    updateNumericInput(session, "cfg_crown_loss_0", value = cfg$crown_dist$on_loss[["0"]])
    updateNumericInput(session, "cfg_crown_loss_1", value = cfg$crown_dist$on_loss[["1"]])
    updateNumericInput(session, "cfg_crown_loss_2", value = cfg$crown_dist$on_loss[["2"]])
    updateNumericInput(session, "cfg_crown_loss_3", value = cfg$crown_dist$on_loss[["3"]])
  })
  
  observeEvent(input$cfg_save, {
    req(input$cfg_name)
    
    cfg <- default_config()
    cfg$matches_per_day <- input$cfg_matches
    cfg$base_winrate <- input$cfg_base_wr
    cfg$vertical_winrate_bonus <- input$cfg_vbonus
    cfg$bot_rate <- input$cfg_bot_rate
    cfg$bot_winrate <- input$cfg_bot_wr
    cfg$trophies_on_win <- input$cfg_trophy_win
    cfg$trophies_on_loss <- input$cfg_trophy_loss
    cfg$gold_drop$min_amount <- input$cfg_gold_min
    cfg$gold_drop$max_amount <- input$cfg_gold_max
    cfg$pass_config$crowns_per_level <- input$cfg_pass_crowns
    cfg$pass_config$gold_per_level <- input$cfg_pass_gold
    
    # Crown distributions
    cfg$crown_dist$on_win <- c(
      "0" = input$cfg_crown_win_0,
      "1" = input$cfg_crown_win_1,
      "2" = input$cfg_crown_win_2,
      "3" = input$cfg_crown_win_3
    )
    cfg$crown_dist$on_loss <- c(
      "0" = input$cfg_crown_loss_0,
      "1" = input$cfg_crown_loss_1,
      "2" = input$cfg_crown_loss_2,
      "3" = input$cfg_crown_loss_3
    )
    
    save_config(input$cfg_name, cfg)
    
    # Update dropdowns
    configs <- load_configs()
    updateSelectInput(session, "cfg_select", choices = names(configs))
    updateSelectInput(session, "cmp_select", choices = names(configs))
    
    # Show success message
    showNotification("Config saved successfully!", type = "success")
  })
}

shinyApp(ui, server)
