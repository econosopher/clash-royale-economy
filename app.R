library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(DT)
library(stringr)
library(jsonlite)
library(digest)

`%||%` <- function(a,b) if (is.null(a)) b else a

# Optional: write stdout/stderr to a log file if CR_SHINY_LOG_FILE is set
log_file <- Sys.getenv('CR_SHINY_LOG_FILE', unset = '')
if (nzchar(log_file)) {
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  try({ sink(log_file, append = TRUE, split = TRUE); sink(log_file, append = TRUE, type = 'message') }, silent = TRUE)
}

# Data-version helpers for editable research tables
base_tables_dir <- function() normalizePath(file.path("assets","research","gemini","tables"), mustWork = FALSE)
custom_root_dir <- function() normalizePath(file.path("assets","research","custom"), mustWork = FALSE)
data_versions <- function() {
  root <- custom_root_dir()
  if (!dir.exists(root)) return(c("Base"))
  subs <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  subs <- subs[nzchar(subs)]
  if (length(subs) == 0) c("Base") else c("Base", sort(unique(subs)))
}
data_version_path <- function(ver) {
  if (identical(ver, "Base") || is.null(ver) || !nzchar(ver)) return(base_tables_dir())
  file.path(custom_root_dir(), ver, "tables")
}
ensure_version_dirs <- function(ver) {
  if (is.null(ver) || !nzchar(ver)) return(FALSE)
  dir.create(file.path(custom_root_dir(), ver, "tables"), recursive = TRUE, showWarnings = FALSE)
  TRUE
}

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
  bg = "#f4f6fb",
  fg = "#111827",
  primary = "#2563eb",
  secondary = "#475569",
  success = "#16a34a",
  info = "#0ea5e9",
  warning = "#f59e0b",
  danger = "#ef4444",
  font_scale = 1.0,
  `enable-rounded` = TRUE
)

ui <- page_fluid(
  theme = custom_theme,
  tags$head(
    tags$title("Clash Economy Simulator"),
    tags$style(HTML("
      body { background-color: #f4f6fb; min-height: 100vh; }
      .main-container { background-color: #ffffff; border-radius: 12px; padding: 28px; margin: 24px auto; max-width: 1320px; box-shadow: 0 12px 32px rgba(15,23,42,0.08); }
      .sidebar-card { background-color: #ffffff; border-radius: 10px; padding: 16px; margin-bottom: 16px; box-shadow: 0 4px 18px rgba(15,23,42,0.05); }
      .sidebar-card .form-group { margin-bottom: 10px; }
      .sidebar-header { font-weight: 600; color: #1f2937; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 1px solid #e5e7eb; }
      .btn-run { background: linear-gradient(90deg, #2563eb 0%, #3b82f6 100%); border: none; font-weight: 600; padding: 12px; transition: transform 0.2s ease, box-shadow 0.2s ease; }
      .btn-run:hover { transform: translateY(-1px); box-shadow: 0 8px 18px rgba(37,99,235,0.35); }
      .nav-pills .nav-link { border-radius: 10px; margin: 0 4px; font-weight: 500; color: #1f2937; }
      .nav-pills .nav-link.active { background: #2563eb; color: #ffffff; box-shadow: 0 4px 12px rgba(37,99,235,0.35); }
      .table-container { background: #ffffff; border-radius: 10px; padding: 16px; box-shadow: 0 3px 16px rgba(15,23,42,0.04); }
      .plot-container { background: #ffffff; border-radius: 10px; padding: 16px; margin-bottom: 24px; box-shadow: 0 3px 16px rgba(15,23,42,0.04); }
      .metric-card { background: linear-gradient(180deg, #2563eb 0%, #1d4ed8 100%); border-radius: 12px; padding: 18px; text-align: center; color: #ffffff; box-shadow: 0 6px 20px rgba(37,99,235,0.35); }
      .metric-value { font-size: 1.75rem; font-weight: 700; line-height: 1.2; }
      .metric-label { font-size: 0.9rem; opacity: 0.9; margin-top: 4px; }
      .thin-sep { border: 0; border-top: 1px solid #e5e7eb; margin: 16px 0; }
      .range-label { font-weight: 600; font-size: 1.05rem; color: #1f2937; }
      .btn.disabled, .btn:disabled { opacity: 0.6; cursor: not-allowed; }
      .dataTables_wrapper .dataTables_filter input { border-radius: 6px; border: 1px solid #cbd5f5; padding: 6px 8px; }
      @keyframes spinner-border { to { transform: rotate(360deg); } }
      .spinner-border { display: inline-block; width: 1.5rem; height: 1.5rem; vertical-align: text-bottom; border: 0.25em solid currentColor; border-right-color: transparent; border-radius: 50%; animation: spinner-border .75s linear infinite; }
    "))
    ,
    # Initialize Bootstrap tooltips
    tags$script(HTML('document.addEventListener("DOMContentLoaded", function(){var tt=[].slice.call(document.querySelectorAll("[data-bs-toggle=\"tooltip\"]")); tt.map(function(el){return new bootstrap.Tooltip(el);});});'))
  ),
  
  div(class = "main-container",
    h1("Clash Economy Simulator", style = "text-align: center; margin-bottom: 30px; color: #1f2937;"),
    # Hidden E2E beacon with JSON status for headless tests
    div(style = "display:none;", textOutput("e2e_beacon")),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        class = "sidebar-content",
        
        div(class = "sidebar-card",
          h4(class = "sidebar-header", "Data & View"),
          selectInput("data_version",
            label = "Data Version",
            choices = data_versions(), 
            selected = "Base",
            width = "100%"
          ),
          checkboxInput("use_cache", "Use cached results (if unchanged)", value = TRUE),
          uiOutput("run_button_ui"),
          uiOutput('cache_status'),
          hr(),
          span("Day Range", style = "font-weight: 500; color: #495057;"),
          sliderInput("day_range", 
            label = NULL,
            min = 1, max = 360, 
            value = c(1, 90),
            width = "100%"
          ),
          
        )
      ),
      
      navset_pill(
        id = "tabs",
        selected = "Visualizations",
        
        nav_panel("Visualizations",
          br(),
          textOutput("range_label", container = function(...) div(class = "range-label", ...)),
          br(),
          fluidRow(
            column(6, div(class = "plot-container", plotlyOutput("plt_trophies"))),
            column(6, div(class = "plot-container", plotlyOutput("plt_gold")))
          ),
          fluidRow(
            column(12, div(class = "plot-container", plotlyOutput("plt_wallet")))
          ),
          fluidRow(
            column(6, div(class = "plot-container", plotlyOutput("plt_winrate"))),
            column(6, div(class = "plot-container", plotlyOutput("plt_boxmix")))
          ),
          fluidRow(
            column(6, div(class = "plot-container", plotlyOutput("plt_spend_rarity"))),
            column(6, div(class = "plot-container", plotlyOutput("plt_power_rarity")))
          ),
          fluidRow(
            column(6, div(class = "plot-container", plotlyOutput("plt_eff_daily"))),
            column(6, div(class = "plot-container", plotlyOutput("plt_eff_cum")))
          ),
          fluidRow(
            column(6, div(class = "plot-container", plotlyOutput("plt_pass"))),
            column(6, div(class = "plot-container", plotlyOutput("plt_trophies_drift")))
          )
        ),
        
        nav_panel("Daily Table",
          br(),
          textOutput("range_label_table", container = function(...) div(class = "range-label", ...)),
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
          div(class = "plot-container", plotlyOutput("cmp_trophies")),
          div(class = "plot-container", plotlyOutput("cmp_gold")),
          div(class = "plot-container", plotlyOutput("cmp_winrate")),
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
        
        nav_panel("Assumptions Editor",
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
          accordion(open = if (file.exists('runs/last_config.json')) character(0) else 'core', id = 'assump_accordion', multiple = TRUE,
            accordion_panel("Core Settings", value = 'core',
              fluidRow(
            column(3,
              numericInput("cfg_seed", label = tagList("Seed", help_icon("Used for reproducible randomness.")), value = 42, min = 1),
              tags$small(class = "text-muted", "Random seed for reproducible results.")
            ),
            column(3,
              numericInput("cfg_matches", label = tagList("Matches/day", help_icon("Daily match count (fixed). Impacts pacing of progress.")), value = 15, min = 0, step = 1),
              tags$small(class = "text-muted", "Fixed number of matches played per day.")
            ),
            column(3,
              numericInput("cfg_base_wr", label = tagList("Base win rate", help_icon("Win chance vs equal opponents before bots/power. Example: 0.10 = 10%.")), value = 0.5, min = 0, max = 1, step = 0.01),
              tags$small(class = "text-muted", "Win chance vs. equal opponents (before bots/power).")
            ),
            column(3,
              numericInput("cfg_vbonus", label = tagList("Vertical bonus", help_icon("Additive win-rate bonus from progression/power. Example: 0.05 = +5%.")), value = 0.0, min = -1, max = 1, step = 0.01),
              tags$small(class = "text-muted", "Additive win-rate bonus from progression/power.")
            )
          ),
          fluidRow(
            column(3,
              numericInput("cfg_bot_rate", label = tagList("Bot rate", help_icon("Share of matches vs bots. Example: 0.10 = 10%.")), value = 0.1, min = 0, max = 1, step = 0.01),
              tags$small(class = "text-muted", "Share of matches vs. bots.")
            ),
            column(3,
              numericInput("cfg_bot_wr", label = tagList("Bot win rate", help_icon("Your win chance against a bot. Example: 0.90 = 90%.")), value = 0.9, min = 0, max = 1, step = 0.01),
              tags$small(class = "text-muted", "Your win chance against a bot.")
            ),
            column(3,
              numericInput("cfg_trophy_win", label = tagList("Trophies on win", help_icon("Trophies gained on a win. Typical values ~30.")), value = 30, min = 0),
              tags$small(class = "text-muted", "Trophies gained on a win.")
            )
          ),
          fluidRow(
            column(3,
              numericInput("cfg_trophy_loss", label = tagList("Trophies on loss", help_icon("Trophies lost on a defeat. Typical values around −29.")), value = -29, max = 0),
              tags$small(class = "text-muted", "Trophies lost on a defeat.")
            ),
            column(3,
              numericInput("cfg_gold_min", label = tagList("Gold min (per win)", help_icon("Lower bound for random gold per win. Displayed with thousands separators (e.g., 12,500).")), value = 10, min = 0),
              tags$small(class = "text-muted", "Lower bound for random gold per win.")
            ),
            column(3,
              numericInput("cfg_gold_max", label = tagList("Gold max (per win)", help_icon("Upper bound for random gold per win. Displayed with thousands separators (e.g., 25,000).")), value = 10, min = 0),
              tags$small(class = "text-muted", "Upper bound for random gold per win.")
            )
          ),
          br()
        ),
        accordion_panel('Pass Settings', value = 'pass',
          fluidRow(
            column(6,
              numericInput("cfg_pass_crowns", label = tagList("Crowns per level", help_icon("Crowns required to advance one Pass level.")), value = 20, min = 1)
            ),
            column(6,
              numericInput("cfg_pass_gold", label = tagList("Gold per level", help_icon("Gold awarded at each Pass level. Displayed with thousands separators.")), value = 50, min = 0)
            )
          )
        ),
        accordion_panel('Crown Distribution', value = 'crowns',
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
    meta = list(status = "", time = NA_character_),
    edit_data = NULL,
    is_computing = FALSE,
    cancel_requested = FALSE
  )
  
  # Apply selected data version by setting lookup directory used by lucky_star tables
  observe({
    ver <- input$data_version %||% "Base"
    options(cr_econ_tables_dir = normalizePath(data_version_path(ver), mustWork = FALSE))
  })
  
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
  # Bundled sample loader for fast initial render
  load_bundled_sample_daily <- function(){
    path <- file.path('runs','examples','sample_daily.json')
    if (file.exists(path)) {
      df <- jsonlite::fromJSON(path, simplifyVector = TRUE)
      tibble::as_tibble(df)
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

  # Explicit run control: compute on button click (or on first load)
  run_simulation <- function() {
    vals$is_computing <- TRUE
    vals$cancel_requested <- FALSE
    cfg <- build_cfg(); sd <- input$cfg_seed %||% 42
    key <- cache_key()
    res <- if (isTRUE(input$use_cache)) load_cached(key) else NULL
    if (!is.null(res)) {
      vals$meta <- list(status = "Loaded from cache", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      vals$is_computing <- FALSE
    } else {
      res <- tryCatch({
        withProgress(message = 'Simulating season...', value = 0, {
          out <- simulate_season(cfg, seed = sd,
            progress = function(i, n){
              setProgress(value = i/n, detail = paste0('Day ', i, ' of ', n))
            },
            should_cancel = function(){ isTRUE(shiny::isolate(vals$cancel_requested)) }
          )
          out
        })
      }, error = function(e) { 
        vals$is_computing <- FALSE
        showNotification(paste('Simulation failed:', conditionMessage(e)), type='error')
        NULL 
      })
      if (!is.null(res)) {
        save_cached(key, res)
        write_json(list(cfg = cfg, seed = sd), file.path('runs','last_config.json'), auto_unbox = TRUE, pretty = TRUE)
        # If canceled, daily may be shorter than cfg$days
        ddays <- try(nrow(res$daily), silent = TRUE)
        if (!inherits(ddays, 'try-error') && is.finite(ddays) && ddays < cfg$days) {
          vals$meta <- list(status = sprintf("Canceled at day %d/%d", ddays, cfg$days), time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        } else {
          vals$meta <- list(status = if (isTRUE(input$use_cache)) "Computed and cached" else "Computed", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        }
      }
      vals$is_computing <- FALSE
      vals$cancel_requested <- FALSE
    }
    if (!is.null(res)) vals$daily_base <- res$daily %>% mutate(data_version = input$data_version)
  }

  # Initial load: use cache if available; otherwise prefer a quick 90-day compute over tiny samples
  observe({
    if (!is.null(vals$daily_base)) return()
    key <- cache_key()
    res <- if (isTRUE(input$use_cache)) load_cached(key) else NULL
    if (!is.null(res)) {
      vals$daily_base <- res$daily %>% mutate(data_version = input$data_version)
      vals$meta <- list(status = "Loaded from cache", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    } else {
      smp <- load_bundled_sample_daily()
      if (!is.null(smp) && nrow(smp) >= 90) {
        vals$daily_base <- smp %>% mutate(data_version = input$data_version)
        vals$meta <- list(status = "Loaded default sample", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      } else {
        # Run a light default simulation to populate 90 days for a richer first view
        cfg0 <- default_config(days = 90, matches_per_day = 15)
        cfg0$lucky_drop$enabled <- FALSE
        sim <- tryCatch(simulate_season(cfg0, seed = 42), error = function(e) NULL)
        if (!is.null(sim) && is.data.frame(sim$daily)) {
          vals$daily_base <- sim$daily %>% mutate(data_version = input$data_version)
          vals$meta <- list(status = "Computed default 90-day run", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        }
      }
    }
  })

  # Dynamic run/cancel button
  output$run_button_ui <- renderUI({
    if (isTRUE(vals$is_computing)) {
      actionButton("cancel_sim", 
        label = tagList(icon('stop'), ' Cancel'), 
        class = 'btn btn-danger', 
        width = '100%'
      )
    } else {
      actionButton("run_sim", 
        label = tagList(icon('play'), ' Run Simulation'), 
        class = 'btn btn-primary btn-run', 
        width = '100%'
      )
    }
  })
  
  # Run button: recompute with current settings
  observeEvent(input$run_sim, {
    if (!isTRUE(vals$is_computing)) {
      run_simulation()
    }
  })
  observeEvent(input$cancel_sim, {
    vals$cancel_requested <- TRUE
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
        pageLength = 15,
        scrollX = TRUE,
        dom = 'tip'  # tight: table info + pagination only
      ),
      rownames = FALSE,
      class = 'compact stripe hover'
    ) %>%
      formatRound(columns = c("winrate", "power_bonus"), digits = 3) %>%
      formatCurrency(columns = c("gold_total", "gold_from_matches", "gold_from_pass", "gold_spent_upgrades"), currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  # Range label above visuals
  output$range_label <- renderText({
    req(vals$daily_base)
    total <- nrow(vals$daily_base)
    start <- format(input$day_range[1], big.mark = ",")
    end <- format(input$day_range[2], big.mark = ",")
    total_fmt <- format(total, big.mark = ",")
    paste0("Days ", start, " – ", end, " of ", total_fmt)
  })
  output$range_label_table <- renderText({
    req(vals$daily_base)
    total <- nrow(vals$daily_base)
    start <- format(input$day_range[1], big.mark = ",")
    end <- format(input$day_range[2], big.mark = ",")
    total_fmt <- format(total, big.mark = ",")
    paste0("Days ", start, " – ", end, " of ", total_fmt)
  })

  # Keep day range slider in sync with available results
  observe({
    req(vals$daily_base)
    total <- nrow(vals$daily_base)
    updateSliderInput(session, "day_range", min = 1, max = total, value = c(1, min(total, input$day_range[2] %||% total)))
  })
  
  # Upgrade summary
  output$tbl_upg_summary <- renderDT({
    req(vals$daily_base)
    df <- vals$daily_base %>%
      filter(day >= input$day_range[1] & day <= input$day_range[2])
    
    if (nrow(df) > 0 && "gold_spent_upgrades" %in% names(df) && 
        "upgrades_applied" %in% names(df)) {
      total_spend <- sum(df$gold_spent_upgrades, na.rm = TRUE)
      total_upgrades <- sum(df$upgrades_applied, na.rm = TRUE)
      total_power <- sum(df$power_gained, na.rm = TRUE)
      days <- nrow(df)
      bank <- {
        income <- df$gold_from_matches + df$gold_from_pass
        spend <- df$gold_spent_upgrades
        sum(income - spend, na.rm = TRUE)
      }
      avg_gold_per_upgrade <- ifelse(total_upgrades > 0, round(total_spend / total_upgrades, 1), 0)
      power_per_1k_gold <- ifelse(total_spend > 0, round(total_power / (total_spend/1000), 2), NA)
      power_per_upgrade <- ifelse(total_upgrades > 0, round(total_power / total_upgrades, 2), NA)
      upgrades_per_day <- round(total_upgrades / days, 2)
      spend_per_day <- round(total_spend / days, 1)

      summary <- data.frame(
        Metric = c(
          "Total Gold Spent", "Total Upgrades", "Avg Gold per Upgrade",
          "Total Power Gained", "Power per Upgrade", "Power per 1k Gold",
          "Upgrades per Day", "Spend per Day", "Net Bank Change"
        ),
        Value = c(total_spend, total_upgrades, avg_gold_per_upgrade, total_power, power_per_upgrade, power_per_1k_gold, upgrades_per_day, spend_per_day, bank),
        check.names = FALSE
      )
      datatable(
        summary,
        options = list(dom = 't', pageLength = 10),
        rownames = FALSE,
        class = 'compact stripe hover'
      ) %>% formatCurrency(columns = c("Value"), currency = "", interval = 3, mark = ",", digits = 0)
    }
  })
  
  # Plots
  output$plt_trophies <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplotly(plot_trophies(df))
  })
  
  output$plt_gold <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplotly(plot_gold_flow(df))
  })

  output$plt_wallet <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplotly(plot_wallet_balance(df))
  })
  
  output$plt_winrate <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    p <- plot_winrate(df)
    if (isTRUE(input$show_smooth)) p <- p + geom_smooth(se = FALSE, method = 'loess', span = 0.3, color = '#94a3b8')
    ggplotly(p)
  })
  
  output$plt_boxmix <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plt <- plot_box_rarity_mix(df)
    if (is.null(plt)) return(NULL)
    ggplotly(plt)
  })
  
  output$plt_spend_rarity <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plt <- plot_spend_by_rarity_range(df)
    if (is.null(plt)) return(NULL)
    ggplotly(plt)
  })
  
  output$plt_power_rarity <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    plt <- plot_power_by_rarity_range(df)
    if (is.null(plt)) return(NULL)
    ggplotly(plt)
  })
  
  output$plt_eff_daily <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    p <- plot_upgrade_efficiency_daily(df)
    if (isTRUE(input$show_smooth)) p <- p + geom_smooth(se = FALSE, method = 'loess', span = 0.3, color = '#16a34a')
    ggplotly(p)
  })
  
  output$plt_eff_cum <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    p <- plot_upgrade_efficiency_cum(df)
    if (isTRUE(input$show_smooth)) p <- p + geom_smooth(se = FALSE, method = 'loess', span = 0.3, color = '#065f46')
    ggplotly(p)
  })
  
  output$plt_pass <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplotly(plot_pass_progress(df))
  })
  
  output$plt_trophies_drift <- renderPlotly({
    req(vals$daily_base)
    df <- vals$daily_base %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplotly(plot_trophies_drift(df))
  })

  # E2E beacon: publish minimal health/status JSON for automation
  output$e2e_beacon <- renderText({
    dd <- vals$daily_base
    has_daily <- !is.null(dd)
    rows <- if (!is.null(dd)) nrow(dd) else 0L
    mt <- tryCatch({
      if (!is.null(dd)) list(
        trophies = as.numeric(tail(dd$trophies_end, 1)),
        gold = suppressWarnings(as.numeric(sum(dd$gold_total))),
        winrate = suppressWarnings(as.numeric(mean(dd$winrate)))
      ) else NULL
    }, error = function(e) NULL)
    cr <- !is.null(vals$cmp_results) && length(vals$cmp_results) > 0
    jsonlite::toJSON(list(
      has_daily = isTRUE(has_daily),
      daily_rows = rows,
      metrics = mt,
      cmp_ready = isTRUE(cr),
      t = as.numeric(Sys.time())
    ), auto_unbox = TRUE)
  })

  # Export button removed (use R/snapshot.R for batch exports)
  
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
  
  output$cmp_trophies <- renderPlotly({
    req(length(vals$cmp_results) > 0)
    df <- bind_rows(vals$cmp_results)
    dd <- df %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplotly(ggplot(dd, aes(day, trophies_end, color = config)) + geom_line() +
      labs(title = "Trophies Comparison", x = "Day", y = "Trophies") + theme_minimal(base_size = 12))
  })
  
  output$cmp_gold <- renderPlotly({
    req(length(vals$cmp_results) > 0)
    df <- bind_rows(vals$cmp_results)
    dd <- df %>% filter(day >= input$day_range[1] & day <= input$day_range[2]) %>%
      group_by(config) %>% arrange(day, .by_group = TRUE) %>% mutate(cum_gold = cumsum(gold_total))
    ggplotly(ggplot(dd, aes(day, cum_gold, color = config)) + geom_line() +
      labs(title = "Cumulative Gold Comparison", x = "Day", y = "Gold (cumulative)") + theme_minimal(base_size = 12))
  })
  
  output$cmp_winrate <- renderPlotly({
    req(length(vals$cmp_results) > 0)
    df <- bind_rows(vals$cmp_results)
    dd <- df %>% filter(day >= input$day_range[1] & day <= input$day_range[2])
    ggplotly(ggplot(dd, aes(day, winrate*100, color = config)) + geom_line() +
      labs(title = "Winrate Comparison", x = "Day", y = "Winrate (%)") + theme_minimal(base_size = 12))
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
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE,
      class = 'compact stripe hover'
    ) %>%
      formatCurrency(columns = c("Gold Spent"), currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c("Upgrades", "Final Trophies", "Final Power"), digits = 0) %>%
      formatRound(columns = "Avg Gold/Upgrade", digits = 1)
  })
  
  # Data Editor
  # Load table for editing when selection changes
  observeEvent(input$edit_table, {
    req(input$edit_table)
    path <- file.path(tables_dir(), input$edit_table)
    if (file.exists(path)) {
      vals$edit_data <- readr::read_csv(path, show_col_types = FALSE)
    } else {
      vals$edit_data <- NULL
    }
  }, ignoreInit = TRUE)

  # Render editable table from reactive state
  output$edit_table_view <- renderDT({
    req(vals$edit_data)
    datatable(vals$edit_data,
      editable = TRUE,
      options = list(pageLength = 15, scrollX = TRUE),
      class = 'table-striped'
    )
  })

  # Track in-place edits from DT and keep them in vals$edit_data
  proxy_edit <- DT::dataTableProxy("edit_table_view")
  observeEvent(input$edit_table_view_cell_edit, {
    info <- input$edit_table_view_cell_edit
    req(!is.null(vals$edit_data))
    i <- info$row; j <- info$col; v <- info$value
    dat <- vals$edit_data
    # Coerce to original column type when reasonable
    tgt <- dat[[j]]
    if (is.numeric(tgt)) {
      nv <- suppressWarnings(as.numeric(v))
      dat[i, j] <- ifelse(is.na(nv), v, nv)
    } else if (inherits(tgt, "integer")) {
      nv <- suppressWarnings(as.integer(v))
      dat[i, j] <- ifelse(is.na(nv), v, nv)
    } else {
      dat[i, j] <- v
    }
    vals$edit_data <- dat
    DT::replaceData(proxy_edit, dat, resetPaging = FALSE, rownames = FALSE)
  })

  # Save current edited table into a versioned directory
  observeEvent(input$edit_save, {
    req(input$edit_table, vals$edit_data)
    ver <- trimws(input$edit_version %||% "")
    if (!nzchar(ver)) {
      showNotification("Please enter a version name to save.", type = "error")
      return(invisible(NULL))
    }
    ensure_version_dirs(ver)
    out_dir <- normalizePath(data_version_path(ver), mustWork = FALSE)
    out_path <- file.path(out_dir, input$edit_table)
    ok <- try({ readr::write_csv(vals$edit_data, out_path) ; TRUE }, silent = TRUE)
    if (identical(ok, TRUE)) {
      # Refresh version choices and switch to the new version
      updateSelectInput(session, "data_version", choices = data_versions(), selected = ver)
      options(cr_econ_tables_dir = out_dir)
      showNotification(sprintf("Saved table to %s", out_path), type = "message")
    } else {
      showNotification("Failed to save table. Check filesystem permissions.", type = "error")
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
