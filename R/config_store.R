library(jsonlite)

config_store_path <- function() {
  normalizePath(file.path("assets", "configs"), mustWork = FALSE)
}

config_store_file <- function() {
  file.path(config_store_path(), "configs.json")
}

default_presets <- function() {
  list(
    Defaults = default_config(days = 360, matches_per_day = 15),
    HighBots = {x <- default_config(days = 360, matches_per_day = 15); x$bot_rate <- 0.30; x$bot_winrate <- 0.90; x},
    FastPass = {x <- default_config(days = 360, matches_per_day = 15); x$pass_config$crowns_per_level <- 10L; x}
  )
}

ensure_store <- function() {
  dir.create(config_store_path(), recursive = TRUE, showWarnings = FALSE)
  f <- config_store_file()
  if (!file.exists(f)) {
    presets <- default_presets()
    write_json(presets, f, pretty = TRUE, auto_unbox = TRUE)
  }
}

load_configs <- function() {
  ensure_store()
  f <- config_store_file()
  cfgs <- tryCatch(read_json(f, simplifyVector = FALSE), error = function(e) list())
  # Merge defaults to ensure core keys exist
  cfgs
}

save_config <- function(name, cfg) {
  ensure_store()
  f <- config_store_file()
  cfgs <- load_configs()
  cfgs[[name]] <- cfg
  write_json(cfgs, f, pretty = TRUE, auto_unbox = TRUE)
  invisible(TRUE)
}

