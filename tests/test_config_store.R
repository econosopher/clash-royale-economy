tmp_store <- file.path(tempdir(), "config-store")
if (dir.exists(tmp_store)) unlink(tmp_store, recursive = TRUE)
old_opt <- options(cr_config_store_path = tmp_store)
on.exit({
  options(old_opt)
  if (dir.exists(tmp_store)) unlink(tmp_store, recursive = TRUE)
}, add = TRUE)

ensure_store()
cfgs <- load_configs()
stopifnot(is.list(cfgs))
stopifnot("Defaults" %in% names(cfgs))

custom <- default_config(days = 5, matches_per_day = 5)
save_config("UnitTestConfig", custom)

cfgs2 <- load_configs()
stopifnot("UnitTestConfig" %in% names(cfgs2))
stopifnot(identical(cfgs2$UnitTestConfig$days, 5L))
