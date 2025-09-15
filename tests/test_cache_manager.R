set.seed(101)

tmp_cache <- file.path(tempdir(), "cr_cache_test")
if (dir.exists(tmp_cache)) unlink(tmp_cache, recursive = TRUE)
cm <- CacheManager$new(cache_dir = tmp_cache, max_size_mb = 2)
assign(".global_cache_manager", cm, envir = .GlobalEnv)
on.exit({
  if (exists(".global_cache_manager", envir = .GlobalEnv)) {
    rm(".global_cache_manager", envir = .GlobalEnv)
  }
  if (dir.exists(tmp_cache)) unlink(tmp_cache, recursive = TRUE)
}, add = TRUE)

cfg <- default_config(days = 3, matches_per_day = 4)
cfg$lucky_drop$enabled <- FALSE

res1 <- simulate_with_cache(cfg, seed = 11, data_version = "unit-test", force_compute = TRUE)
stopifnot(isFALSE(isTRUE(attr(res1, "from_cache"))))

res2 <- simulate_with_cache(cfg, seed = 11, data_version = "unit-test")
stopifnot(isTRUE(attr(res2, "from_cache")))

stats <- cm$stats()
stopifnot(is.list(stats), "disk" %in% names(stats))
