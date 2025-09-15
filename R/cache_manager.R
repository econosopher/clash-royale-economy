#' @title Cache Manager
#' @description File-based caching system using cachem for persistent storage
#' @import cachem
#' @import digest

source("R/constants.R")

#' @export
CacheManager <- R6::R6Class("CacheManager",
  public = list(
    #' @field cache_dir Directory for cache files
    cache_dir = NULL,
    
    #' @field disk_cache Cachem disk cache object
    disk_cache = NULL,
    
    #' @field memory_cache Cachem memory cache object  
    memory_cache = NULL,
    
    #' @field max_size Maximum cache size in bytes
    max_size = NULL,
    
    #' @description Initialize cache manager
    #' @param cache_dir Directory for cache files
    #' @param max_size_mb Maximum cache size in MB
    initialize = function(cache_dir = "runs/cache", max_size_mb = CACHE_MAX_SIZE_MB) {
      self$cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
      self$max_size <- max_size_mb * 1024 * 1024
      
      # Ensure cache directory exists
      if (!dir.exists(self$cache_dir)) {
        dir.create(self$cache_dir, recursive = TRUE)
      }
      
      # Initialize disk cache with cachem
      self$disk_cache <- cachem::cache_disk(
        dir = self$cache_dir,
        max_size = self$max_size,
        max_age = CACHE_EXPIRY_MINUTES * 60,
        evict = "lru"
      )
      
      # Initialize memory cache for frequently accessed items
      self$memory_cache <- cachem::cache_mem(
        max_size = 50 * 1024 * 1024,  # 50MB memory cache
        max_age = 300,  # 5 minutes
        evict = "lru"
      )
      
      invisible(self)
    },
    
    #' @description Generate cache key from configuration
    #' @param cfg Configuration object
    #' @param seed Random seed
    #' @param data_version Data version identifier
    #' @return Cache key string
    generate_key = function(cfg, seed, data_version = "Base") {
      key_data <- list(
        config = cfg,
        seed = seed,
        data_version = data_version,
        version = packageVersion("base")
      )
      digest::digest(key_data, algo = "xxhash64")
    },
    
    #' @description Get cached result
    #' @param key Cache key
    #' @return Cached object or NULL if not found
    get = function(key) {
      # Check memory cache first
      result <- tryCatch({
        self$memory_cache$get(key)
      }, error = function(e) NULL)
      
      if (!is.null(result)) {
        return(result)
      }
      
      # Check disk cache
      result <- tryCatch({
        self$disk_cache$get(key)
      }, error = function(e) NULL)
      
      # If found on disk, also store in memory
      if (!is.null(result)) {
        tryCatch({
          self$memory_cache$set(key, result)
        }, error = function(e) {})
      }
      
      return(result)
    },
    
    #' @description Store result in cache
    #' @param key Cache key
    #' @param value Object to cache
    set = function(key, value) {
      # Store in both caches
      tryCatch({
        self$disk_cache$set(key, value)
        self$memory_cache$set(key, value)
        invisible(TRUE)
      }, error = function(e) {
        warning("Failed to cache result: ", e$message)
        invisible(FALSE)
      })
    },
    
    #' @description Check if key exists in cache
    #' @param key Cache key
    #' @return TRUE if exists, FALSE otherwise
    exists = function(key) {
      self$memory_cache$exists(key) || self$disk_cache$exists(key)
    },
    
    #' @description Remove item from cache
    #' @param key Cache key
    remove = function(key) {
      tryCatch({
        self$memory_cache$remove(key)
      }, error = function(e) {})
      
      tryCatch({
        self$disk_cache$remove(key)
      }, error = function(e) {})
      
      invisible(self)
    },
    
    #' @description Clear all cache
    clear = function() {
      tryCatch({
        self$memory_cache$reset()
        self$disk_cache$reset()
        message("Cache cleared successfully")
      }, error = function(e) {
        warning("Failed to clear cache: ", e$message)
      })
      invisible(self)
    },
    
    #' @description Get cache statistics
    #' @return List with cache stats
    stats = function() {
      list(
        memory = tryCatch({
          list(
            size = self$memory_cache$size(),
            keys = self$memory_cache$keys()
          )
        }, error = function(e) list(size = 0, keys = character())),
        
        disk = tryCatch({
          list(
            size = self$disk_cache$size(),
            keys = self$disk_cache$keys(),
            directory = self$cache_dir
          )
        }, error = function(e) list(size = 0, keys = character(), directory = self$cache_dir))
      )
    },
    
    #' @description Prune old entries
    prune = function() {
      tryCatch({
        self$memory_cache$prune()
        self$disk_cache$prune()
        message("Cache pruned successfully")
      }, error = function(e) {
        warning("Failed to prune cache: ", e$message)
      })
      invisible(self)
    }
  )
)

#' @export
#' @description Global cache manager instance
get_cache_manager <- function() {
  if (!exists(".global_cache_manager", envir = .GlobalEnv)) {
    cache_dir <- getOption("cr_cache_dir", default = "runs/cache")
    max_size <- getOption("cr_cache_max_size_mb", default = CACHE_MAX_SIZE_MB)
    assign(".global_cache_manager", CacheManager$new(cache_dir = cache_dir, max_size_mb = max_size), envir = .GlobalEnv)
  }
  get(".global_cache_manager", envir = .GlobalEnv)
}

#' @export
#' @description Cached simulation wrapper
#' @param cfg Configuration object
#' @param seed Random seed
#' @param data_version Data version
#' @param force_compute Force recomputation
#' @return Simulation results
simulate_with_cache <- function(cfg, seed, data_version = "Base", force_compute = FALSE) {
  cache_mgr <- get_cache_manager()
  key <- cache_mgr$generate_key(cfg, seed, data_version)
  
  if (!force_compute) {
    cached <- cache_mgr$get(key)
    if (!is.null(cached)) {
      attr(cached, "from_cache") <- TRUE
      return(cached)
    }
  }
  
  # Run simulation
  result <- tryCatch({
    simulate_season(cfg, seed = seed)
  }, error = function(e) {
    stop("Simulation failed: ", e$message)
  })
  
  # Cache result
  cache_mgr$set(key, result)
  attr(result, "from_cache") <- FALSE
  
  return(result)
}
