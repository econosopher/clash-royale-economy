#' @template param-cfg
#' @param cfg Configuration object containing simulation parameters

#' @template param-state  
#' @param state GameState object or list representing current game state

#' @template param-seed
#' @param seed Random seed for reproducibility

#' @template return-simulation
#' @return List containing:
#' \describe{
#'   \item{daily}{Data frame with per-day simulation results}
#'   \item{final_state}{Final game state after simulation}
#'   \item{summary}{Summary statistics}
#' }