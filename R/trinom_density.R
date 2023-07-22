#' Trinomial Density
#'
#' @description Calculate Trinomial(N, Pf, Pt) density with given parameters
#' @param x Futility probability (pf)
#' @param y Toxicity probability (pt)
#' @param f_count Futility count (x)
#' @param t_count Toxicity count (z)
#' @param N Sample size
#' @return The Trinomial(N, Pf, Pt) density at (f_count, t_count)
#' @usage trinom_density(0.4, 0.1, 4, 1, 10)
#' @export
trinom_density <- function(x, y, f_count, t_count, N) {
  if (all(x < 0 | y < 0)) {stop('x and y need to be between 0 and 1')}
  
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  return(stats::dbinom(f_count, N-t_count, x/(1-y)) * stats::dbinom(t_count, N, y))
}
