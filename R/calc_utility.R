#' Calculate Utility
#'
#' @description Calculate expected utility function = 1 - E\[lower loss\] - E\[upper loss\]
#' @param a Futility count
#' @param b Efficacy count
#' @param c Toxicity count
#' @param num_samples Number of times to sample Dirichlet(a,b,c) distribution. The higher this number, the more precise the estimate (default = 1e4).
#' @usage calc_utility(dose_info$X_d[1], dose_info$Y_d[1], dose_info$Z_d[1], num_samples = 1e4)
#' @return The calculated utility value (numeric)
#' @details The observed pf and pt values are only one instantiation of the Dirichlet(a,b,c) distribution.
#' In this function we simulate many draws from this distribution and calculate expected losses by averaging loss over all drawn toxicity and futility probabilities, and then derive the Utility value for that dose level.
#' In other words, this calculation is mathematically equivalent to finding the lower and upper utility values for every sample,
#' and then taking Utility = 1 - mean(all toxicity losses) - mean(all futility losses).
#'
#' Note that the bottleneck of this function is `num_samples` -- the higher this value is, the slower the overall simulation performance. Between 1e4 and 1e6 is a reasonable range.
calc_utility <- function(a, b, c, num_samples = 1e4) {
  if (num_samples < 1e3) {stop('please set num_samples higher to increase precision')}
  if (!requireNamespace("MCMCpack", quietly = TRUE)) {
    stop("Package \"MCMCpack\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  samples = MCMCpack::rdirichlet(num_samples, c(a+1, b+1, c+1))
  pf_hats = samples[,1]
  pt_hats = samples[,3]

  losses_pfhats = sapply(pf_hats, ploss, target = 0.2, tolerance = 0.05)
  losses_pthats = sapply(pt_hats, ploss, target = 0.2, tolerance = 0.05)
  return(1 - mean(losses_pfhats) - mean(losses_pthats))
}
