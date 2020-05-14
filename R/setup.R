#' Dose-Response Table Setup
#'
#' @description Set up dose response-tracking table documenting the current dose's availability status,
#' sample size, and futility, efficacy, and toxicity counts and corresponding probabilities.
#' This table should be updated every time new cohort responses are available.
#' @param doses Vector of ordered dose levels
#' @return An empty data frame in format
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]:
#'
#' * dose_lvl -- dose level
#'
#' * availability -- whether dose is available for further testing
#'
#' * N_d -- sample size
#'
#' * X_d -- futility counts
#'
#' * Y_d -- efficacy counts
#'
#' * Z_d -- toxicity counts
#'
#' * pf -- estimated chance of futility
#'
#' * pe -- estimated chance of efficacy
#'
#' * pt -- estimated chance of toxicity
#' @usage doses = c(60,80,100,120,140)
#' dose_info = setup(doses)
#' @export
setup <- function(doses) {
  if (length(doses) == 0) {stop('no doses given')}
  doses = doses[order(doses)]

  dose_info = data.frame("dose_lvl" = factor(doses, levels = doses),
                         "available" = rep(TRUE, length(doses)),
                         "N_d" = rep(0, length(doses)),
                         "X_d" = rep(0, length(doses)),
                         "Y_d" = rep(0, length(doses)),
                         "Z_d" = rep(0, length(doses)),
                         "pf"  = rep(0, length(doses)),
                         "pe"  = rep(0, length(doses)),
                         "pt"  = rep(0, length(doses))
  )

  dose_info
}
