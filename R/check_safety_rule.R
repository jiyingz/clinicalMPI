#' Check Safety Rule
#'
#' @description Check if an available dose violates safety rule (toxicity count/sample size > safety threshold zeta).
#' @param dose A numeric dose level (assumed to be available)
#' @param dose_info Dataframe containing
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]
#' @param zeta Invoke stopping rule if more than this proportion of subjects exhibit toxicity
#' @return Boolean T/F whether safety rule was violated
#' @usage check_safety_rule(80, dose_info, 0.8)
check_safety_rule <- function(dose, dose_info, zeta) {
  dose_ind = which(dose_info$dose_lvl == dose)
  if (length(dose_ind) == 0) {stop('Invalid dose value. Please enter the dose value and not the index.')}

  pt = dose_info$pt[dose_ind]
  if (pt > zeta) { return(T) }
  return(F)
}
