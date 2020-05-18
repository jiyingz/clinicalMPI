#' Check Futility Rule
#'
#' @description Check if an available dose violates futility rule (futility count/sample size > futility threshold (eta).
#' @param dose A numeric dose level (assumed to be available)
#' @param dose_info Dataframe containing
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]
#' @param eta Invoke stopping rule if more than this proportion of subjects exhibit futility
#' @return Boolean T/F whether futility rule was violated
#' @usage check_futility_rule(80, dose_info, 0.8)
#' @export
check_futility_rule <- function(dose, dose_info, eta) {
  dose_ind = which(dose_info$dose_lvl == dose)
  if (length(dose_ind) == 0) {stop('Invalid dose value. Please enter the dose value and not the index.')}

  pf = dose_info$pf[dose_ind]
  if (pf > eta) { return(T) }
  return(F)
}
