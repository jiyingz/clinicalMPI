#' Adjust Dose-Response Table when Safety Rule Invoked
#'
#' @description Adjust dose availabilities given that the safety rule was invoked, if dose sample size >= 6 (if sample size < 6, choose to stay and explore dose further, so no availabilities are changed).
#' The dose that invoked the safety rule and any higher doses will be made unavailable for future consideration.
#' @param dose A numeric dose level that violated the safety rule
#' @param dose_info Dataframe containing
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]
#' @return Updated dose-response table dataframe with adjusted availabilities
#' @usage dose_info = adjust_for_safety_rule(120, dose_info) #if dose level 120 invoked safety rule
adjust_for_safety_rule <- function(dose, dose_info) {
  dose_ind = which(dose_info$dose_lvl == dose)
  if (length(dose_ind) == 0) {stop('Invalid dose value. Please enter the dose value and not the index.')}

  dose_samp_size = dose_info$N_d[dose_ind]
  if (dose_samp_size >= 6) {
    for (ind in dose_ind:nrow(dose_info)) {
      dose_info$available[ind] = FALSE #make dose and all higher doses unavailable
    }
  }

  return(dose_info)
}
