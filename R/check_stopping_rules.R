#' Check Stopping Rules
#'
#' @description check if dose invokes stopping rules:
#'
#' * Maximum sample size reached
#'
#' * No doses available for evaluation
#'
#' * Lowest dose is too toxic
#'
#' * Highest dose is too futile
#'
#' * Decision is DU/EU and no doses in direction to move are available for evaluation
#' @param dose A numeric dose level (assumed to be available)
#' @param dose_info Dataframe containing
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]
#' @param sample_size Current sample size (across all doses)
#' @param max_samp_size Maximum sample size for entire trial
#' @param decision Decision, one of "DU", "D", "S", "E", or "EU". Defaults to `NA`.
#' @param closest_lower_dose If decision is to de-escalate, the closest available lower dose, `-Inf` if none available. Defaults to `NA`.
#' @param closest_higher_dose If decision is to escalate, the closest available higher dose, `Inf` if none available. Defaults to `NA`.
#' @return Boolean T/F whether any stopping rules were violated
#' @usage check_stopping_rules(80, dose_info, 15, 24, "D", closest_lower_dose = 60)
check_stopping_rules <- function(dose, dose_info, sample_size, max_samp_size, decision = NA, closest_lower_dose = NA, closest_higher_dose = NA) {
  if (!(decision %in% c(NA, "DU", "D", "S", "E", "EU"))) {stop('Invalid decision.')}
  if (!(dose %in% dose_info$dose_lvl)) {stop('Invalid dose value. Please enter the dose value and not the index.')}
  if (!(closest_lower_dose %in% c(NA, -Inf, dose_info$dose_lvl))) {stop('Invalid closest lower dose.')}
  if (!(closest_higher_dose %in% c(NA, Inf, dose_info$dose_lvl))) {stop('Invalid closest higher dose.')}

  #max sample size reached | no available doses | cannot de-escalate | cannot escalate
  if ((sample_size > max_samp_size) |
      (sum(dose_info$available) == 0) |
      (decision == "DU" & !(is.na(closest_lower_dose)) & closest_lower_dose == -Inf) |
      (decision == "EU" & !(is.na(closest_higher_dose)) & closest_higher_dose == Inf)) {return(TRUE)}

  return(FALSE)
}
