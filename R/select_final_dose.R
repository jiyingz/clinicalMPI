#' Final Dose Selection
#'
#' @description Select the final dose out of available doses that has the highest utility.
#' @param dose_info Dataframe containing
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]
#' @param sample_size Cumulative sample size at trial end
#' @param max_samp_size Maximum sample size for trial
#' @param stopping_rule Whether or not a stopping rule was invoked
#' @param filter Posterior sum threshold to determine dose availability for final selection (pf+pt <= filter)
#' @return The final dose recommendation, or -99 if no dose was selected
#' @usage select_final_dose(dose_info, stopping_rule = FALSE, filter = 0.5)
#' @details No final dose is recommended if no doses are available, any stopping rules have been invoked, or no dose meets the selection criteria of pf+pt <= filter (e.g. filter = 0.5).
#' The latter criteria is to exclude doses with too high toxicity or futility, but have not invoked safety or futility rules.
#' Another alternative is to exclude doses with estimated chances of toxicity and futility above the equivalence intervals, but this has been found to be too strict a criteria and results in
#' no dose recommendation too often through simulation studies.
#' @export
select_final_dose <- function(dose_info, sample_size, max_samp_size, stopping_rule, filter = 0.5) {
  selected_dose_ind = -99
  max_utility = -99

  if (sample_size != max_samp_size & stopping_rule == TRUE) {return(-99)}

  for (dose_ind in 1:nrow(dose_info)) {
    if (dose_info$available[dose_ind] == TRUE) {
      N_d = dose_info$N_d[dose_ind]
      X_d = dose_info$X_d[dose_ind]
      Y_d = dose_info$Y_d[dose_ind]
      Z_d = dose_info$Z_d[dose_ind]

      if (N_d > 0) {
        if (X_d/N_d + Z_d/N_d <= filter) {
          dose_utility = calc_utility(X_d, Y_d, Z_d)

          if (dose_utility > max_utility) {
            selected_dose_ind = dose_ind
            max_utility = dose_utility
          }
        }
      }
    }
  }

  if (selected_dose_ind == -99) {
    return(-99)
  } else {
    return(dose_info$dose_lvl[selected_dose_ind])
  }
}
