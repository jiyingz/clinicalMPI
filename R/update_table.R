#' Update Dose-Response Table
#'
#' @description Update table with dose-response information from a new cohort. For the given dose level, add the new futility, efficacy, and toxicity counts to existing numbers.
#' @param dose A numeric dose level (assumed to be available)
#' @param dose_info Dataframe containing
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]
#' @param x_d Number of subjects experiencing futility
#' @param y_d Number of subjects experiencing efficacy
#' @param z_d Number of subjects experiencing toxicity
#' @return A dataframe containing updated dose-response table (`dose_info`)
#' @usage dose_info = change_dose_info(80, 1, 2, 0) #update dose 80 with an *additional* 1 futility and 2 efficacy counts
#' @export
update_table <- function(dose, dose_info, x_d, y_d, z_d) {
  dose_ind = which(dose_info$dose_lvl == dose)
  if (length(dose_ind) == 0) {stop('Invalid dose value. Please enter the dose value and not the index.')}

  dose_info$X_d[dose_ind] = dose_info$X_d[dose_ind] + x_d
  dose_info$Y_d[dose_ind] = dose_info$Y_d[dose_ind] + y_d
  dose_info$Z_d[dose_ind] = dose_info$Z_d[dose_ind] + z_d
  dose_info$N_d[dose_ind] = dose_info$N_d[dose_ind] + x_d + y_d + z_d

  zero_indexes = which(dose_info$N_d == 0)
  dose_info$pf[-zero_indexes] = dose_info$X_d[-zero_indexes] / dose_info$N_d[-zero_indexes]
  dose_info$pe[-zero_indexes] = dose_info$Y_d[-zero_indexes] / dose_info$N_d[-zero_indexes]
  dose_info$pt[-zero_indexes] = dose_info$Z_d[-zero_indexes] / dose_info$N_d[-zero_indexes]

  dose_info$pf[zero_indexes] = 0
  dose_info$pe[zero_indexes] = 0
  dose_info$pt[zero_indexes] = 0

  dose_info
}
