#' Change Dose-Response Table Row Entries
#'
#' @description Change an entire row in dose-response table with desired information.
#' For the given dose level, change the existing futility, efficacy, and toxicity counts to the specified numbers.
#' Useful for fixing entry mistakes and debugging.
#' @param dose A numeric dose level (assumed to be available)
#' @param dose_info Dataframe containing
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]
#' @param x_d Number of subjects experiencing futility
#' @param y_d Number of subjects experiencing efficacy
#' @param z_d Number of subjects experiencing toxicity
#' @return A dataframe containing updated dose-response table (`dose_info`)
#' @usage dose_info = change_dose_info(80, 1, 2, 0) #update dose 80 to 1 futility and 2 efficacy counts
#' @export
change_dose_info <- function(dose, dose_info, x_d, y_d, z_d) {
  if(x_d < 0 | y_d < 0 | z_d < 0) {stop('invalid negative value entered')}

  dose_ind = which(dose_info$dose_lvl == dose)

  n_d = x_d + y_d + z_d
  dose_info$X_d[dose_ind] = x_d
  dose_info$Y_d[dose_ind] = y_d
  dose_info$Z_d[dose_ind] = z_d
  dose_info$N_d[dose_ind] = n_d

  if(n_d == 0) {stop('cannot calculate probabilities -- dividing by 0')}

  dose_info$pf[dose_ind] = x_d / n_d
  dose_info$pe[dose_ind] = y_d / n_d
  dose_info$pt[dose_ind] = z_d / n_d

  dose_info
}
