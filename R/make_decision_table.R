#' Make Decision Table
#'
#' @description Produces the pre-smoothed decision table for a certain dose sample size using given parameters and thresholds.
#' The decision table should be generalizable to any dose level. Safety is valued over efficacy, so any estimates exceeding the
#' toxicity tolerance will call for de-escalation.
#'
#' **IMPORTANT:** The decision table must be named in the following format to run simulations: "decision_table#" where #=N.
#' @param N Number of patients tested at a dose
#' @param PF Target tolerable futility probability
#' @param PF_tolerance Half-length of futility equivalence interval (epsilon_F)
#' @param eta Upper futility threshold beyond which invokes the futility rule
#' @param PT Target tolerable toxicity probability
#' @param PT_tolerance Half-length of toxicity equivalence interval (epsilon_T)
#' @param zeta Upper toxicity threshold beyond which invokes the safety rule
#' @returns A matrix containing the decision table.
#' Futility counts are on the row axis, and toxicity counts are on the column axis.
#' @usage make_decision_table(15, PF = 0.2, PF_tolerance = 0.05, eta = 0.8, PT = 0.2, PT_tolerance = 0.05, zeta = 0.8)
#' @export
make_decision_table <- function(N, PF, PF_tolerance, eta, PT, PT_tolerance, zeta) {
  if (N <= 0) {stop('invalid sample size')}
  if (PF_tolerance < 0) {stop('PF_tolerance cannot be negative')}
  if (PT_tolerance < 0) {stop('PT_tolerance cannot be negative')}
  if (eta < PF + PF_tolerance) {stop('eta cannot be < PF + PF_tolerance')}
  if (zeta < PT + PT_tolerance) {stop('zeta cannot be < PT + PT_tolerance')}

  dose = 1
  dose_info = setup(dose)
  intervals = make_intervals(PF, PF_tolerance, eta, PT, PT_tolerance, zeta)
  pf_matrix = intervals$pf_matrix
  pt_matrix = intervals$pt_matrix

  decision_table = matrix(NA, nrow = N+1, ncol = N+1)
  for (f_count in 0:N) {
    for (t_count in 0:N) {
      e_count = N - f_count - t_count
      if (e_count >= 0) {
        dose_info = change_dose_info(dose, dose_info, f_count, e_count, t_count)

        if (check_safety_rule(dose, dose_info, zeta) == TRUE) { #upper right corner of decision table
          decision_table[f_count+1, t_count+1] = "DU"
        } else {
          selected = select_highestprob_interval(dose_info, pf_matrix, pt_matrix)
          best_pf = selected$best_pf
          best_pt = selected$best_pt
          decision_table[f_count+1, t_count+1] = make_decision(best_pf, best_pt, pf_matrix, pt_matrix,
                                                               PF, PF_tolerance, PT, PT_tolerance)
          if (check_futility_rule(dose, dose_info, eta) == TRUE & dose_info$pt < PT+PT_tolerance) { #lower left corner of decision table
            decision_table[f_count+1, t_count+1] = "EU"
          }
        }
      }
    }
  }

  message('Futility counts are on the row axis, toxicity counts are on the column axis.')
  message('Counts start at 0 even though row and column indexing starts at 1! Subtract 1 to get the count number.')
  return(decision_table)
}
