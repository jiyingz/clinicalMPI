#' Make Decision
#'
#' @description Make a decision to escalate (E), stay (S), or de-escalate (D) dose level based on the selected model.
#' Prioritizes safety over efficacy in that if the chosen model's estimated chance of toxicity is higher than the toxicity equivalence interval, choose to de-escalate.
#' Otherwise, choose to escalate if chosen model lies above the futility equivalence interval, and stay if it is within or below both equivalence intervals.
#' (Assumes that DU/EU decision, if it is the one to make, has already been taken care of before hitting this function.)
#' @param best_pf Futility interval index for chosen model
#' @param best_pt Toxicity interval index for chosen model
#' @param pf_matrix Matrix containing start and end points of futility (pf) intervals
#' @param pt_matrix Matrix containing start and end points of toxicity (pt) intervals
#' @param PF Target tolerable futility probability
#' @param PF_tolerance Half-length of futility equivalence interval (epsilon_F)
#' @param PT Target tolerable toxicity probability
#' @param PT_tolerance Half-length of toxicity equivalence interval (epsilon_T)
#' @return String containing decision for next dose ("D", "S", "E")
#' @usage intervals = make_intervals(PF, PF_tolerance, eta, PT, PT_tolerance, zeta)
#' pf_matrix = intervals$pf_matrix
#' pt_matrix = intervals$pt_matrix
#' selected = select_highestprob_interval(dose_info, pf_matrix, pt_matrix)
#' best_pf = selected$best_pf
#' best_pt = selected$best_pt
#' decision_table[f_count+1, t_count+1] = make_decision(best_pf, best_pt, pf_matrix, pt_matrix,
#'                                                      PF = 0.2, PF_tolerance = 0.05, PT = 0.2, PT_tolerance = 0.05)
#'
#' @export
make_decision <- function(best_pf, best_pt, pf_matrix, pt_matrix, PF, PF_tolerance, PT, PT_tolerance) {
  pf_EI = which(mapply(function(x, y) {isTRUE(all.equal(x, y))}, pf_matrix[,1], PF - PF_tolerance))
  pt_EI = which(mapply(function(x, y) {isTRUE(all.equal(x, y))}, pt_matrix[,1], PT - PT_tolerance))

  if (best_pt > pt_EI) {
    return("D")
  }
  else if (best_pf > pf_EI) {
    return("E")
  }
  else {
    return("S")
  }

}
