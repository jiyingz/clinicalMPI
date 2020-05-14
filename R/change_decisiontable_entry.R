#' Smooth Decision Table
#'
#' @description Change any entry of a decision table
#' @param decision_table A matrix decision table, returned from `make_decision_table()`
#' @param futility_index Futility interval index(es) of decision to be changed
#' @param toxicity_index Toxicity interval index(es) of decision to be changed
#' @param new_decision Decision to be changed to, one of "DU", "D", "S", "E", or "EU"
#' @returns A matrix containing the updated decision table
#' @usage decision_table15 = make_decision_table(15, PF = 0.2, PF_tolerance = 0.05, eta = 0.8, PT = 0.2, PT_tolerance = 0.05, zeta = 0.8)
#' decision_table15 = change_decisiontable_entry(decision_table15, 12, 4, "E")
#' @details Note that the entries for futility and toxicity indexes can be single values or vectors. However, when changing entries values, this will change all the entries within the bounds of the specified ranges which can lead to unexpected function behavior.
#' For usage safety, please only enter one entry to change at a time. Always make sure to check the smoothed decision table afterwards to make sure the results are as desired.
#' @export
change_decisiontable_entry <- function(decision_table, futility_index, toxicity_index, new_decision) {
  if (sum(!(new_decision %in% c("DU", "D", "S", "E", "EU"))) != 0) {stop('invalid decision, please enter one of the following: "DU", "D", "S", "E", or "EU"')}

  decision_table[futility_index, toxicity_index] = new_decision
  return(decision_table)
}
