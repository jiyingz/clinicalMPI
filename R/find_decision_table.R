#' Find Decision Table
#'
#' @description Given the dose sample size, choose the right (pre-calculated) decision table to get decision from.
#' Decision tables are created using `make_decision_table()` and must be named "decision_table#" where #=N.
#' Variable is read from current environment, so decision table must be loaded.
#' @param dose_samp_size Sample size to extract decision table for
#' @return A matrix containing the correct decision table
#' @usage decision_table = find_decision_table(15) #returns `decision_table15` into `decision_table` variable
#' @export
find_decision_table <- function(dose_samp_size) {
  return(eval(parse(text = paste("decision_table", dose_samp_size, sep = ""))))
}
