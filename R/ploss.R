#' Calculate Loss (1-Utility)
#'
#' @description Calculates loss for a given value with target and tolerance levels. Can be used for toxicity or futility.
#' Loss = 0 if value below target - tolerance, 1 if above target + tolerance, and linear from 0 to 1 between.
#'
#' @param value Estimated probability of futility/toxicity
#' @param target Target chance of futility/toxicity
#' @param tolerance Half-length of the equivalence interval
#' @return Calculated numeric loss for given value
#' @usage ploss(value = 0.18, target = 0.2, tolerance = 0.05) #calculate loss for 0.18 when equivalence interval is [0.15,0.25]
ploss <- function(value, target, tolerance) {
  loss = 0

  if(value >= target-tolerance & value < target+tolerance) {
    loss = (value - (target-tolerance)) * (1 / (tolerance*2)) #second part is slope
  }

  if (value >= target+tolerance) {
    loss = 1
  }

  return(loss)
}
