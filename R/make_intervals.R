#' Probability Intervals (Regions) Setup
#'
#' @description Make probability intervals given the target futility and toxicity level threshold parameters
#' @param PF Target tolerable futility probability
#' @param PF_tolerance Half-length of futility equivalence interval (epsilon_F)
#' @param eta Upper futility threshold beyond which invokes the futility rule
#' @param PT Target tolerable toxicity probability
#' @param PT_tolerance Half-length of toxicity equivalence interval (epsilon_T)
#' @param zeta Upper toxicity threshold beyond which invokes the safety rule
#' @return A list containing:
#'
#' * pf_matrix -- matrix containing start and end points of p_f intervals
#'
#' * pt_matrix -- Matrix containing start and end points of p_t intervals
#' @usage intervals = make_intervals(PF=0.2, PF_tolerance=0.05, eta=0.8, PT=0.2, PT_tolerance=0.05, zeta=0.8)
#'
#' pf_matrix = intervals$pf_matrix
#'
#' pt_matrix = intervals$pt_matrix
#' @export
make_intervals <- function(PF, PF_tolerance, eta, PT, PT_tolerance, zeta) {
  if (PF_tolerance < 1e-5) {stop('PF_tolerance too small, estimates may be unstable')}
  if (PT_tolerance < 1e-5) {stop('PT_tolerance too small, estimates may be unstable')}
  if (PF < 0 | PT < 0 | eta > 1 | zeta > 1) {stop('param out of acceptable bounds')}
  if (eta < PF) {stop('eta cannot be smaller than PF')}
  if (zeta < PT) {stop('zeta cannot be smaller than PT')}

  for (round in 1:2) {
    interval_start = c()
    if (round == 1) { #making pf intervals
      low = PF-PF_tolerance
      high = PF+PF_tolerance
      int_length = PF_tolerance * 2
    }
    else { #making pt intervals
      low = PT-PT_tolerance
      high = PT+PT_tolerance
      int_length = PT_tolerance * 2
    }

    continue = TRUE
    while (continue) { #add lower intervals
      if (low <= 0) {
        low = 0
        continue = FALSE
      }
      interval_start = c(low, interval_start)
      low = low - int_length
    }

    continue = TRUE
    while (continue) { #add higher intervals
      if (round == 1 & high > eta & high - int_length < eta) {
        high = eta
      }
      if (round == 2 & high > zeta & high - int_length < zeta) {
        high = zeta
      }
      if (high >= 1) {
        continue = FALSE
        break
      }
      interval_start = c(interval_start, high)
      high = high + int_length
    }

    interval_end = c(interval_start[2:length(interval_start)], 1)

    if (round == 1) { #making pf matrix
      pf_matrix = cbind(interval_start, interval_end)
    }
    else { #making pt matrix
      pt_matrix = cbind(interval_start, interval_end)
    }
  }

  return(list("pf_matrix" = pf_matrix, "pt_matrix" = pt_matrix))
}
