#' Select Model
#'
#' @description Select the probability region (model) with highest posterior probability, given current results. Note that for generalizability, this function currently calculates probabilities for regions completely falling within the pf+pt <=1 region.
#' @param dose_info Dataframe containing
#'
#' \[dose level | availability | N_d | X_d | Y_d | Z_d | pf | pe | pt\]
#' @param pf_matrix Matrix containing start and end points of futility (pf) intervals
#' @param pt_matrix Matrix containing start and end points of toxicity (pt) intervals
#' @return A list containing:
#'
#' * post_probs -- a matrix of posterior probailities per region (model)
#'
#' * best_pf -- futility interval index for chosen model
#'
#'  * best_pt -- toxicity interval index for chosen model
#' @usage selected = select_highestprob_interval(dose_info, pf_matrix, pt_matrix)
#' best_pf = selected$best_pf
#' best_pt = selected$best_pt
#' @export
select_highestprob_interval <- function(dose_info, pf_matrix, pt_matrix) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  post_probs = matrix(0, nrow = nrow(pf_matrix)-2, ncol = nrow(pt_matrix)-2)

  for (i in 1:(nrow(post_probs)-2)) {
    for (j in 1:(ncol(post_probs)-2)) {

      pf_low = as.numeric(pf_matrix[i,1])
      pf_high = as.numeric(pf_matrix[i,2])
      pt_low = as.numeric(pt_matrix[j,1])
      pt_high = as.numeric(pt_matrix[j,2])

      if(pf_high + pt_high <= 1) {
        post_probs[i,j] = (stats::integrate(
          function(x) {sapply(x,
                              function(x) {stats::integrate(
                                function(y) {trinom_density(x, y, dose_info$X_d, dose_info$Z_d, dose_info$N_d)},
                                pt_low, pt_high)$value})
          }, pf_low, pf_high)$value) / ((pt_high - pt_low) * (pf_high - pf_low))
      } else {
        post_probs[i,j] = 0
      }
    }
  }

  best_rect_ind = which(post_probs == max(post_probs, na.rm = T), arr.ind = T)
  best_pf = best_rect_ind[1]
  best_pt = best_rect_ind[2]

  return(list("post_probs" = post_probs, "best_pf" = best_pf, "best_pt" = best_pt))
}
