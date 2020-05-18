#' Run Simulation
#'
#' @description Run simulations to get performance characteristics with given parameters. Performance outcomes are averaged over 1000 simulation trials by default.
#' @param true_probs A Dx3 matrix containing the true chances of treatment futility, efficacy, and toxicity, where D = # of dose levels
#' @param dose_levels A length D vector containing dose levels to be included in the trial. Can be in index or true value form (e.g. could be either c(1,2,3,4,5) or c(40,60,80,100,120)).
#' @param starting_dose Which dose to start at, must be a value within `dose_levels`
#' @param PF Target tolerable futility probability
#' @param PF_tolerance Half-length of futility equivalence interval (epsilon_F)
#' @param eta Upper futility threshold beyond which invokes the futility rule
#' @param PT Target tolerable toxicity probability
#' @param PT_tolerance Half-length of toxicity equivalence interval (epsilon_T)
#' @param zeta Upper toxicity threshold beyond which invokes the safety rule
#' @param cohort_size How many new patients to test in each round of the trial
#' @param max_samp_size The maximum number of subjects for the entire trial (default = 24)
#' @param num_sims How many trials to perform in simulation (default = 1000)
#' @param filter Posterior sum threshold to determine dose availability for final selection (pf+pt <= filter) (default = 0.5)
#' @param seed A random seed (default = 111)
#' @return A list containing:
#'
#' * prop_time_selected -- Percent of trials where each dose was selected as the final dose recommendation
#'
#' * times_early_stopping -- How many trials stopped before reaching maximum sample size
#'
#' * mean_patients_perdose -- Mean number of patients allocated to each dose level
#'
#' * min_patients_perdose -- Minimum number of patients allocated to each dose level
#'
#' * max_patients_perdose -- Maximum number of patients allocated to each dose level
#' @usage library(stats)
#' true_probs = matrix(data = c(0.95, 0.0, 0.05,
#'                              0.5, 0.4, 0.1,
#'                              0.2, 0.65, 0.15,
#'                              0.1, 0.4, 0.5,
#'                              0.05, 0.0, 0.95), nrow = 5, ncol = 3, byrow = TRUE)
#'
#' dose_levels = c(60,80,100,120,140)
#' starting_dose = 100
#' PF = 0.2
#' PF_tolerance = 0.05
#' eta = 0.8
#' PT = 0.2
#' PT_tolerance = 0.05
#' zeta = 0.8
#' #cohort size is 3, max sample size is 24, using defaults
#'
#' results = run_simulation(true_probs, dose_levels, starting_dose, PF, PF_tolerance, eta, PT, PT_tolerance, zeta)
#' @details In order to run simulations, decision tables must be pre-specified (e.g. by using the `make_decision_table()` function) and loaded into the working environment.
#' All decision tables must be named in the format "decision_table#", where # = dose sample size.
#'
#' For simulations the cohort size must be a fixed value, although this may not necessarily be true in real-life trials.
#'
#' Simulations are meant to give an idea of how variable trial results may be given the chosen parameters, and is not to be taken as a statistical guarantee of any sort.
#' Results depend on the assumptions that
#'
#' 1) the "true" probabilities of futility and toxicity are valid, and
#'
#' 2) no unexpected cohort additions or dropouts occur throughout the trial.
#'
#' These assumptions are difficult to adhere to in real-life scenarios, and so the actual performance characteristics may differ from those outputted by the simulation.
#' In other words, take the simulation performance results with a grain of salt. However, the closer the real-life trial adheres to the ideal simulation conditions,
#' and the more sure we are about the "true" chances of toxicity and futility, the closer the actual outcome will adhere to what is expected from simulation results!
#' For example, if simulation results show that the correct dose is chosen in 73% of trials, then in one real-life trial there is a 73% chance that the trial outcomes will point to a truly acceptable dose.

run_simulation <- function(true_probs, dose_levels, starting_dose, PF, PF_tolerance, eta, PT, PT_tolerance, zeta, cohort_size = 3, max_samp_size = 24, num_sims = 1000, filter = 0.5, seed = 111) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  set.seed(seed)
  true_probs = true_probs
  rownames(true_probs) = dose_levels

  intervals = make_intervals(PF, PF_tolerance, eta, PT, PT_tolerance, zeta)
  pf_matrix = intervals$pf_matrix
  pt_matrix = intervals$pt_matrix

  #LOGGING
  #Keep track of number of patients allocated to doses for each trial simulation
  num_patients_per_dose_lvl = matrix(0, nrow = nrow(true_probs), ncol = num_sims)
  rownames(num_patients_per_dose_lvl) = dose_levels

  #Keep track of the number of times each final dose was selected
  times_selected = matrix(0, nrow = nrow(true_probs) + 1, ncol = 1)
  rownames(times_selected) = c(-99, dose_levels)

  #SIMULATIONS
  print("Starting simulations...")
  for(i in 1:num_sims) {
    if (i %% 20 == 0) { print(paste("Sim", i)) }

    sample_size = 0
    decision = NA
    dose_info = setup(dose_levels)
    dose = starting_dose
    stopping_rule = check_stopping_rules(dose, dose_info, sample_size, max_samp_size)

    while (stopping_rule == FALSE) {
      sample_size = sample_size + cohort_size

      #Get cohort outcomes from draw of corresponding trinomial distn
      dose_ind = which(as.numeric(rownames(true_probs)) == dose)
      a = true_probs[dose_ind,1]
      b = true_probs[dose_ind,2]
      c = true_probs[dose_ind,3]
      outcomes = stats::rmultinom(n=1, size = 3, prob = c(a,b,c))
      dose_info = update_table(dose, dose_info, outcomes[1], outcomes[2], outcomes[3])
      num_patients_per_dose_lvl[dose_ind, i] = num_patients_per_dose_lvl[dose_ind, i] + cohort_size #logging

      #Check and make adjustments for safety/futility rules
      safety_rule_invoked = check_safety_rule(dose, dose_info, zeta)
      futility_rule_invoked = check_futility_rule(dose, dose_info, eta)
      if (safety_rule_invoked) {
        dose_info = adjust_for_safety_rule(dose, dose_info)
      } else if (futility_rule_invoked) {
        dose_info = adjust_for_futility_rule(dose, dose_info)
      }

      if (sample_size < max_samp_size) {
        #Make action decision
        if ((safety_rule_invoked | futility_rule_invoked) & dose_info$N_d[dose_ind] < 6) {
          decision = "S"
        } else {
          decision_table = find_decision_table(dose_info$N_d[dose_ind])
          decision = decision_table[dose_info$X_d[dose_ind]+1, dose_info$Z_d[dose_ind]+1]
          if(is.na(decision)) {
            print(dose_info$N_d[dose_ind])
            print(dose_info)
            print(dose_ind)
            print(decision_table)
          }
        }

        #Choose next dose
        if (dose_ind == 1) {
          closest_lower_dose = -Inf
        } else {
          closest_lower_dose = max(which(dose_info$available[1:dose_ind-1])) #-Inf if none available
        }

        if (dose_ind == nrow(dose_info)) {
          closest_higher_dose = Inf
        } else {
          closest_higher_dose = dose_ind + min(which(dose_info$available[(dose_ind+1):nrow(dose_info)])) #Inf if none available
        }

        if (decision == "D") {
          if (closest_lower_dose == -Inf) { dose = dose }
          else { dose = dose_info$dose_lvl[closest_lower_dose] }
        } else if (decision == "S") {
          dose = dose
        } else if (decision == "E") {
          if (closest_higher_dose == Inf) { dose = dose }
          else { dose = dose_info$dose_lvl[closest_higher_dose] }
        } else if (decision == "DU" & closest_lower_dose != -Inf) {
          dose = dose_info$dose_lvl[closest_lower_dose]
        } else if (decision == "EU" & closest_higher_dose != Inf) {
          dose = dose_info$dose_lvl[closest_higher_dose]
        }
      }

      stopping_rule = check_stopping_rules(dose, dose_info, sample_size + cohort_size, max_samp_size, decision, closest_lower_dose, closest_higher_dose)
      if(stopping_rule == TRUE) { break }
    }

    #Select final dose
    final_dose = select_final_dose(dose_info, sample_size, max_samp_size, stopping_rule, filter)
    ind = which(as.numeric(rownames(times_selected)) == final_dose)
    if(ind == -99){print(ind)}
    times_selected[ind,1] = times_selected[ind,1] + 1 #logging

  }

  #RESULTS
  prop_time_selected = 100 * (times_selected/sum(times_selected))
  rownames(prop_time_selected)[1] = "No dose selected"
  times_early_stopping = length(which(colSums(num_patients_per_dose_lvl) < max_samp_size))
  mean_patients_perdose = 100 * apply(sweep(num_patients_per_dose_lvl, 2, colSums(num_patients_per_dose_lvl), '/'), 1, mean)
  min_patients_perdose = 100 * apply(sweep(num_patients_per_dose_lvl, 2, colSums(num_patients_per_dose_lvl), '/'), 1, min)
  max_patients_perdose = 100 * apply(sweep(num_patients_per_dose_lvl, 2, colSums(num_patients_per_dose_lvl), '/'), 1, max)

  return(list(prop_time_selected = prop_time_selected, times_early_stopping = times_early_stopping, mean_patients_perdose = mean_patients_perdose, min_patients_perdose = min_patients_perdose, max_patients_perdose = max_patients_perdose))
}
