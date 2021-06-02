# clinicalMPI
This R package implements the novel modified probability interval (mPI) phase I clinical trial design. 
The mPI design is intended to find the maximum utility dose out of a provided selection. It is useful for treatments where both too low and too high efficacy are actively dangerous to patients, for example in organ transplant mixed-chimerism radiation therapy. 
This design uses a Bayesian interval-based model to make decisions. It is advantageous in that it does not depend on arbitrary physician-elicited input to make decisions and is flexible enough to adapt to unforeseen parameter or sample size changes during a trial. 

The package can be installed and attached using the following commands:

```
library(devtools)
install_github("jiyingz/clinicalMPI")
library(clinicalMPI)
```

The two main functionalities of this package are to 1) pre-calculate decision tables, and 2) run simulations with given parameters to evaluate hypothetical performance results.

The parameters that will need to be provided are the testable dose levels, starting dose level, hypothetical true probabilities of futility, efficacy, and toxicity for all tested doses (the "scenario"), target futility and toxicity probabilities, equivalence interval tolerance levels, highest allowable thresholds, maximum sample size, and cohort size.  Currently the package only supports symmetric equivalence intervals.

An example usage is shown below:

**PRE-CALCULATE DECISION TABLES:**

_Set parameters:_

```
#Params
N = seq(3, 21, by=3)
PF = 0.2
PF_tolerance = 0.05
PT = 0.2
PT_tolerance = 0.05
zeta = 0.8
eta = 0.8
```

The following code calculates a single decision table for a dose sample size of 3:

```
decision_table3 = make_decision_table(3, PF, PF_tolerance, eta, PT, PT_tolerance, zeta)
```

The following code calculates multiple decision tables for dose sample sizes of 3, 6, 9, ..., 21:

```
for(N in seq(3, 21, by=3)) {
    assign(paste("decision_table", N, sep = ""), make_decision_table(N, PF, PF_tolerance, eta, PT, PT_tolerance, zeta))
}
```

Please look at the decision tables and make any necessary adjustments before using, as some boundaries could be rough. For example, to change the decision in `decision_table12` for 3 futilities and 3 toxicities to "Stay", use the following command: 

`decision_table12 = change_decisiontable_entry(decision_table12, futility_index = 3+1, toxicity_index = 3+1, new_decision = "S") # note we need to +1 to the indexing since we start counting events at 0 but datafames are indexed starting at 1.`

In executing real-life trials, it is only necessary to pre-calculate decision tables for the dose-level sample sizes encountered. 
The decision table used should correspond to how many patients have been tried at a dose level over the trial so far. 
The decision-making logic is common to all dose-levels. 


**RUNNING SIMULATIONS:**

_Additional parameters needed:_


```
#Scenario:
true_probs = matrix(data = c(0.95, 0.0, 0.05,
                             0.5, 0.4, 0.1,
                             0.2, 0.65, 0.15,
                             0.1, 0.4, 0.5,
                             0.05, 0.0, 0.95), nrow = 5, ncol = 3, byrow = TRUE)
dose_levels = c(1,2,3,4,5)
starting_dose = 3
cohort_size = 3
max_sample_size = 24 #(default)
num_sims = 1000 #(default)
```
The following code outputs the results from running a simulation with the given parameters:

```
run_simulation(true_probs, dose_levels, starting_dose, PF, PF_tolerance, eta, PT, PT_tolerance, zeta)
```


Additional functions are available to experiment with customized functionality.

