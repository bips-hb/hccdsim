#hccdsim: a simulator for longitudinal health care claims data (HCCD)
--------------------------------------------------------

`hccdsim` is a `R` simulator for longitudinal health care claims data. It can be used for simulating large numbers of patients, 
observed over many time points. The package leans heavily on  `R`'s functional programming aspect and makes it easy to specify complex relationships between drug exposures and adverse drug reactions (ADRs) which can depend on numerous aspects of the patients (sex, age, region etc.). 

### Usage

The main function of the package is `generate_cohort` (see `?generate_cohort`). For example, 

```R 
generate_cohort(
		n_patients = 1000, 
		simulation_time = 100,
  		risk_function = risk_immediate(),
  		prescription_model = prescription_model_markov_chain(),
  		ade_model = ade_model_no_effect(),
  		min_chance_drug = probability_constant(0.01),
  		max_chance_drug = probability_constant(0.5),
  		min_chance_ade = probability_constant(0.001),
  		max_chance_ade = probability_constant(0.2),
  		patient_model = patient_model_sex(0.5), 
  		verbose = FALSE)
``` 
simulates a cohort, where 

* `n_patients` is the number of patients simulated in the cohort 

* `simulation_time` is the number of time steps each patient is observed 

* `risk_function` is a function that describes how the risk of suffering the adverse drug reaction (ADR) with the patient's drug exposure (see more details below) 

* `prescription_model` is a function that describes how the drug is prescribed over time 

* `ade_model` is a function on how the occurence of an ADR affects the drug prescriptions or the 
occurence of another ADR in the future 

* `min_chance_drug` is a function that returns the probability of the drug being prescribed when the drug history and the ADE history have no effect

* `max_chance_drug` is a function that returns the probability of the ADE when the drug history and the ADE history have the highest possible effect

* `min_chance_ade` is a function that returns the probability of the ADR occuring when the patient is at the lowest possible risk 

* `max_chance_ade` is a function that returns the probability of the ADR occuring when the patient is at the highest possible risk

* `patient_model` is a function that generates patients with particular characteristics (e.g., sex, age, origin etc). 

Note that this provides the user with the possibility to create very specific models. For example, one can model a situation in which the risk changes over time depended on the person's age or sex. Or where the occurence of the ADR halts the patient's drug exposure (e.g., myocardial infarct). 

### Examples of Risk Functions

See `R/riskFunctions.R` for all. The function `risk_no_effect` models the situation where the drug has no effect on the ADR. `risk_immediate` models a higher risk when the patient is exposed and a baseline risk for when the patient is not exposed. `risk_withdrawal` models withdrawal effects, where the risk increases after the patient stopped with the drug and decreases over time. `risk_long_time_after` models the situation in which the risk increases over time, possibly a long time after the last drug exposure. 

### Examples of Patient Models

See `R/patientModels.R` for all models. For example, a model that generates randomly male and female patients: 

```R
patient_model_sex <- function(prob_male) {
  function() {
    if (rbinom(1, 1, prob_male) == 1) {
      list(sex = "male")
    } else {
      list(sex = "female")
    }
  }
}
```

where `prob_male` is the probability that the patient is male. 

### Example

```R
set.seed(1)

generate_cohort(
		n_patients = 10, 
		simulation_time = 5,
  		risk_function = risk_immediate(),
  		prescription_model = prescription_model_markov_chain(),
  		ade_model = ade_model_no_effect(),
  		min_chance_drug = probability_constant(0.01),
  		max_chance_drug = probability_constant(0.5),
  		min_chance_ade = probability_constant(0.001),
  		max_chance_ade = probability_constant(0.2),
  		patient_model = patient_model_sex(0.5), 
  		verbose = FALSE)
```
returns 

```R
$drug_prescriptions
      [,1] [,2] [,3] [,4] [,5]
 [1,]    0    1    0    0    0
 [2,]    1    0    0    1    1
 [3,]    0    0    0    1    0
 [4,]    0    0    1    1    1
 [5,]    0    0    1    1    1
 [6,]    0    0    0    1    0
 [7,]    0    1    0    0    0
 [8,]    0    0    1    0    0
 [9,]    0    0    0    0    1
[10,]    0    1    0    0    0

$ade_progression
      [,1] [,2] [,3] [,4] [,5]
 [1,]    0    0    0    0    0
 [2,]    0    0    0    0    1
 [3,]    0    0    0    0    0
 [4,]    0    0    0    0    0
 [5,]    0    0    0    0    0
 [6,]    0    0    0    0    0
 [7,]    0    0    0    0    0
 [8,]    0    0    0    0    0
 [9,]    0    0    0    0    0
[10,]    0    0    0    0    0

$patient_profiles
$patient_profiles[[1]]
$patient_profiles[[1]]$sex
[1] "female"


$patient_profiles[[2]]
$patient_profiles[[2]]$sex
[1] "female"


$patient_profiles[[3]]
$patient_profiles[[3]]$sex
[1] "female"


$patient_profiles[[4]]
$patient_profiles[[4]]$sex
[1] "female"


$patient_profiles[[5]]
$patient_profiles[[5]]$sex
[1] "male"


$patient_profiles[[6]]
$patient_profiles[[6]]$sex
[1] "female"


$patient_profiles[[7]]
$patient_profiles[[7]]$sex
[1] "female"


$patient_profiles[[8]]
$patient_profiles[[8]]$sex
[1] "female"


$patient_profiles[[9]]
$patient_profiles[[9]]$sex
[1] "female"


$patient_profiles[[10]]
$patient_profiles[[10]]$sex
[1] "male"
```

which is a cohort with 10 patients, observed over 5 time points. `$drug_prescriptions` returns a binary matrix, where the (i,j)-th entry is 1 if the i-th patient was prescribed the drug at time point j, and 0 otherwise. 

`$ade_progression` is a `10 x 5` binary matrix as well, where the (i,j)-th entry is 1 if the i-th patient experienced the ADR at time point j, and 0 otherwise. 

`$patient_profiles` is a list with all the information of the individual patients (in this case, only the patients' sex). 

### Details 

Due to the models flexibility, it is difficult to provide a complete overview here. See for more details the commentary for the individual functions and the descriptions in the individual R files. 

### Acknowledgements

We gratefully acknowledge the financial support from the innovation fund (“Innovationsfonds”) of the Federal Joint Committee in Germany (grant number: 01VSF16020).

### Contact

Louis Dijkstra\
Leibniz Institute for Prevention Research & Epidemiology  
E-mail: dijkstra (at) leibniz-bips.de