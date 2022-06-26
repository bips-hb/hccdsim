#' Generate a Cohort 
#' 
#' Generates a cohort of patients, each with a 
#' drug prescription and ADR history. 
#' See for more information and details \code{\link{generate_patient}}.
#' 
#' @param n_patients Number of patients 
#' @inheritParams generate_patient
#' 
#' @return A \code{cohort} object; a list with 
#'       \item{\code{n_patients}}{The total number of patients}
#'       \item{\code{simulation_time}}{The total number of time steps}
#'       \item{\code{drug_history}}{A binary matrix with \code{n_patients}
#'             rows and \code{simulation_time} columns}
#'       \item{\code{adr_history}}{A binary matrix with \code{n_patients}
#'             rows and \code{simulation_time} columns}
#'       \item{\code{patient_profile}}{A list with the patient profiles for each patient}
#'
#' @seealso \code{\link{generate_patient}}   
#' @examples 
#' # choose a patient model (in this case based on the patients sex)
#' patient_model <- patient_model_sex(prob_male = 0.5)
#' (patient_profile <- patient_model())
#' 
#' # choose a risk model (effect of the drug is immediate)
#' risk_model <- risk_model_immediate()
#' 
#' # how the drug is prescribed over time
#' drug_model <- drug_model_markov_chain() 
#' 
#' # how the occurence of an ADR affect the prescription of the drug
#' adr_model = adr_model_no_effect()
#' 
#' # minimum and maximal probabilities for the drug being prescribed
#' min_chance_drug <- probability_model_sex(prob_male = .1, prob_female = .2)
#' max_chance_drug <- probability_model_sex(prob_male = .5, prob_female = .53)
#' 
#' # minimum and maximal probabilities for the ADR occurring 
#' min_chance_adr <- probability_model_sex(prob_male = .01, prob_female = .05)
#' max_chance_adr <- probability_model_sex(prob_male = .2, prob_female = .3)
#' 
#' generate_cohort(n_patients = 10, 
#'                simulation_time = 20, 
#'                risk_model      = risk_model, 
#'                drug_model      = drug_model,
#'                adr_model       = adr_model, 
#'                min_chance_drug = min_chance_drug,
#'                max_chance_drug = max_chance_drug,
#'                min_chance_adr  = min_chance_adr, 
#'                max_chance_adr  = max_chance_adr, 
#'                patient_model   = patient_model,
#'                verbose = TRUE) 
#' @export
generate_cohort <- function(n_patients = 100, 
                            simulation_time = 30, 
                            risk_model      = risk_model_immediate(), 
                            drug_model      = drug_model_markov_chain(),
                            adr_model       = adr_model_no_effect(), 
                            min_chance_drug = probability_model_constant(.01),
                            max_chance_drug = probability_model_constant(.5),
                            min_chance_adr  = probability_model_constant(.001), 
                            max_chance_adr  = probability_model_constant(.2), 
                            patient_model   = patient_model_uninformative(),
                            verbose = FALSE) { 
  
  # initial data 
  drug_history <- matrix(rep(NA, n_patients * simulation_time), nrow = n_patients)
  adr_history <- drug_history
  patient_profiles <- vector(mode = "list", length = n_patients) # list
  
  if (verbose) { 
    pb <- txtProgressBar(min = 0, max = n_patients, style = 3)
  }
  
  # generate patients 
  lapply(1:n_patients, function(i) { 
    patient_profile <- patient_model() # generate the profile of the patient, e.g., sex, age
    patient <- generate_patient(simulation_time, 
                                risk_model, 
                                drug_model, 
                                adr_model, 
                                min_chance_drug, 
                                max_chance_drug,
                                min_chance_adr, 
                                max_chance_adr, 
                                patient_profile = patient_profile) 
    drug_history[i, ] <<- patient$drug_history 
    adr_history[i, ] <<- patient$adr_history
    patient_profiles[[i]] <<- patient_profile
    
    if (verbose & i %% 100 == 0) { 
      setTxtProgressBar(pb, i)
    }
  })
  
  if (verbose) { 
    close(pb) 
  }
  
  res <- list(
    n_patients = n_patients, 
    simulation_time = simulation_time,  
    drug_history = drug_history,
    adr_history = adr_history,
    patient_profiles = patient_profiles
  )
  class(res) <- "cohort"
  return(res)
}

#' Print function for \code{\link{generate_cohort}}
#' @export
print.cohort <- function(cohort) { 
  cat(sprintf("No. patients: %d     No. of time points: %d\n\n", 
              cohort$n_patients,
              cohort$simulation_time))
  
  for (i in 1:cohort$n_patients) { 
    cat(sprintf("patient %d   drugs: ", i)) 
    for(t in 1:cohort$simulation_time) {
      if (cohort$drug_history[i, t] == 1) {
        cat(green(1))
      } else {
        cat(blue("."))
      }
    }
    cat(sprintf("\npatient %d   ADR:   ", i))
    for(t in 1:cohort$simulation_time) { 
        if (cohort$adr_history[i, t] == 1) {
          cat(red(1))
        } else {
          cat(blue("."))
        }
    }
    cat(sprintf("\n\n")) 
  }
}