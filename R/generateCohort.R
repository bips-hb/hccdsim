#' Generate a Patient 
#' 
#' Generates an individual patient
#' 
#' @param n_patients Number of patients 
#' @inheritParams generate_patient
#' 
#' @return A list with two matrices: the drug prescriptions and the 
#'         the ADE progression. Each row is a different patient
#'
#' @seealso \code{\link{generate_patient}}                  
#' @export
#' @export
generate_cohort <- function(n_patients = 1000, 
                            simulation_time = 100, 
                            risk_function = risk_immediate(), 
                            prescription_model = prescription_model_markov_chain(),
                            ade_model = ade_model_no_effect(), 
                            min_chance_drug = probability_constant(.01),
                            max_chance_drug = probability_constant(.5),
                            min_chance_ade = probability_constant(.001), 
                            max_chance_ade = probability_constant(.2), 
                            patient_model = patient_model_uninformative(),
                            verbose = FALSE) { 
  
  # initial data 
  drug_prescriptions <- matrix(rep(NA, n_patients * simulation_time), nrow = n_patients)
  ade_progression <- drug_prescriptions
  patient_profiles <- vector(mode = "list", length = n_patients) # list
  
  if (verbose) { 
    pb <- txtProgressBar(min = 0, max = n_patients, style = 3)
  }
  
  # generate patients 
  lapply(1:n_patients, function(i) { 
    patient_profile <- patient_model() # generate the profile of the patient, e.g., sex, age
    patient <- generate_patient(simulation_time, 
                                risk_function, 
                                prescription_model, 
                                ade_model, 
                                min_chance_drug, 
                                max_chance_drug,
                                min_chance_ade, 
                                max_chance_ade, 
                                patient_profile = patient_profile) 
    drug_prescriptions[i, ] <<- patient$drug_prescriptions 
    ade_progression[i, ] <<- patient$ade_progression
    patient_profiles[[i]] <<- patient_profile
    
    if (verbose & i %% 100 == 0) { 
      setTxtProgressBar(pb, i)
    }
  })
  
  if (verbose) { 
    close(pb) 
  }
  
  list(
    drug_prescriptions = drug_prescriptions,
    ade_progression = ade_progression,
    patient_profiles = patient_profiles
  )
}