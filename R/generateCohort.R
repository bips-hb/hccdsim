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
                            patient_model = patient_model_sex(.5),
                            verbose = FALSE) { 
  
  # initial data 
  drug_prescriptions <- matrix(rep(NA, n_patients * simulation_time), nrow = n_patients)
  ade_progression <- drug_prescriptions
  
  if (verbose) { 
    pb <- txtProgressBar(min = 0, max = n_patients, style = 3)
  }
  
  # generate patients 
  lapply(1:n_patients, function(i) { 
    patient <- generate_patient(simulation_time, 
                                risk_function, 
                                prescription_model, 
                                ade_model, 
                                min_chance_drug, 
                                max_chance_drug,
                                min_chance_ade, 
                                max_chance_ade, 
                                patient = patient_model()) 
    drug_prescriptions[i, ] <<- patient$drug_prescriptions 
    ade_progression[i, ] <<- patient$ade_progression
    
    if (verbose) { 
      setTxtProgressBar(pb, i)
    }
  })
  
  if (verbose) { 
    close(pb) 
  }
  
  list(
    drug_prescriptions = drug_prescriptions,
    ade_progression = ade_progression
  )
}