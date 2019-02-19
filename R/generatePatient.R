#' @export
generate_patient <- function(simulation_time = 100, 
                             risk_function = risk_immediate(), 
                             prescription_model = prescription_model_markov_chain(),
                             ade_model = ade_model_no_effect(), 
                             min_chance_drug = probability_constant(.01),
                             max_chance_drug = probability_constant(.5),
                             min_chance_ade = probability_constant(.001), 
                             max_chance_ade = probability_constant(.2), 
                             patient = NULL) { 
  
  # determine first time the drug is prescribed 
  t <- determine_first_prescription(simulation_time, min_chance_drug) 
  
  # initialize the drug and ade time processes 
  drug_prescriptions <- c(rep(0, t - 1), 1, rep(NA, simulation_time - t))
  ade_progression <- c(rbinom(t - 1, 1, min_chance_ade), rep(NA, simulation_time - t + 1))
  
  ade_progression[t] <- update_ade_progression(drug_prescriptions[1:(t-1)],
                                     risk_function, 
                                     min_chance_ade,
                                     max_chance_ade, 
                                     patient) 
  
  if (t == simulation_time) { 
    return(
      list(
        drug_prescriptions = drug_prescriptions,
        ade_progression = ade_progression
      )
    )
  }
  
  # simulate the remaining time points
  sapply((t+1):simulation_time, 
         function(k) {
            drug_prescriptions[k] <<- update_drug_prescription(drug_prescriptions[1:(k-1)], 
                                                               ade_progression[1:(k-1)], 
                                                               prescription_model,
                                                               ade_model, 
                                                               min_chance_drug,
                                                               max_chance_drug, 
                                                               patient)
            ade_progression[k] <<- update_ade_progression(drug_prescriptions[1:k],
                                                          risk_function, 
                                                          min_chance_ade,
                                                          max_chance_ade, 
                                                          patient) 
         }) 
  
  list(
    drug_prescriptions = drug_prescriptions,
    ade_progression = ade_progression
  )
}