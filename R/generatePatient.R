#' Generate a Patient 
#' 
#' Generates an individual patient
#' 
#' @param simulation_time The total number of time steps
#' @param risk_function One of the risk models
#' @param prescription_model One of the prescription models
#' @param ade_model One of the ADE effect models
#' @param min_chance The probability of the ADE when 
#'            the drug history has no effect 
#' @param max_chance The probability of the ADE when 
#'            the drug history has the highest possible effect
#' @param min_chance_drug The probability of the drug being prescribed
#'            when the drug history and the ADE history have no effect
#' @param max_chance_drug The probability of the ADE when 
#'            the drug history and the ADE history have the highest 
#'            possible effect
#' @param patient A list with the covariates of the patient (Default: \code{NULL})
#' 
#' @return A list with a vector of the drug prescriptions and the 
#'         the ADE progression
#'
#' @seealso \code{\link{generate_cohort}}                  
#' @export
generate_patient <- function(simulation_time = 100, 
                             risk_function = risk_immediate(), 
                             prescription_model = prescription_model_markov_chain(),
                             ade_model = ade_model_no_effect(), 
                             min_chance_drug = probability_constant(.01),
                             max_chance_drug = probability_constant(.5),
                             min_chance_ade = probability_constant(.001), 
                             max_chance_ade = probability_constant(.2), 
                             patient_profile = NULL) { 
  
  # determine first time the drug is prescribed 
  t <- determine_first_prescription(simulation_time, min_chance_drug(patient_profile)) 
  
  # initialize the drug and ade time processes 
  drug_prescriptions <- c(rep(0, t - 1), 1, rep(NA, simulation_time - t))
  ade_progression <- c(rbinom(t - 1, 1, min_chance_ade(patient_profile)), rep(NA, simulation_time - t + 1))
  
  ade_progression[t] <- update_ade_progression(drug_prescriptions[1:(t-1)],
                                     risk_function, 
                                     min_chance_ade,
                                     max_chance_ade, 
                                     patient_profile) 
  
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
                                                               patient_profile)
            ade_progression[k] <<- update_ade_progression(drug_prescriptions[1:k],
                                                          risk_function, 
                                                          min_chance_ade,
                                                          max_chance_ade, 
                                                          patient_profile) 
         }) 
  
  list(
    drug_prescriptions = drug_prescriptions,
    ade_progression = ade_progression,
    patient_profile = patient_profile
  )
}