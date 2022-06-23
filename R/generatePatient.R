#' Generate a Patient 
#' 
#' Generates the drug prescription and 
#' ADE history of an individual patient. 
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
#' @return A \code{patient} object; a list with 
#'       \item{\code{drug_prescriptions}}{binary vector of the 
#'                drug prescription history}
#'       \item{\code{ade_progression}}{binary vector of the 
#'                ade progression}
#'       \item{\code{simulation_time}}{The total number of time steps}
#'       \item{\code{patient_profile}}{A list with patient characteristics}
#'
#' @seealso \code{\link{generate_cohort}}  
#' @examples 
#' # create a patient
#' patient_model <- patient_model_sex(prob_male = 0.5)
#' (patient_profile <- patient_model())
#' 
#' # choose a risk function
#' risk_function <- risk_immediate()
#' 
#' # how the drug is prescribed over time
#' prescription_model <- prescription_model_markov_chain() 
#' 
#' # how the occurence of an ADE affect the prescription of the drug
#' ade_model = ade_model_no_effect()
#' 
#' # minimum and maximal probabilities for the drug being prescribed
#' min_chance_drug <- probability_sex(prob_male = .1, prob_female = .2)
#' max_chance_drug <- probability_sex(prob_male = .5, prob_female = .53)
#' 
#' # minimum and maximal probabilities for the ADE occurring 
#' min_chance_ade <- probability_sex(prob_male = .01, prob_female = .05)
#' max_chance_ade <- probability_sex(prob_male = .2, prob_female = .3)
#' 
#' generate_patient(simulation_time = 100, 
#'                  risk_function, 
#'                  prescription_model,
#'                  ade_model, 
#'                  min_chance_drug,
#'                  max_chance_drug,
#'                  min_chance_ade, 
#'                  max_chance_ade, 
#'                  patient_profile = patient_profile)                
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
  t <- determine_first_prescription(1, simulation_time, min_chance_drug(patient_profile)) 
  
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
  
  res <- list(
    drug_prescriptions = drug_prescriptions,
    ade_progression = ade_progression,
    simulation_time = simulation_time, 
    patient_profile = patient_profile
  )
  class(res) <- "patient"
  return(res)
}

#' Function for printing a patient 
#' @export
print.patient <- function(patient) { 
  cat(sprintf("Patient\n")) 
  if (length(patient$patient_profile) != 0) {
    for (i in 1:length(patient$patient_profile)) { 
      cat(sprintf("\t-- %s: ", names(patient$patient_profile)[i]))
      cat(patient$patient_profile[[i]])
      cat(sprintf("\n"))
    }
  }
  cat(sprintf("\nNo. of time points: %d\n\n", patient$simulation_time))
  
  cat(sprintf("drug: "))
  for(i in 1:patient$simulation_time) { 
    if (patient$drug_prescriptions[i] == 1) { 
       cat(green(1))
    } else { 
      cat(blue("."))
    }
  }
  cat(sprintf("\nADE:  "))
  for(i in 1:patient$simulation_time) { 
    if (patient$ade_progression[i] == 1) { 
      cat(red(1))
    } else { 
      cat(blue("."))
    }
  }
  cat(sprintf("\n"))
}
