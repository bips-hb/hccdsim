#' Generate a Patient 
#' 
#' Generates the drug- and 
#' ADR history of an individual patient. 
#' 
#' @param simulation_time The total number of time steps
#' @param risk_model One of the risk models
#' @param drug_model One of the drug models
#' @param adr_model One of the ADR effect models
#' @param min_chance The probability of the ADR when 
#'            the drug history has no effect 
#' @param max_chance The probability of the ADR when 
#'            the drug history has the highest possible effect
#' @param min_chance_drug The probability of the drug being prescribed
#'            when the drug history and the ADR history have no effect
#' @param max_chance_drug The probability of the ADR when 
#'            the drug history and the ADR history have the highest 
#'            possible effect
#' @param patient A list with the covariates of the patient (Default: \code{NULL})
#' 
#' @return A \code{patient} object; a list with 
#'       \item{\code{drug_history}}{binary vector of the 
#'                drug history}
#'       \item{\code{adr_history}}{binary vector of the 
#'                ADR history}
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
#' risk_model <- risk_immediate()
#' 
#' # how the drug is prescribed over time
#' drug_model <- drug_model_markov_chain() 
#' 
#' # how the occurence of an ADR affect the prescription of the drug
#' adr_model = adr_model_no_effect()
#' 
#' # minimum and maximal probabilities for the drug being prescribed
#' min_chance_drug <- probability_sex(prob_male = .1, prob_female = .2)
#' max_chance_drug <- probability_sex(prob_male = .5, prob_female = .53)
#' 
#' # minimum and maximal probabilities for the ADR occurring 
#' min_chance_adr <- probability_sex(prob_male = .01, prob_female = .05)
#' max_chance_adr <- probability_sex(prob_male = .2, prob_female = .3)
#' 
#' generate_patient(simulation_time = 100, 
#'                  risk_model, 
#'                  drug_model,
#'                  adr_model, 
#'                  min_chance_drug,
#'                  max_chance_drug,
#'                  min_chance_adr, 
#'                  max_chance_adr, 
#'                  patient_profile = patient_profile)                
#' @export
generate_patient <- function(simulation_time = 100, 
                             risk_model = risk_model_immediate(), 
                             drug_model = drug_model_markov_chain(),
                             adr_model = adr_model_no_effect(), 
                             min_chance_drug = probability_constant(.01),
                             max_chance_drug = probability_constant(.5),
                             min_chance_adr = probability_constant(.001), 
                             max_chance_adr = probability_constant(.2), 
                             patient_profile = NULL) { 
  
  # determine first time the drug is prescribed 
  t <- determine_first_prescription(1, simulation_time, min_chance_drug(patient_profile)) 
  
  # initialize the drug and ADR time processes 
  drug_history <- c(rep(0, t - 1), 1, rep(NA, simulation_time - t))
  adr_history <- c(rbinom(t - 1, 1, min_chance_adr(patient_profile)), rep(NA, simulation_time - t + 1))
  
  adr_history[t] <- update_adr_history(drug_history[1:(t-1)],
                                     risk_model, 
                                     min_chance_adr,
                                     max_chance_adr, 
                                     patient_profile) 
  
  if (t == simulation_time) { 
    return(
      list(
        drug_history = drug_history,
        adr_history = adr_history
      )
    )
  }
  
  # simulate the remaining time points
  sapply((t+1):simulation_time, 
         function(k) {
            drug_history[k] <<- update_drug_prescription(drug_history[1:(k-1)], 
                                                         adr_history[1:(k-1)], 
                                                         drug_model,
                                                         adr_model, 
                                                         min_chance_drug,
                                                         max_chance_drug, 
                                                         patient_profile)
            adr_history[k] <<- update_adr_history(drug_history[1:k],
                                                  risk_model, 
                                                  min_chance_adr,
                                                  max_chance_adr, 
                                                  patient_profile) 
         }) 
  
  res <- list(
    drug_history = drug_history,
    adr_history = adr_history,
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
    if (patient$drug_history[i] == 1) { 
       cat(green(1))
    } else { 
      cat(blue("."))
    }
  }
  cat(sprintf("\nADR:  "))
  for(i in 1:patient$simulation_time) { 
    if (patient$adr_history[i] == 1) { 
      cat(red(1))
    } else { 
      cat(blue("."))
    }
  }
  cat(sprintf("\n"))
}
