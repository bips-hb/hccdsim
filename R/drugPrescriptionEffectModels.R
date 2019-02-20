##############################################
# drugPrescriptionEffectModels.R 
#
# Contains all the possible drug prescription 
# effect models. They model the way in which
# the drug is prescriped over time, given 
# the history of the drug's prescriptions.
##############################################

#' The Drug Prescription Model 'Markov Chain'
#' 
#' The drug prescriptions are modelled as a
#' Markov Chain. 
#' 
#' @return A drug prescription model function
#' @family drug prescription effect models
#' @export 
prescription_model_markov_chain <- function() { 
  function(drug_prescriptions, ...) { 
    if (drug_prescriptions[length(drug_prescriptions)]) { 
      1
    } else {
      0 
    }
  }
}

#' The Drug Prescription Model 'duration'
#' 
#' The drug is prescriped for maximally the 
#' given duration (number of time steps)
#' 
#' @param duration The duration that the drug is prescribed
#' 
#' @return A drug prescription model function
#' @family drug prescription effect models
#' @export 
prescription_model_duration <- function(duration) { 
  if (duration < 2) { 
    stop("duration should at least be 2") 
  }

  function(drug_prescriptions, ...) { 
    # find last occurrence of no prescription
    if (drug_prescriptions[length(drug_prescriptions)] == 1) {
      if (sum(drug_prescriptions) == length(drug_prescriptions)) { 
        duration_current_prescription <- length(drug_prescriptions) 
      } else { 
        duration_current_prescription <-
          match(0, rev(drug_prescriptions)) - 1
      }
      if (duration_current_prescription < duration) {
        1
      } else {
        0
      }
    } else {
      0
    }
  }
}

prescription_model_auto_regressive <- function() { 
  function(drug_prescriptions, p, coeff, ...) { 
    # TODO 
    NA
  }
}