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