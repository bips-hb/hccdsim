##############################################
# riskFunctions.R 
#
# Contains all the risk models. 
# They model how the risk of suffering the ADE
# changes with the history of drug prescriptions
# of the patient. 
# The risk functions return values between 0 and 
# 1. Zero is minimal risk, 1 is full risk. 
##############################################

#' Risk Model 'No Effect'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADE changes the history of 
#' drug prescriptions of the patient. 
#' It returns 0 when the drug prescription history
#' has no effect, and 1 when the patient is at 
#' maximal risk.  
#' In this case, the drug has no effect on the ADE
#' what so ever.
#' 
#' @return A risk function
#' @family Risk models
#' @export
risk_no_effect <- function() { 
  function(drug_prescriptions, ...) { 
    0 
  }
}

#' Risk Model 'Immediate'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADE changes the history of 
#' drug prescriptions of the patient. 
#' It returns 0 when the drug prescription history
#' has no effect, and 1 when the patient is at 
#' maximal risk.  
#' In this case, the probability of the ADE 
#' is maximal only when the patient is currently
#' exposed.
#' 
#' @return A risk function
#' @family Risk models
#' @export
risk_immediate <- function() {
  function(drug_prescriptions, ...) { 
    if (drug_prescriptions[length(drug_prescriptions)]) { 
      1  
    } else {
      0  
    }
  }
}

#' Risk Model 'Withdrawal'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADE changes the history of 
#' drug prescriptions of the patient. 
#' It returns 0 when the drug prescription history
#' has no effect, and 1 when the patient is at 
#' maximal risk.  
#' In this case, once the patient is no longer exposed
#' to the drug, the probability of the ADE peaks 
#' and then dissipates exponentially. 
#' 
#' @param rate The rate with which the risk dissipates
#' 
#' @return A risk function
#' @family Risk models
#' @export
risk_withdrawal <- function(rate) {
  function(drug_prescriptions, ...) { 
    # if case the drug was never prescribed or the drug is 
    # currently prescribed 
    if (!any(as.logical(drug_prescriptions)) | drug_prescriptions[length(drug_prescriptions)] == 1) { 
      0
    } else {
      # determine how long ago it was prescribed 
      time_steps_ago = length(drug_prescriptions) - max(which(drug_prescriptions == 1)) 
      exp(-rate * (time_steps_ago - 1)) 
    }
  }
}

#' Risk Model 'Long Time After'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADE changes the history of 
#' drug prescriptions of the patient. 
#' It returns 0 when the drug prescription history
#' has no effect, and 1 when the patient is at 
#' maximal risk.  
#' In this case, the risk of the ADE increases
#' only after a certian moment in time before it 
#' reaches the maximal risk.
#' 
#' @param rate The rate with which the risk increases
#' @param delay How long it takes before the risk is .5
#' 
#' @return A risk function
#' @family Risk models
#' @export
risk_long_time_after <- function(rate, delay) {
  function(drug_prescriptions, ...) { 
    
    # if case the drug was never prescribed or the drug is 
    # currently prescribed 
    if (!any(as.logical(drug_prescriptions))) { 
      0
    } else { 
      # moment of first prescription
      time_since_first_prescription = length(drug_prescriptions) - min(which(drug_prescriptions == 1)) 
    
      # use a sigmoid function to determine the effect
      1 / (1 + exp(-rate * (time_since_first_prescription - delay)))
    }
  }
}

#' Risk Model 'Increase/Decrease'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADE changes the history of 
#' drug prescriptions of the patient. 
#' It returns 0 when the drug prescription history
#' has no effect, and 1 when the patient is at 
#' maximal risk.  
#' In this case, the risk first increases linearly and 
#' reaches its peak after \code{peak} time points. Then 
#' it goes down at the same rate before it is zero again. 
#' 
#' @param peak The point at which the risk peaks
#' 
#' @return A risk function
#' @family Risk models
#' @export
risk_increase_decrease <- function(peak) { 
  
  if (peak < 2) { 
    stop("peak should be at least 2") 
  }
  
  function(drug_prescriptions, ...) { 
    # if (length(drug_prescriptions) == 1 & drug_prescriptions[1] == 1) { 
    #   return(0)  
    # }
    
    drug_prescriptions <- c(0, drug_prescriptions)
    
    if (sum(drug_prescriptions) > 0) { # prescribed at least once
      
      # time point when the drug was prescribed last
      time_last_prescription <- rev(which(drug_prescriptions == 1))[1]
      # how long ago the last prescription lasted
      time_since_last_prescription <- length(drug_prescriptions) - time_last_prescription
      # how long was the drug prescribed last time
      
      #if (length(drug_prescriptions) == sum(drug_prescriptions)) { 
       # duration <- length(drug_prescriptions) 
      #} else { 
        duration <- match(0, rev(drug_prescriptions[1:time_last_prescription])) - 1
      #}
      # whether the drug is currently prescribed or not
      currently_prescribed <- drug_prescriptions[length(drug_prescriptions)]
      
      if (currently_prescribed) { 
        if (duration <= peak) { # before/at the point where the risk peaks
          (duration-1) / (peak-1)
        } else { 
          max(0, 1 - (duration - peak) / (peak - 1))
        }
      } else { # not currently prescribed 
        if (duration > peak) { 
          max(0, 1 - (duration + time_since_last_prescription - peak)/(peak-1))   
        } else { 
        max(0, (duration-1)/(peak-1) - (time_since_last_prescription)/(peak-1))
        }
      }
    } else { # not prescribed 
      0
    }
  }
}



