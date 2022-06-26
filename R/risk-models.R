##############################################
# risk-models.R 
#
# Contains all the risk models. 
# They model how the risk of suffering the ADR
# changes with the history of drug history
# of the patient. 
# The risk models return values between 0 and 
# 1. Zero is minimal risk, 1 is full risk. 
##############################################

#' Risk Model 'No Effect'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADR changes with the drug exposures
#' of a patient. It returns \code{0} when the drug 
#' prescription history has no effect, and \code{1} 
#' when the patient is at maximal risk.  
#' In this case, the drug has no effect on the ADR
#' what so ever.
#' 
#' @return A risk model
#' @family Risk models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0)
#' risk_model <- risk_model_no_effect() 
#' risk_model() 
#' # -> 0
#' @export
risk_model_no_effect <- function() { 
  function(drug_history, ...) { 
    0 # always lowest risk
  }
}

#' Risk Model 'Immediate'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADR changes with the drug exposures
#' of a patient. It returns \code{0} when the drug 
#' prescription history has no effect, and \code{1} 
#' when the patient is at maximal risk.  
#' In this case, the probability of the ADR 
#' is maximal only when the patient is currently
#' exposed.
#' 
#' @return A risk model
#' @family Risk models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0)
#' risk_model <- risk_model_immediate() 
#' risk_model(drug_history) 
#' @export
risk_model_immediate <- function() {
  function(drug_history, ...) { 
    if (drug_history[length(drug_history)]) { 
      1  # highest risk
    } else {
      0  # lowest risk
    }
  }
}

#' Risk Model 'Withdrawal'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADR changes with the drug exposures
#' of a patient. It returns \code{0} when the drug 
#' prescription history has no effect, and \code{1} 
#' when the patient is at maximal risk.
#' In this case, once the patient is no longer exposed
#' to the drug, the probability of the ADR peaks 
#' and then dissipates exponentially. 
#' Let \eqn{\tau} be the number of time points since
#' the patient was prescribed the drug last. The return
#' value is the given by 
#' \deqn{\exp(-\gamma \cdot (\tau - 1))} 
#' where \eqn{\gamma} is \code{rate}.
#' 
#' @param rate The rate with which the risk dissipates
#' 
#' @return A risk model
#' @family Risk models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0)
#' 
#' risk_model <- risk_model_withdrawal(rate = 1.2) 
#' risk_model(drug_history) 
#' @export
risk_model_withdrawal <- function(rate) {
  
  # check correctness input
  if (rate <= 0) { 
    stop("rate should be > 0") 
  }
  
  function(drug_history, ...) { 
    # if case the drug was never prescribed or the drug is 
    # currently prescribed 
    if (!any(as.logical(drug_history)) | drug_history[length(drug_history)] == 1) { 
      0
    } else {
      # determine how long ago it was prescribed 
      time_steps_ago = length(drug_history) - max(which(drug_history == 1)) 
      exp(-rate * (time_steps_ago - 1)) 
    }
  }
}

#' Risk Model 'Long Time After'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADR changes with the drug exposures
#' of a patient. It returns \code{0} when the drug 
#' prescription history has no effect, and \code{1} 
#' when the patient is at maximal risk.
#' In this case, the risk of the ADR increases
#' only after a certain moment in time before it 
#' reaches the maximal risk.
#' Let \eqn{t_0} be the time point that the drug
#' was prescribed for the first time, and \eqn{\delta}
#' be the number of time points since first description
#' that the function returns 0.5 ("half way"). The 
#' return value is 
#' \deqn{1 / (1 + \exp(-\gamma \cdot (t_0 - \delta)))} 
#' where \eqn{\gamma} is \code{rate}. Note that this 
#' is a sigmoid function.  
#' 
#' @param rate The rate with which the risk increases
#' @param delay How long it takes before the risk is .5
#' 
#' @return A risk model
#' @family Risk models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1)
#' 
#' risk_model <- risk_model_long_time_after(rate = .5, delay = 9) 
#' risk_model(drug_history) 
#' @export
risk_model_long_time_after <- function(rate, delay) {
  function(drug_history, ...) { 
    
    # if case the drug was never prescribed or the drug is 
    # currently prescribed 
    if (!any(as.logical(drug_history))) { 
      0
    } else { 
      # moment of first prescription
      time_since_first_prescription = length(drug_history) - min(which(drug_history == 1)) 
    
      # use a sigmoid function to determine the effect
      1 / (1 + exp(-rate * (time_since_first_prescription - delay)))
    }
  }
}

#' Risk Model 'Increase/Decrease'
#' 
#' A risk model reflects how the probability of 
#' suffering the ADR changes with the drug exposures
#' of a patient. It returns \code{0} when the drug 
#' prescription history has no effect, and \code{1} 
#' when the patient is at maximal risk. 
#' In this case, the risk first increases linearly and 
#' reaches its peak after \code{peak} time points. Then 
#' it goes down at the same rate before it hits zero again. 
#' 
#' @param peak The point at which the risk peaks
#' 
#' @return A risk model
#' @family Risk models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1)
#' 
#' risk_model <- risk_model_increase_decrease(peak = 3) 
#' risk_model(drug_history) 
#' @export
risk_model_increase_decrease <- function(peak) { 
  
  if (peak < 2) { 
    stop("peak should be at least 2") 
  }
  
  function(drug_history, ...) { 
    
    drug_history <- c(0, drug_history)
    
    if (sum(drug_history) > 0) { # prescribed at least once
      
      # time point when the drug was prescribed last
      time_last_prescription <- rev(which(drug_history == 1))[1]
      # how long ago the last prescription lasted
      time_since_last_prescription <- length(drug_history) - time_last_prescription

      duration <- match(0, rev(drug_history[1:time_last_prescription])) - 1
      
      # whether the drug is currently prescribed or not
      currently_prescribed <- drug_history[length(drug_history)]
      
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