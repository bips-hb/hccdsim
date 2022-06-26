##############################################
# drug-history-effect-models.R 
#
# Contains all the possible drug history 
# effect models. A drug history model 
# returns either a 1 when the drug is being 
# prescribed the next time point and 0 otherwise. 
# Whether the drug is prescribed again depends on 
# the drug history. 
##############################################

#' The Drug History Model 'Markov Chain'
#' 
#' \code{drug_model_markov_chain} returns a 
#' drug history model function. The returned
#' function models the drug history time series
#' as a simple Markov chain model. 
#' 
#' @return A drug history model function
#' @family drug history effect models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0)
#' 
#' # returns an ADR model function
#' drug_model <- drug_model_markov_chain() 
#' 
#' # apply the model
#' drug_model(drug_history)
#' @export 
drug_model_markov_chain <- function() { 
  function(drug_history, ...) { 
    if (drug_history[length(drug_history)]) { 
      1
    } else {
      0 
    }
  }
}

#' The drug history Model 'duration'
#' 
#' \code{drug_model_duration} returns a 
#' drug history model function. The returned
#' function models the prescription process such 
#' that the drug is prescribed for a maximum number
#' of time points, i.e., the \code{duration}
#' 
#' @param duration The duration that the drug is prescribed
#' 
#' @return A drug history model function
#' @family drug history effect models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0)
#' 
#' # returns an ADR model function
#' drug_model <- drug_model_duration(3) 
#' 
#' # apply the model
#' drug_model(drug_history)
#' @export 
drug_model_duration <- function(duration) { 
  if (duration < 2) { 
    stop("duration should at least be 2") 
  }

  function(drug_history, ...) { 
    # find last occurrence of no prescription
    if (drug_history[length(drug_history)] == 1) {
      if (sum(drug_history) == length(drug_history)) { 
        duration_current_prescription <- length(drug_history) 
      } else { 
        duration_current_prescription <-
          match(0, rev(drug_history)) - 1
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