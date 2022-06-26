##############################################
# adr-effect-models.R 
#
# Contains all the possible ADR effect models. 
# They model which effect the appearance of the 
# ADR has on the prescription of the drug. 
##############################################

#' The ADR model 'no effect'
#' 
#' \code{adr_model_no_effect} models the effect of 
#' the appearance of an ADR on the drug prescriptions 
#' of patient. In this case, the appearance of an ADR 
#' has no effect on the prescription of a drug. 
#' 
#' @return An ADR model function
#' @family ADR effect models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0)
#' adr_history <- c(0, 0, 1, 1, 0) 
#' 
#' # returns An ADR model function
#' adr_model <- adr_model_no_effect() 
#' 
#' adr_model(adr_history, drug_history)
#' # -> 1
#' @export 
adr_model_no_effect <- function() { 
  function(...) { 
    1 
  }
}

#' The ADR model 'full stop'
#' 
#' \code{adr_model_full_stop} models the effect of 
#' the appearance of an ADR on the drug prescriptions 
#' of patient. In this case, the appearance of an ADR 
#' stops any future drug prescription. This can, for example, 
#' reflect that the ADR is so severe, that the drug 
#' is never prescribed again.
#' 
#' @param drug_adr_both If \code{TRUE}, the prescriptions only stop 
#'                      when the ADR takes place when the patient
#'                      is prescribed the drug at the same time step 
#'                      (Default: \code{FALSE})
#' 
#' @return An ADR model function
#' @family ADR effect models
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0)
#' adr_history <- c(0, 0, 1, 1, 0) 
#' 
#' # returns an ADR model function
#' adr_model <- adr_model_full_stop(drug_adr_both = FALSE) 
#' adr_model(adr_history, drug_history)
#'
#' # -> 0
#' 
#' adr_model <- adr_model_full_stop(drug_adr_both = TRUE) 
#' adr_model(adr_history, drug_history)
#' # -> 0 
#' @export 
adr_model_full_stop <- function(drug_adr_both = FALSE) {
  if (drug_adr_both) {
    function(adr_history, drug_history, ...) {
      if (any(as.logical(adr_history) & as.logical(drug_history))) {
        0       # drug will not be prescribed anymore 
      } else {
        1       # drug can still be prescribed
      }
    }
  } else {
    function(adr_history, ...) {
      if (any(as.logical(adr_history))) {
        0       # drug will not be prescribed anymore 
      } else {
        1       # drug can still be prescribed
      }
    }
  }
}