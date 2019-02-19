#' @export 
ade_model_no_effect <- function() { 
  function(...) { 
    1 
  }
}

#' Full Stop 
#' 
#' Returns a ADE effect model. It stops the prescription of the drug
#' completely (hence, full stop) when the ADE occurs. 
#' 
#' @param drug_ade_both If true, the prescriptions only stop 
#'                      when the ADE takes place when the patient
#'                      is prescribed the drug at the same time step
#'                      
#' @return An ADE model function
#' @export 
ade_model_full_stop <- function(drug_ade_both = FALSE) {
  if (drug_ade_both) {
    function(ade_progression, drug_prescriptions, ...) {
      if (any(as.logical(ade_progression) & as.logical(drug_prescriptions))) {
        0
      } else {
        1
      }
    }
  } else {
    function(ade_progression, ...) {
      if (any(as.logical(ade_progression))) {
        0
      } else {
        1
      }
    }
  }
}