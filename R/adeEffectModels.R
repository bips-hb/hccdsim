##############################################
# adeEffectModels.R 
#
# Contains all the possible ADE effect models. 
# They model which effect the appearance of the 
# ADE has on the prescription of the drug. 
##############################################

#' The ADE model 'no effect'
#' 
#' \code{ade_model_no_effect} models the effect of 
#' the appearance of an ADE on the drug prescriptions 
#' of patient. In this case, the appearance of an ADE 
#' has no effect on the prescription of a drug. 
#' 
#' @return An ADE model function
#' @family ADE effect models
#' @export 
ade_model_no_effect <- function() { 
  function(...) { 
    1 
  }
}

#' The ADE model 'full stop'
#' 
#' \code{ade_model_full_stop} models the effect of 
#' the appearanceof an ADE on the drug prescriptions 
#' of patient. In this case, the appearance of an ADE 
#' stops any future drug prescription. This can, for example, 
#' reflect that the ADE is so severe, that the drug 
#' is never prescribed again.
#' 
#' @param drug_ade_both If \code{TRUE}, the prescriptions only stop 
#'                      when the ADE takes place when the patient
#'                      is prescribed the drug at the same time step (Default: \code{FALSE})
#' 
#' @return An ADE model function
#' @family ADE effect models
#' @export 
ade_model_full_stop <- function(drug_ade_both = FALSE) {
  if (drug_ade_both) {
    function(ade_progression, drug_prescriptions, ...) {
      if (any(as.logical(ade_progression) & as.logical(drug_prescriptions))) {
        0       # drug will not be prescribed anymore 
      } else {
        1       # drug can still be prescribed
      }
    }
  } else {
    function(ade_progression, ...) {
      if (any(as.logical(ade_progression))) {
        0       # drug will not be prescribed anymore 
      } else {
        1       # drug can still be prescribed
      }
    }
  }
}