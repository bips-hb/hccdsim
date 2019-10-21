#' Update ADE Progression 
#' 
#' Returns whether or not the ADE occurs at the next
#' time point (1) or not (0). 
#' 
#' @param drug_prescriptions Binary vector denoting the 
#'            drug prescriptions history
#' @param risk_function One of the risk models 
#' @param min_chance The probability of the ADE when 
#'            the drug history has no effect 
#' @param max_chance The probability of the ADE when 
#'            the drug history has the highest possible effect
#' 
#' @return 1 or 0
#' @family Update functions
#' @export
update_ade_progression <- function(drug_prescriptions, 
                                   risk_function, 
                                   min_chance,
                                   max_chance, 
                                   patient_profile, ...) { 
  prob <- min_chance(patient_profile) + 
          (max_chance(patient_profile) - min_chance(patient_profile)) * risk_function(drug_prescriptions, ...)
  rbinom(1,1,prob)
}