#' Update Drug Prescription
#' 
#' Returns whether or not the drug is prescribed 
#' at the next time point (1) or not (0). 
#' 
#' @param drug_prescriptions Binary vector denoting the 
#'            drug prescriptions history
#' @param ade_progression Binary vector denoting the history
#'            of the ADE
#' @param prescription_model One of the prescription models
#' @param ade_model One of the ADE effect models
#' @param min_chance The probability of the drug being prescribed
#'            when the drug history and the ADE history have no effect
#' @param max_chance The probability of the ADE when 
#'            the drug history and the ADE history have the highest 
#'            possible effect
#' 
#' @return 1 or 0
#' @family Update functions
#' @export
update_drug_prescription <- function(drug_prescriptions, 
                                     ade_progression, 
                                     prescription_model,
                                     ade_model,
                                     min_chance,
                                     max_chance, ...) { 
  prob <- ade_model(drug_prescriptions, ade_progression, ...) * (min_chance + (max_chance - min_chance) * 
                                                      prescription_model(drug_prescriptions, ...))
  rbinom(1,1,prob)
}