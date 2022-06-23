#' Update Drug Prescription
#' 
#' Returns whether or not the drug is prescribed 
#' at the next time point (\code{1}) or not (\code{0}). 
#' The probability depends on the drug prescriptions, 
#' the ADE history and the prescription_model.
#' 
#' @param drug_prescriptions Binary vector denoting the 
#'            drug prescriptions history
#' @param ade_progression Binary vector denoting the history
#'            of the ADE
#' @param prescription_model A prescription model function, 
#'                           e.g., \code{\link{prescription_model_markov_chain}}
#' @param ade_model An ADE effect model, e.g., \code{\link{ade_model_full_stop}}
#' @param min_chance The probability of the drug being prescribed
#'            when the drug history and the ADE history have no effect
#' @param max_chance The probability of the ADE when 
#'            the drug history and the ADE history have the highest 
#'            possible effect
#' 
#' @return \code{1} or \code{0}
#' @family Update functions
#' @examples 
#' drug_prescriptions <- c(1, 0, 1, 0, 0)
#' ade_progression <- c(0, 0, 1, 1, 0) 
#' 
#' # choose the drug prescription model and the ADE effect model
#' prescription_model <- prescription_model_markov_chain()
#' ade_model <- ade_model_no_effect() 
#' 
#' # choose probability models. Note that the probabilities change
#' # with sex
#' min_chance <- probability_sex(prob_male = .1, prob_female = .05)
#' max_chance <- probability_sex(prob_male = .7, prob_female = .8)
#' 
#' # create a patient profile: 
#' patient_model <- patient_model_sex(prob_male = 0.5) 
#' (patient_profile <- patient_model()) 
#' 
#' # update the ade_progression
#' update_drug_prescription(drug_prescriptions, 
#'                          ade_progression, 
#'                          prescription_model, 
#'                          ade_model,
#'                          min_chance,
#'                          max_chance,
#'                          patient_profile)
#' @export
update_drug_prescription <- function(drug_prescriptions, 
                                     ade_progression, 
                                     prescription_model,
                                     ade_model,
                                     min_chance,
                                     max_chance, 
                                     patient_profile, ...) { 
  # determine the probability whether the drug will be prescribed 
  # at the next time point
  prob <- ade_model(drug_prescriptions, ade_progression, ...) * 
                (min_chance(patient_profile) + (max_chance(patient_profile) - min_chance(patient_profile)) * 
                                                      prescription_model(drug_prescriptions, ...))
  rbinom(1,1,prob)
}