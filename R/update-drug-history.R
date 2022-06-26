#' Update Drug Prescription
#' 
#' Returns whether or not the drug is prescribed 
#' at the next time point (\code{1}) or not (\code{0}). 
#' The probability depends on the drug prescriptions, 
#' the ADR history and the \code{drug_model}.
#' 
#' @param drug_history Binary vector denoting the 
#'            drug prescriptions history
#' @param adr_history Binary vector denoting the history
#'            of the ADR
#' @param drug_model A drug model function, 
#'                           e.g., \code{\link{drug_model_markov_chain}}
#' @param adr_model An ADR effect model, e.g., \code{\link{adr_model_full_stop}}
#' @param min_chance The probability of the drug being prescribed
#'            when the drug history and the ADR history have no effect
#' @param max_chance The probability of the ADR when 
#'            the drug history and the ADR history have the highest 
#'            possible effect
#' 
#' @return \code{1} or \code{0}
#' @family Update functions
#' @examples 
#' drug_history <- c(1, 0, 1, 0, 0)
#' adr_history <- c(0, 0, 1, 1, 0) 
#' 
#' # choose the drug prescription model and the ADR effect model
#' drug_model <- drug_model_markov_chain()
#' adr_model <- adr_model_no_effect() 
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
#' # update the adr_history
#' update_drug_prescription(drug_history, 
#'                          adr_history, 
#'                          drug_model, 
#'                          adr_model,
#'                          min_chance,
#'                          max_chance,
#'                          patient_profile)
#' @export
update_drug_prescription <- function(drug_history, 
                                     adr_history, 
                                     drug_model,
                                     adr_model,
                                     min_chance,
                                     max_chance, 
                                     patient_profile, ...) { 
  # determine the probability whether the drug will be prescribed 
  # at the next time point
  prob <- adr_model(drug_history, adr_history, ...) * 
                (min_chance(patient_profile) + (max_chance(patient_profile) - min_chance(patient_profile)) * 
                                                      drug_model(drug_history, ...))
  rbinom(1,1,prob)
}