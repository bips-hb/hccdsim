#' Update ADE Progression 
#' 
#' Returns whether or not the ADE occurs at the next
#' time point (\code{1}) or not (\code{0}). The probability of the 
#' ADE occuring depends on the patient drug prescription
#' history (\code{drug_prescriptions}) and the risk 
#' function (\code{risk_function}). 
#' 
#' @param drug_prescriptions Binary vector denoting the 
#'            drug prescriptions history
#' @param risk_function A risk model, e.g., \code{\link{risk_immediate}}
#' @param min_chance A probability model function. The probability of the ADE when 
#'            the drug history has no effect 
#' @param max_chance A probability model function. The probability of the ADE when 
#'            the drug history has the highest possible effect
#' 
#' @return \code{1} or \code{0}
#' @family Update functions
#' @examples 
#' drug_prescriptions <- c(1, 0, 1, 0, 0)
#' 
#' # choose the risk model
#' risk_function <- risk_immediate() 
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
#' update_ade_progression(drug_prescriptions, 
#'                        risk_function, 
#'                        min_chance,
#'                        max_chance,
#'                        patient_profile)
#' @export
update_ade_progression <- function(drug_prescriptions, 
                                   risk_function, 
                                   min_chance,
                                   max_chance, 
                                   patient_profile, ...) { 
  # determine the probability of the ADE occuring at the next time point 
  prob <- min_chance(patient_profile) + 
          (max_chance(patient_profile) - min_chance(patient_profile)) * risk_function(drug_prescriptions, ...)
  rbinom(1,1,prob)
}