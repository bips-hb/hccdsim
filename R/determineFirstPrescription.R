#' First Prescription
#' 
#' This function is used for modelling patients 
#' that are exposed to the drug at least once 
#' during simulation time. 
#' 
#' @param simulation_time The total number of time points for this patient
#' @param min_chance_drug The probability of the drug being prescribed 
#'                        when previous prescriptions have no influence 
#'                        on the current time point
#'                        
#' @return A value between 1 and \code{simulation_time}
#' @export
determine_first_prescription <- function(simulation_time, min_chance_drug) { 
  probs <- sapply(1:simulation_time, function(t) (1 - min_chance_drug)^(t - 1))
  probs <- probs * min_chance_drug / sum(probs)
  sample(x = 1:simulation_time, 1, replace = T, prob = probs)
}