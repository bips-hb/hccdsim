#' @export
update_ade_progression <- function(drug_prescriptions, 
                                   risk_function, 
                                   min_chance,
                                   max_chance, ...) { 
  prob <- min_chance + (max_chance - min_chance) * risk_function(drug_prescriptions, ...)
  rbinom(1,1,prob)
}