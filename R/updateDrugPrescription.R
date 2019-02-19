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