##############################################
# patientModels.R 
#
# Contains all the patient models. 
# They model how the covariates of the patients,
# e.g., sex and age, are generated. These 
# patient descriptions can then be used 
# in any of the other models: drug prescription
# models, ADE effect models etc.  
##############################################

#' Patient Model 'Uniformative'
#' 
#' A patient model helps to generate the covariates
#' of the patient, e.g., e.g., sex and age. These 
#' patient descriptions can then be used 
#' in any of the other models: drug prescription
#' models, ADE effect models etc.  
#' In this case, the patient has no covariates. 
#' All patients are the same.
#' 
#' @return A patient model function
#' @family Patient models
#' @export
patient_model_uninformative <- function(...) { 
  function() { 
    list()  
  }
}

#' Patient Model 'Sex'
#' 
#' A patient model helps to generate the covariates
#' of the patient, e.g., e.g., sex and age. These 
#' patient descriptions can then be used 
#' in any of the other models: drug prescription
#' models, ADE effect models etc.  
#' In this case, the patient has only the covariate
#' `sex'.  
#' 
#' @param prob_male The probability that the patient is 'male'
#' 
#' @return A patient model function
#' @family Patient models
#' @export
patient_model_sex <- function(prob_male) {
  function() {
    if (rbinom(1, 1, prob_male) == 1) {
      list(sex = "male")
    } else {
      list(sex = "female")
    }
  }
}
