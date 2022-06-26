##############################################
# patient-models.R 
#
# Contains all the patient models. 
# They model how the covariates of the patients,
# e.g., sex and age, are generated. These 
# patient descriptions can then be used 
# in any of the other models: drug 
# models, ADR effect models etc.  
##############################################

#' Patient Model 'Uniformative'
#' 
#' A patient model helps to generate the covariates
#' of the patient, e.g., e.g., sex and age. These 
#' patient descriptions can then be used 
#' in any of the other models: drug prescription
#' models, ADR effect models etc.  
#' In this case, the patient has no covariates. 
#' All patients are the same.
#' 
#' @return A patient model function
#' @family Patient models
#' @examples 
#' patient_model <- patient_model_uninformative() 
#' patient_model()
#' # -> empty list. There are no attributes assigned to the patient
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
#' models, ADR effect models etc.  
#' In this case, the patient has only the covariate
#' `sex'.  
#' 
#' @param prob_male The probability that the 
#'                  patient is 'male' (Default: \code{0.5})
#' 
#' @return A patient model function
#' @family Patient models
#' @examples 
#' patient_model <- patient_model_sex(prob_male = .4)
#' patient_model()
#' # -> list with only item 'sex'
#' @export
patient_model_sex <- function(prob_male = 0.5) {
  function() {
    if (rbinom(1, 1, prob_male) == 1) {
      list(sex = "male")
    } else {
      list(sex = "female")
    }
  }
}
