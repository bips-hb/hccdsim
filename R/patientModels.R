
#' @export
patient_model_uninformative <- function(...) { 
  function() { 
    list()  
  }
}

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
