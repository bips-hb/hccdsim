##############################################
# probabilityModels.R 
#
# Contains all the probability models. 
# They model how probabilities of any kind change
# with the type of patient. E.g., the probability
# of suffering the ADE can differ for males 
# and females. 
##############################################

#' Probability Model 'Constant'
#' 
#' Probability models define how probabilities 
#' change with the type of patient. In this case,
#' the type of patient has no effect on the 
#' probability. 
#' 
#' @param prob The probability
#' 
#' @return A probability model function
#' @family Probability models
#' @export
probability_constant <- function(prob, ...) { 
  function(...) { 
    prob
  }
}

#' Probability Model 'Sex'
#' 
#' Probability models define how probabilities 
#' change with the type of patient. In this case,
#' the sex of the patient changes the probability.
#' 
#' @param prob_male,prob_female The probabilities
#'         when the patient is either male or female
#' 
#' @return A probability model function
#' @family Probability models
#' @export
#' @export
probability_sex <- function(prob_male, prob_female, ...) { 
  function(patient, ...) { 
    
    # check whether the sex of the patient is specified
    if (!("sex" %in% names(patient))) { 
       stop("the sex of the patient should be specified")
    }
    
    if (patient$sex == "male") { 
      prob_male
    } else { 
      prob_female 
    }
  }
}

