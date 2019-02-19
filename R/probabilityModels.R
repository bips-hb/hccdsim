
#' @export
probability_constant <- function(prob, ...) { 
  prob
}

#' @export
probability_sex <- function(prob_male, prob_female, patient, ...) { 
   if (patient$sex == "male") { 
      prob_male
   } else { 
      prob_female 
   }
}

