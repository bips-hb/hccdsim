% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probabilityModels.R
\name{probability_sex}
\alias{probability_sex}
\title{Probability Model 'Sex'}
\usage{
probability_sex(prob_male, prob_female, ...)
}
\arguments{
\item{prob_male, prob_female}{The probabilities
when the patient is either male or female}
}
\value{
A probability model function
}
\description{
Probability models define how probabilities 
change with the type of patient. In this case,
the sex of the patient changes the probability.
}
\seealso{
Other Probability models: \code{\link{probability_constant}}
}
