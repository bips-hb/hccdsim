% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/riskFunctions.R
\name{risk_no_effect}
\alias{risk_no_effect}
\title{Risk Model 'No Effect'}
\usage{
risk_no_effect()
}
\value{
A risk function
}
\description{
A risk model reflects how the probability of 
suffering the ADE changes the history of 
drug prescriptions of the patient. 
It returns 0 when the drug prescription history
has no effect, and 1 when the patient is at 
maximal risk.  
In this case, the drug has no effect on the ADE
what so ever.
}
\seealso{
Other Risk models: \code{\link{risk_immediate}},
  \code{\link{risk_increase_decrease}},
  \code{\link{risk_long_time_after}},
  \code{\link{risk_withdrawal}}
}
