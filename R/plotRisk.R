#' Plot Risk
#' 
#' Plots the risk function's behavior for 
#' a given drug prescription history
#' 
#' @param drug_prescriptions Binary vector denoting the 
#'                    drug prescription history
#' @param risk_function One of the risk models 
#' @param ylim The limits of the y-axis: \code{c(ymin, ymax)}
#' 
#' @export
plot_risk <- function(drug_prescriptions, risk_function = risk_immediate(), ylim = c(0,1)) { 
  risks <- sapply(1:length(drug_prescriptions), function(i)
    risk_function(x[1:i]))
  
  plot(1:length(drug_prescriptions), risks, ylim = ylim)
}