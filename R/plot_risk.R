#' @export
plot_risk <- function(drug_prescriptions, risk_function = risk_immediate(), ylim = c(0,1)) { 
  risks <- sapply(1:length(drug_prescriptions), function(i)
    risk_function(x[1:i]))
  
  plot(1:length(drug_prescriptions), risks, ylim = ylim)
  # 
  # return(
  #   list(
  #     drug_prescriptions = drug_prescriptions,
  #     risks = risks 
  #   )
  # )
}