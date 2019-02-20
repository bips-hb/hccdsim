#' Plot Risk
#' 
#' Plots the risk function's behavior for 
#' a given drug prescription history
#' 
#' @param drug_prescriptions Binary vector denoting the 
#'                    drug prescription history
#' @param risk_function One of the risk models 
#' @param title Title of the plot
#' @param ylim The limits of the y-axis: \code{c(ymin, ymax)}
#' 
#' @export
plot_risk <- function(drug_prescriptions, risk_function = risk_immediate(), title = "", ylim = c(0,1)) { 
  risks <- sapply(1:length(drug_prescriptions), function(i)
    risk_function(x[1:i]))
  
  data <- data.frame(
    x = 1:length(drug_prescriptions), 
    y = risks
  )
  
  changes <- data.frame(changes = which(diff(drug_prescriptions) != 0) + .5)
  
  ggplot() + 
    geom_point(data = data, mapping = aes(x = x, y = y)) +
    geom_vline(data = changes, aes(xintercept = changes), linetype='dashed', color = "red") + 
    xlab("time") + 
    ylab("risk") + 
    scale_x_continuous(limits = c(0.5,length(drug_prescriptions)+.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,1), expand = c(.01, .01)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    ggtitle(title) 

}