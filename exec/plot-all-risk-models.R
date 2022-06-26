#####################################
# plot-all-risk-models.R 
#
# Script for plotting the various 
# risk models implemented in this 
# package. See more detail, 
# R/plot-risks.R and R/risk-models.R
#
# The resulting plots are stored in 
# the figures/ folder
#####################################
library(hccdsim)

# the drug history used for the plots
drug_history <- c(0, 0, rep(1,10), rep(0, 20))

# a longer history used for effect models that take a long time
long_drug_history <- c(0, 0, rep(1,10), rep(0, 100))

# dimensions of the plots 
width  <- 6
height <- 3

# No effect model ---------------------
plot_risk(drug_history, risk_model = risk_model_no_effect())
ggsave("figures/risk_model_no_effect.pdf", width = width, height = height)

# Immediate model ---------------------
plot_risk(drug_history, risk_model = risk_model_immediate())
ggsave("figures/risk_model_immediate.pdf", width = width, height = height)

# Withdrawal model --------------------
rate <- c(1,.5,.25)

sapply(rate, function(rate) { 
  p <- plot_risk(drug_history, risk_model = risk_model_withdrawal(rate))
  ggsave(sprintf("figures/risk_withdrawal_%g.pdf", rate), p, width = width, height = height)
  })

# Increase-decrease model ----------------
peak <- c(3, 6) 
sapply(peak, function(peak) { 
  p <- plot_risk(drug_history, risk_model = risk_model_increase_decrease(peak))
  ggsave(sprintf("figures/risk_model_increase_decrease_%d.pdf", peak), p, width = width, height = height)
})

# Long time after model ---------------
delay <- c(50, 80) 
sapply(delay, function(delay) { 
  p <- plot_risk(long_drug_history, risk_model = risk_model_long_time_after(0.1, delay))
  ggsave(sprintf("figures/risk_long_time_after_0.1_%d.pdf", delay), p, width = width, height = height)
})