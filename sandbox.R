library(hccdsim)

x <- c(rep(1,4),0, 0, rep(1, 4), 0, rep(1,4), rep(0,20), rep(1,3))
plot_risk(x, risk_function = risk_increase_decrease(6))

plot_risk(x, risk_function = risk_withdrawal(1))
plot_risk(x, risk_function = risk_withdrawal(.5))
plot_risk(x, risk_function = risk_withdrawal(.25))

x <- c(1,1,1,1)
drug_prescriptions <- x


          