library(hccdsim)

x <- c(0, 0, rep(1,10), rep(0, 10))
drug_prescriptions <- x
x
t <- which(x == 1)
t
plot_risk(x, risk_function = risk_increase_decrease(6))

indices <- i 

plot_risk(x, risk_function = risk_withdrawal(.4))
plot_risk(x, risk_function = risk_immediate())
plot_risk(x, risk_function = risk_long_time_after(.4, 30))
plot_risk(x, risk_function = risk_increase_decrease(12))

x
which(diff(x) != 0)

diff(i)
which(diff(i) > 1)

plot_risk(x, risk_function = risk_withdrawal(1))
plot_risk(x, risk_function = risk_withdrawal(.5))
plot_risk(x, risk_function = risk_withdrawal(.25))

x <- c(1,1,1,1)
drug_prescriptions <- x


          