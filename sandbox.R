library(hccdsim)

x <- c(0, 0, rep(1,10), rep(0, 20))
drug_prescriptions <- x
x
t <- which(x == 1)
t
plot_risk(x, risk_function = risk_immediate())

plot_risk(x, risk_function = risk_immediate())

library(dplyr)

sim_param <- dplyr::as_tibble(
  expand.grid(
    n_patients = c(1000, 5000, 10000), #c(1000, 10000, 100000),
    simulation_time = c(52 * 4),
    ade_model = c("ade_model_no_effect", "ade_model_full_stop"),
    prescription_model =
      c(
        "prescription_model_markov_chain",
        "prescription_model_duration"
      ),
    min_chance_drug = c(.01),
    max_chance_drug = c(.1, .25, .8),
    min_chance_ade = c(.001),
    max_chance_ade = c(.005, .01, .1),
    repetition = 1:100
  )
)

r1 <- dplyr::as_tibble(expand.grid(risk_function = "risk_withdrawal", rate_withdrawal = c(1,.5,.25)))
r2 <- dplyr::as_tibble(expand.grid(risk_function = "risk_increase_decrease", peak = c(3,6)))
r3 <- dplyr::as_tibble(expand.grid(risk_function = "risk_long_time_after", rate_long_time_after = c(.1), delay = c(100,200)))
r4 <- dplyr::tibble(risk_function = c("risk_no_effect", "risk_immediate"))

risk <- dplyr::full_join(r1,r2)
risk <- dplyr::full_join(risk, r3)
risk <- dplyr::full_join(risk, r4)

sim_param <- merge(risk, sim_param)






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


          