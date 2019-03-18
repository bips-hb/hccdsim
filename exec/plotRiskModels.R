library(hccdsim)

drug_prescriptions <- c(0, 0, rep(1,10), rep(0, 10))

width <- 5
height <- 2

# tibble with various risk models
r1 <- dplyr::as_tibble(expand.grid(risk_function = "risk_withdrawal", rate_withdrawal = c(1,.5,.25)))
r2 <- dplyr::as_tibble(expand.grid(risk_function = "risk_increase_decrease", peak = c(3,6)))
r3 <- dplyr::as_tibble(expand.grid(risk_function = "risk_long_time_after", rate_long_time_after = c(.1), delay = c(100,200)))
r4 <- dplyr::tibble(risk_function = c("risk_no_effect", "risk_immediate"))

risk <- dplyr::full_join(r1,r2)
risk <- dplyr::full_join(risk, r3)
risk <- dplyr::full_join(risk, r4)

drug_prescriptions <- c(0, 0, rep(1,10), rep(0, 20))

plot_risk(drug_prescriptions, risk_function = risk_no_effect())
ggsave("figures/risk_no_effect.pdf", width = width, height = height)

plot_risk(drug_prescriptions, risk_function = risk_immediate())
ggsave("figures/risk_immediate.pdf", width = width, height = height)

rate <- c(1,.5,.25)
sapply(rate, function(rate) { 
  p <- plot_risk(drug_prescriptions, risk_function = risk_withdrawal(rate))
  ggsave(sprintf("figures/risk_withdrawal_%g.pdf", rate), p, width = width, height = height)
  })

peak <- c(3, 6) 
sapply(peak, function(peak) { 
  p <- plot_risk(drug_prescriptions, risk_function = risk_increase_decrease(peak))
  ggsave(sprintf("figures/risk_increase_decrease_%d.pdf", peak), p, width = width, height = height)
})

drug_prescriptions <- c(0, 0, rep(1,10), rep(0, 200))

delay <- c(100, 200) 
sapply(delay, function(delay) { 
  p <- plot_risk(drug_prescriptions, risk_function = risk_long_time_after(0.1, delay))
  ggsave(sprintf("figures/risk_long_time_after_%d.pdf", delay), p, width = width, height = height)
})

