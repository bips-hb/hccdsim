library(hccdsim)

x <- c(0, 0, rep(1,10), rep(0, 20))

width <- 6
height <- 3

plot_risk(x, risk_function = risk_no_effect())
ggsave("figures/risk_no_effect.pdf", width = width, height = height)



plot_risk(x, risk_function = risk_immediate())
ggsave("figures/risk_immediate.pdf", width = width, height = height)

rate <- c(1,.5,.25)
sapply(rate, function(rate) { 
  p <- plot_risk(x, risk_function = risk_withdrawal(rate))
  ggsave(sprintf("figures/risk_withdrawal_%g.pdf", rate), p, width = width, height = height)
  })

peak <- c(3, 6) 
sapply(peak, function(peak) { 
  p <- plot_risk(x, risk_function = risk_increase_decrease(peak))
  ggsave(sprintf("figures/risk_increase_decrease_%d.pdf", peak), p, width = width, height = height)
})

x <- c(0, 0, rep(1,10), rep(0, 200))

delay <- c(100, 200) 
sapply(delay, function(delay) { 
  p <- plot_risk(x, risk_function = risk_long_time_after(0.1, delay))
  ggsave(sprintf("figures/risk_long_time_after_0.1_%d.pdf", delay), p, width = width, height = height)
})

