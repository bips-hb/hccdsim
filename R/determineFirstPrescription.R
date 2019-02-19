#' @export
determine_first_prescription <- function(simulation_time, min_chance_drug) { 
  probs <- sapply(1:simulation_time, function(t) (1 - min_chance_drug)^(t - 1))
  probs <- probs * min_chance_drug / sum(probs)
  sample(x = 1:simulation_time, 1, replace = T, prob = probs)
}