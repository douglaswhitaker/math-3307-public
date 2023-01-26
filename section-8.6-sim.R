set.seed(905)
library(ggplot2)

n <- 35
p <- .5
NUM_SIM <- 50
CONF_LEVEL <- 0.95

lower_bound <- function(x, n, alpha_level = 0.05){
  p_hat <- x/n
  return(p_hat - qnorm(p = 1 - alpha_level/2) * sqrt((p_hat*(1-p_hat))/n))
}
upper_bound <- function(x, n, alpha_level = 0.05){
  p_hat <- x/n
  return(p_hat + qnorm(p = 1 - alpha_level/2) * sqrt((p_hat*(1-p_hat))/n))
}

do_sim <- function(n, p, NUM_SIM, CONF_LEVEL){
  alpha_level <- 1 - CONF_LEVEL
  is <- phats <- lowers <- uppers <- c()
  for (i in 1:NUM_SIM){
    temp_samp <- rbinom(n = 1, size = n, prob = p) # random number of successes
    is[i] <- i
    phats[i] <- temp_samp/n 
    lowers[i] <- lower_bound(x = temp_samp, n = n, alpha_level = alpha_level)
    uppers[i] <- upper_bound(x = temp_samp, n = n, alpha_level = alpha_level)
  }
  captured <- (p > lowers) & (p < uppers)
  print(paste("Proportion of simulated CIs capturing the true p: ", sum(captured)/NUM_SIM), sep ="")
  sim_res <- data.frame(is, phats, lowers, uppers, captured)
  g <- ggplot(sim_res, aes(is, phats)) + 
        geom_point() +
        geom_errorbar(aes(ymin = lowers, ymax = uppers, color = captured)) +
        geom_hline(yintercept = p, color = "red")
  print(g)
}

do_sim(n = n, p = p, NUM_SIM = NUM_SIM, CONF_LEVEL)
