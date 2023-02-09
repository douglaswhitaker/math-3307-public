

set.seed(839)

NUM_SIM <- 1
N <- 1000
p <- 0.6
epsilon <- 0.05

# Set up empty graph
plot(x = 1:length(flips), 
     y = rep(p, length(flips)), 
     type = "n", 
     ylim = c(0, 1),
     main = paste("Estimate of p based on ", N, " trials\n(based on ", 
                  NUM_SIM, ifelse(NUM_SIM > 1, " runs)", " run)"), sep = ""), 
     xlab = "N",
     ylab = "p estimate")
abline(h = p, col = "darkred", lwd = 3) # add the reference line for the true p
abline(h = c(p + epsilon, p - epsilon), col = "darkblue", lty = 2, lwd = 2)

# perform the simulations
for (i in 1:NUM_SIM){
  flips <- sample(x = 1:0, size = N, replace = TRUE, prob = c(p, 1-p)) 
  points(x = 1:length(flips), 
         y = cumsum(flips)/(1:length(flips)), 
         type = "l", 
         col = "#000000CC", lwd = 2)
}

