
###############
# Example 7.1 #
###############

# Table approach

tab <- expand.grid(1:6, 1:6, 1:6)
table(rowSums(tab)/3)
hist(rowSums(tab)/3, breaks = seq(from = 0, to = 7, by = 1/3), 
     ylim = c(0, .4), freq = FALSE,
     main = "Sampling Distribution of Three Dice (Theoretical)",
     xlab = "Sample Means of 3 Dice Rolls",
     col = "#FE6100AA")
mean(rowSums(tab)/3)
sd(rowSums(tab)/3)

# Simulation approach

set.seed(71)
NUM_DICE <- 3
NUM_SIDES <- 6
NUM_SIM <- 4000

y_bars <- c()

for (i in 1:NUM_SIM){
  rolls <- c()
  for (j in 1:NUM_DICE){
    rolls[j] <- sample(x = 1:NUM_SIDES, size = 1)
  }
  y_bars[i] <- mean(rolls)
}
mean(y_bars)
sd(y_bars)

hist(y_bars, breaks = seq(from = 0, to = 7, by = 1/3), ylim = c(0, .4), 
     main = "Sampling Distribution of Three Dice (Simulated)",
     xlab = "Sample Means of 3 Dice Rolls",
     freq = FALSE, col = "#648FFFAA")
hist(rowSums(tab)/3, breaks = seq(from = 0, to = 7, by = 1/3),      
     main = "Sampling Distribution of Three Dice (Theoretical and Simulated)",
     xlab = "Sample Means of 3 Dice Rolls",
     ylim = c(0, .4), freq = FALSE, col = "#FE6100AA")
hist(y_bars, breaks = seq(from = 0, to = 7, by = 1/3), freq = FALSE, col = "#648FFFAA", add = TRUE)

###############
# Example 7.2 #
###############

# Directly from problem information (find right tail, double, complement)
1-2*pnorm(q = 0.3, mean = 0, sd = 1.0/sqrt(9), lower.tail = FALSE)

# Convert to standard normal
1-2*pnorm(q = -0.9)

###############
# Example 7.3 #
###############

# Approach 1
qnorm(p = 0.025, mean = 0, sd = 1.0, lower.tail = FALSE)
ceiling((qnorm(p=0.025)/0.3)^2)

# Approach 2
achieved_left_tail_prob <- 0.50
n <- 0
mu = 0
sigma = 1.0
while(achieved_left_tail_prob >= 0.025){
  n <- n + 1
  achieved_left_tail_prob <- pnorm(q = -0.3, mean = mu, sd = sigma/sqrt(n), lower.tail = TRUE)
}
print(n)

###############
# Example 7.4 #
###############

qchisq(p = 0.95, df = 6)

###############
# Example 7.5 #
###############

# Approach 1
qchisq(p = 0.05, df = (10-1), lower.tail = TRUE)
qchisq(p = 0.05, df = (10-1), lower.tail = FALSE)

qchisq(p = 0.05, df = (10-1), lower.tail = TRUE)/9
qchisq(p = 0.05, df = (10-1), lower.tail = FALSE)/9

# Approach 2
lefts <- seq(from = 0.01, to = 0.09, by = 0.01)
for (p_left in lefts){
  cat("\n-----")
  cat(paste("\nb1 = ", round(qchisq(p = p_left, df = (10-1), lower.tail = TRUE)/9, 4), sep = ""))
  cat(paste("\nb2 = ", round(qchisq(p = 1-p_left, df = (10-1), lower.tail = TRUE)/9, 4), sep = ""))
}

###############
# Example 7.6 #
###############
pt(q = -2, df = (6-1))
1-2*pt(q = -2, df = (6-1))

###############
# Example 7.7 #
###############
qf(p = 0.95, df1 = (6-1), df2 = (10-1), lower.tail = TRUE)
