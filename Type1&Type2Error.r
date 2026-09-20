# Lab 13: Simulate Type I and Type II Errors in Hypothesis Testing

# Parameters
mu0 <- 5              # Null hypothesis mean
mu1 <- 6              # Alternative hypothesis mean
sigma <- 2            # Population standard deviation
n <- 30               # Sample size
alpha <- 0.05         # Significance level
N_sim <- 1000         # Number of simulations

# Initialize counters
type_I_errors <- 0    # Counter for Type I errors
type_II_errors <- 0   # Counter for Type II errors

# Simulation
set.seed(123)         # Ensure reproducibility

for (i in 1:N_sim) {
  
  # Simulate data under H0
  data_H0 <- rnorm(n, mean = mu0, sd = sigma)
  
  # Perform one-sample t-test under H0
  t_test_H0 <- t.test(data_H0,
                      mu = mu0,
                      alternative = "greater")
  
  # Count Type I error
  if (t_test_H0$p.value < alpha) {
    type_I_errors <- type_I_errors + 1
  }
  
  # Simulate data under H1
  data_H1 <- rnorm(n, mean = mu1, sd = sigma)
  
  # Perform one-sample t-test under H1
  t_test_H1 <- t.test(data_H1,
                      mu = mu0,
                      alternative = "greater")
  
  # Count Type II error
  if (t_test_H1$p.value >= alpha) {
    type_II_errors <- type_II_errors + 1
  }
}

# Output
print(paste("Type I Error Rate:", type_I_errors / N_sim))
print(paste("Type II Error Rate:", type_II_errors / N_sim))
print(paste("Power:", 1 - (type_II_errors / N_sim)))

# Graphical Output
par(mfrow = c(1, 2))  # Display two plots side by side

# Type I Error Distribution
hist(replicate(
  N_sim,
  t.test(rnorm(n, mean = mu0, sd = sigma),
         mu = mu0,
         alternative = "greater")$p.value),
  breaks = 30,
  col = "lightblue",
  main = "P-values under H0",
  xlab = "P-value",
  border = "white"
)

abline(v = alpha, col = "red", lwd = 2)  # Significance level line

# Type II Error Distribution
hist(replicate(
  N_sim,
  t.test(rnorm(n, mean = mu1, sd = sigma),
         mu = mu0,
         alternative = "greater")$p.value),
  breaks = 30,
  col = "lightgreen",
  main = "P-values under H1",
  xlab = "P-value",
  border = "white"
)

abline(v = alpha, col = "red", lwd = 2)  # Significance level line