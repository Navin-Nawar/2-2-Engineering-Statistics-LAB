# Lab 15: Compare the Power of Different Tests for the Same Hypothesis

# Parameters
mu0 <- 5              # Null hypothesis mean
mu1 <- 6              # Alternative hypothesis mean
sigma <- 2            # Population standard deviation
n <- 30               # Sample size
alpha <- 0.05         # Significance level
N_sim <- 1000         # Number of simulations

# Initialize power counters
power_t_test <- 0     # Counts successful rejections using t-test
power_z_test <- 0     # Counts successful rejections using z-test

# Simulation
set.seed(123)         # Ensure reproducibility

for (i in 1:N_sim) {
  
  # Simulate data under H1
  data <- rnorm(n, mean = mu1, sd = sigma)
  
  # Perform t-test
  t_test <- t.test(data, mu = mu0, alternative = "greater")
  
  if (t_test$p.value < alpha) {
    power_t_test <- power_t_test + 1
  }
  
  # Perform z-test
  z_stat <- (mean(data) - mu0) / (sigma / sqrt(n))
  z_critical <- qnorm(1 - alpha)
  
  if (z_stat > z_critical) {
    power_z_test <- power_z_test + 1
  }
}

# Output
print(paste("Power of t-test:", power_t_test / N_sim))
print(paste("Power of z-test:", power_z_test / N_sim))

# Graphical Output
par(mfrow = c(1, 2))

# Power comparison
barplot(
  c(power_t_test / N_sim, power_z_test / N_sim),
  names.arg = c("t-test", "z-test"),
  col = c("lightblue", "lightgreen"),
  main = "Power Comparison",
  ylab = "Power"
)

# Effect of sample size on power
sample_sizes <- seq(10, 100, by = 10)

power_t <- sapply(sample_sizes, function(n) {
  sum(
    replicate(
      N_sim,
      t.test(
        rnorm(n, mean = mu1, sd = sigma),
        mu = mu0,
        alternative = "greater"
      )$p.value < alpha
    )
  ) / N_sim
})

plot(
  sample_sizes,
  power_t,
  type = "b",
  col = "blue",
  main = "Power vs. Sample Size",
  xlab = "Sample Size",
  ylab = "Power"
)




# ----------lab---------
# Name of the experiment: Compare the power of different tests for the same hypothesis.

# Theory:
# This lab compares the power of two hypothesis tests, the t-test and the z-test for the same hypothesis. The objective is to simulate data under the alternative hypothesis (H1) and calculate the power of each test across multiple simulations. The power of a test is the probability that the test correctly rejects the null hypothesis when H1 is false.

# Objective:
# • To compare the power of the t-test and z-test for testing the same hypothesis.
# • To investigate how the sample size affects the power of the t-test.
# • To visualize the power comparison and the effect of sample size on the power of the t-test.

# Pseudocode:
# 1. Set parameters: μ0, μ1, sigma, n, alpha.
# 2. Initialize power counters:
#    • power_t-test = 0
#    • power_z-test = 0
# 3. Run simulations (N_sim times):
#    • Simulate data under H1.
#    • Perform a t-test and check if p-value < alpha. If true, increment power_t-test.
#    • Perform a z-test and check if test statistic > critical value. If true, increment power_z-test.
# 4. Output: Calculate and print the power of the t-test and z-test.
# 5. Plot:
#    • Barplot comparing the power of both tests.
#    • Plot the effect of sample size on the power of the t-test.
