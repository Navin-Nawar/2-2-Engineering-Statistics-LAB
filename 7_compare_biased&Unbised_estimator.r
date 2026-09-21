# Lab 8: Compare Biased and Unbiased Estimators

# Set the true population parameters
mu <- 5          # True Mean
sigma <- 2       # True Standard Deviation
n <- 30          # Sample size
N_sim <- 1000    # Number of simulations

# Initialize two empty vectors
sample_vars <- numeric(N_sim)  # Stores Unbiased Variances
biased_vars <- numeric(N_sim)  # Stores Biased Variances

# Start simulation
set.seed(123)  # Ensure reproducibility

for (i in 1:N_sim) {
  
  # Generate a random sample of size n
  data <- rnorm(n, mean = mu, sd = sigma)
  
  # Calculate Unbiased Sample Variance using (n - 1)
  sample_vars[i] <- var(data)
  
  # Calculate Biased Sample Variance using n
  biased_vars[i] <- sum((data - mean(data))^2) / n
}

# Create graphical output
par(mfrow = c(1, 2))  # Display two plots side by side

# Histogram of Unbiased Variances
hist(sample_vars,
     breaks = 30,
     col = "lightblue",
     main = "Unbiased Sample Variance",
     xlab = "Variance",
     border = "white")

# Add the true population variance line
abline(v = sigma^2, col = "red", lwd = 2)

# Histogram of Biased Variances
hist(biased_vars,
     breaks = 30,
     col = "lightgreen",
     main = "Biased Sample Variance",
     xlab = "Variance",
     border = "white")

# Add the true population variance line
abline(v = sigma^2, col = "red", lwd = 2)






# ------lab --------------

#  Name of the experiment: Comparison of\
#  Biased and unbiased variance and two-\
#  sample z-test.

#  Theory:

#  1. Biased vs unbiased variance:\
#     • The sample variance formula divides by\
#     n-1 to correct for under estimation of\
#     population variance.\
#     • The biased variance formula divided by\
#     n, leading to systematic underestimation.
# 2. Z-test for two means:\
#     • Used to compare the means of two\
#     independent samples when population variances\
#     are known or large sample sizes\
#     apply.\
#     • The test statistic is given by:

#  z = (x̄₁ - x̄₂) / √(σ₁²/n₁ + σ₂²/n₂)

#  • A high absolute z-score suggests a\
#  significant difference in means.

#  Objective:

#  1. To compare biased vs. unbiased sample\
#     variance using simulation.
# 2. To perform a two-sample z-test to\
#     determine whether there is a significant\
#     difference between two independent groups

#  Pseudocode:

#  1. Variance comparison:\
#     • Set parameters: μ, σ, n, N\_sim, mu,\
#     sigma, N - (5, 10), μ, σ, n, N\_sim\
#     • Initialize vectors for sample variance\
#     and biased variance.\
#     • For each simulation:\
#     • Generate a normal sample\
#     • Unbiased and variance computation.\
#     • Plot histogram of both variances.
# 2. Two - sample z \- test:\
#     • Generate two normal samples with\
#     different means.\
#     • Perform a z-test using z-test().\
#     • Plot histogram of both groups.