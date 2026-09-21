# Parameters
mu <- 5                              # Population mean
sigma <- 2                           # Population standard deviation
sample_sizes <- c(10, 30, 100, 500, 1000)  # Different sample sizes
N_sim <- 1000                        # Number of simulations

# Initialize vectors
means <- numeric(length(sample_sizes))  # Store mean for each sample size
vars <- numeric(length(sample_sizes))   # Store variance for each sample size

# Simulation
set.seed(123)                        # Set seed for reproducibility

for (i in 1:length(sample_sizes)) {

  n <- sample_sizes[i]               # Select current sample size

  # Generate 1000 sample means
  sample_means <- replicate(
    N_sim,
    mean(rnorm(n, mean = mu, sd = sigma))
  )

  means[i] <- mean(sample_means)     # Average of 1000 sample means
  vars[i] <- var(sample_means)       # Variance of 1000 sample means
}

# Graphical Output
par(mfrow = c(1, 2))                 # Arrange 2 plots in one row

# Plot sample means
plot(sample_sizes, means,
     type = "b",                     # Points connected by lines
     col = "blue",
     main = "Convergence of Sample Mean",
     xlab = "Sample Size",
     ylab = "Mean")

abline(h = mu,
       col = "red",                  # True population mean
       lwd = 2)

# Plot variance of sample means
plot(sample_sizes, vars,
     type = "b",
     col = "green",
     main = "Convergence of Variance of Sample Mean",
     xlab = "Sample Size",
     ylab = "Variance")

abline(h = sigma^2,
       col = "red",
       lwd = 2)



# ----------lab report---------
# Name of the experiment: Demonstrate consistency by increasing sample size.

#  Theory:

#  Consistency of an estimator means that as the sample size increases, the estimator converges to the true parameter value.\
#  The sample mean x̄ is a consistent estimator for the population mean μ, and its variance decreases as the sample size increases, following:

#  var (x̄) = σ²/n

#  Objective:

#  To demonstrate consistency by showing that:\
#  • The sample mean converges to the population mean (μ)\
#  • The variance of the sample mean decreases with increasing sample size

#  Pseudocode:

#  1. Define parameters: population mean (μ), standard deviation, sample size and number of simulations (N\_sim).
# 2. Initialize reserve for sample means and variances.
# 3. For each sample size:\
#     • Generate N\_sim sample means.\
#     • Compute and store the average sample mean and variance.
# 4. Plot the sample means and variances against sample size with theoretical reference lines