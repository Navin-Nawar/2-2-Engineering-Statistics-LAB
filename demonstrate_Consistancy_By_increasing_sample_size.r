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