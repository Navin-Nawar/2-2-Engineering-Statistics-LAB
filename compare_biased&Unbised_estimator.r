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