# Lab 9: Calculate Efficiency of Estimators

# Set the true population parameters
mu <- 5          # True Mean
sigma <- 2       # True Standard Deviation
n <- 30          # Sample size
N_sim <- 1000    # Number of simulations

# Initialize two empty vectors
means <- numeric(N_sim)       # Stores Sample Means
medians <- numeric(N_sim)     # Stores Sample Medians

# Start simulation
set.seed(123)  # Ensure reproducibility

for (i in 1:N_sim) {
  
  # Generate a random sample of size n
  data <- rnorm(n, mean = mu, sd = sigma)
  
  # Calculate and store the sample mean
  means[i] <- mean(data)
  
  # Calculate and store the sample median
  medians[i] <- median(data)
}

# Calculate efficiency using the ratio of variances
efficiency <- var(medians) / var(means)

# Display the efficiency value
print(paste("Efficiency (Median/Mean):", efficiency))

# Create graphical output
par(mfrow = c(1, 2))  # Display two plots side by side

# Histogram of Sample Means
hist(means,
     breaks = 30,
     col = "lightblue",
     main = "Distribution of Sample Means",
     xlab = "Value",
     border = "white")

# Histogram of Sample Medians
hist(medians,
     breaks = 30,
     col = "lightgreen",
     main = "Distribution of Sample Medians",
     xlab = "Value",
     border = "white")





------lab----------
Name of the experiment: Efficiency of Mean vs. Median.

 Theory:\
 • The sample mean is the most efficient estimator for normally distributed data,having the smallest variance.\
 • The sample median is more robust to outliers but has higher variance.\
 • Efficiency is measured as the ratio of variances.
 Efficiency = Var (median)/Var (mean)

 A lower value (\<1) indicates that mean\
 and median using simulation.

 Objective:\
 To compare the efficiency of the sample\
 mean and median using simulation.

 Pseudocode:

 1. Set parameters: μ, σ, n, Nsim (mu,\
    sigma, n, N - 1text + 5miny) μ, σ, n, Nsim
2. Initialize vectors for sample means and\
    medians.
3. For each simulation:\
    • Generate normal data\
    • Compute sample mean and median
4. Compute efficiency as the ratio of\
    variances.
5. Plot histogram of means and medians.