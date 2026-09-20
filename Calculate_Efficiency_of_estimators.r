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