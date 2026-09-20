# Parameters
mu <- 0                              # Population mean
sigma <- 1                           # Population standard deviation
n <- 20                              # Sample size
N_sim <- 1000                        # Number of simulations

# Initialize vectors
medians <- numeric(N_sim)            # Store sample medians
ranges <- numeric(N_sim)             # Store sample ranges

# Simulation
set.seed(123)                        # Set seed for reproducibility

for (i in 1:N_sim) {                 # Repeat simulation 1000 times

  data <- rnorm(n, mean = mu, sd = sigma)  # Generate one sample

  medians[i] <- median(data)         # Calculate sample median
  ranges[i] <- max(data) - min(data) # Calculate sample range
}

# Graphical Output
par(mfrow = c(2, 2))                 # Arrange 4 plots in 2 × 2 layout


# Histogram of medians
hist(medians,
     breaks = 30,                    # Number of intervals
     col = "lightblue",              # Histogram color
     probability = TRUE,             # Use density scale
     main = "Distribution of Medians",
     xlab = "Median")                # X-axis label

curve(dnorm(x, mean = mu, sd = sigma / sqrt(n)),
      add = TRUE,                    # Add theoretical normal curve
      col = "red",
      lwd = 2)


# Histogram of ranges
hist(ranges,
     breaks = 30,                    # Number of intervals
     col = "lightgreen",             # Histogram color
     probability = TRUE,             # Use density scale
     main = "Distribution of Ranges",
     xlab = "Range")                 # X-axis label


# Density plot of medians
plot(density(medians),
     col = "blue",                   # Density curve color
     lwd = 2,
     main = "Density Plot of Medians",
     xlab = "Median")

curve(dnorm(x, mean = mu, sd = sigma / sqrt(n)),
      add = TRUE,                    # Add theoretical normal curve
      col = "red",
      lwd = 2)


# Boxplot of medians and ranges
boxplot(list(Medians = medians, Ranges = ranges),
        col = c("lightblue", "lightgreen"),
        main = "Boxplot of Medians and Ranges")