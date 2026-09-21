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



# -------------lab report---------------
# Name of the experiment: Distribution of medians and ranges from sample population.\
#  Theory:\
#  In this lab, the goal is to examine the distributions of the medians and ranges computed from multiple samples drawn from a normal distribution. The median is the middle value when the data is sorted, and the range is the difference between the maximum and minimum values in the sample. The median is a robust measure of central tendency, especially in the presence of outliers.\
#  The range provides a measure of the spread or variability in the sample data, but it can be highly influenced by outliers.\
#  By simulating multiple samples from a normal distribution, we can investigate how the distributions of medians and ranges behave across repeated samples. For each sample, we calculate both the median and the range, and then analyze their distribution.\
#  Objective:

#  1. Simulate 1000 random samples from a normal distribution with mean 0 and standard deviation, each containing 20 values.
# 2. Compute the median and range for each sample.
# 3. Visualize the distributions of medians and ranges through histograms, density plots and boxplots.
# 4. Compare the distributions of medians and ranges with the normal distribution and inspect their shapes.\
    
#     Pseudocode:
# 1. set parameters:\
#     ● mu = 0\
#     ● sigma = 1\
#     ● n = 20\
#     ● N-sim = 1000
# 2. Initialize vectors to store the results of the medians and ranges for each simulation.
# 3. Set graphical layout to a 2x2 grid.
# 4. Create histograms for the distributions of medians and ranges.\
#     • Overlay the theoretical normal distribution curve for medians.
# 5. Create a boxplot comparing the distribution of medians and ranges.