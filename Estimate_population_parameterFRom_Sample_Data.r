# Parameters
mu <- 5                              # Population mean
sigma <- 2                           # Population standard deviation
n <- 30                              # Sample size
N_sim <- 1000                        # Number of simulations

# Generate sample data
set.seed(123)                        # Set seed for reproducibility
sample_data <- rnorm(n, mean = mu, sd = sigma)  # Generate sample

# Point estimates
sample_mean <- mean(sample_data)     # Calculate sample mean
sample_var <- var(sample_data)       # Calculate sample variance

# Confidence intervals
conf_int_mean <- t.test(sample_data)$conf.int   # 95% CI for mean

conf_int_var <- c(
  (n - 1) * sample_var / qchisq(0.975, df = n - 1),  # Lower limit
  (n - 1) * sample_var / qchisq(0.025, df = n - 1)   # Upper limit
)

# Output
print(paste("Sample Mean:", sample_mean))
print(paste("Sample Variance:", sample_var))
print(paste("95% CI for Mean:", conf_int_mean))
print(paste("95% CI for Variance:", conf_int_var))

# Graphical Output
par(mfrow = c(1, 2))                 # Arrange 2 plots in one row

# Histogram with mean and CI
hist(sample_data,
     breaks = 30,                    # Number of intervals
     col = "lightblue",              # Histogram color
     main = "Sample Data with Mean and Variance",
     xlab = "Value")

abline(v = sample_mean,
       col = "red",                  # Mean line color
       lwd = 2)                      # Line thickness

abline(v = conf_int_mean,
       col = "blue",                 # CI line color
       lty = 2)                      # Dashed line

# Boxplot
boxplot(sample_data,
        col = "lightgreen",
        main = "Boxplot of Sample Data",
        ylab = "Value")