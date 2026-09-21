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




# -----------Lab report---------------

#  Name of the Experiment: Estimate population parameters (mean, variance) from sample data.\
#  Theory:\
#  When we draw a sample from a population we use the sample data to estimate population parameters, such as the mean and variance. The sample mean and sample variance are often used as estimates, but they come with some uncertainty. Therefore, we can calculate confidence intervals to estimate the range in which the true population parameters likely fall.\
#  • The sample mean is an estimate of the population mean.\
#  • The sample variance is an estimate of the population variance.\
#  • A confidence interval provides a range of values within the population parameters is likely to lie, given the sample data.\
#  Objective:

#  1. Generate sample data from a normal distribution using the known population parameters.
# 2. Estimate the population parameters from sample data.
# 3. Calculate the 95% confidence interval for both the sample mean and the sample variance.
# 4. Visualize the sample data.\
#     Pseudocode:
# 5. set parameters:\
#     ● μ₀ = 5\
#     ● sigma₀ = 2\
#     ● N = 50\
#     ● N-sim = 1000
# 6. Generate one sample data:\
#     • Draw n = 50 random samples from a normal distribution with mean mu₀ and standard deviation sigma₀.
# 7. Point estimates:\
#     • calculate the sample mean; sample mean = mean (sample\_data)\
#     • calculate the sample variance; sample\_var = variance (sample\_data)
# 8. Confidence interval:\
#     • calculate the 95% confidence interval for the mean using the t-distribution\
#     • compute the 95% interval for the variance using chi-squared distribution formula.
# 9. Plot results:\
#     • plot the sample mean, sample variance, confidence intervals for the mean and confidence interval for the variance.
# 10. Repeat the experiment:\
#      • Repeat the above steps for N-sim = 1000 samples.