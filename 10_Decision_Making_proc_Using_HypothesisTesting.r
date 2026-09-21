# Lab 11: Simulate Decision-Making Processes Using Hypothesis Testing

# Parameters
mu0 <- 5              # Null hypothesis mean
mu1 <- 6              # True population mean
sigma <- 2            # Population standard deviation
n <- 30                # Sample size
alpha <- 0.05          # Significance level

# Generate sample data
set.seed(123)          # Ensure reproducibility
sample_data <- rnorm(n, mean = mu1, sd = sigma)  # Generate random sample

# Perform one-sample t-test
t_test_result <- t.test(sample_data, mu = mu0, alternative = "greater")

# Decision
if (t_test_result$p.value < alpha) {
  decision <- "Reject H0"              # Significant evidence against H0
} else {
  decision <- "Fail to reject H0"      # Not enough evidence against H0
}

# Output
print(paste("Test Statistic:", t_test_result$statistic))  # Display test statistic
print(paste("P-value:", t_test_result$p.value))            # Display p-value
print(paste("Decision:", decision))                        # Display final decision

# Graphical Output
par(mfrow = c(1, 2))  # Display two plots side by side

# Histogram of Sample Data
hist(sample_data,
     breaks = 30,
     col = "lightblue",
     main = "Sample Data",
     xlab = "Value",
     border = "white")

abline(v = mu0, col = "red", lwd = 2)              # Null hypothesis mean
abline(v = mean(sample_data), col = "blue", lwd = 2)  # Sample mean

# Density Plot
plot(density(sample_data),
     col = "blue",
     lwd = 2,
     main = "Density Plot",
     xlab = "Value")

abline(v = mu0, col = "red", lwd = 2)  # Null hypothesis mean
abline(v = mean(sample_data), col = "blue", lwd = 2)  # Sample mean









# ----------lab---------
# Name of the experiment: Simulate decision-making processes using Hypothesis testing.

#  Theory:\
#  The lab demonstrates the use of hypothesis testing to simulate decision-making processes. It tests the hypothesis about the population mean by comparing the sample mean against hypothesized value.

#  Objective:\
#  • To simulate a decision-making process using hypothesis testing.\
#  • To perform a one-sample t-test and interpret the result.\
#  • To visualize the data using a histogram and density plot.

#  Pseudocode:

#  1. Define parameters:\
#     • Null hypothesis mean (mu0), true population mean (mu1), standard deviation (sigma), sample size (n), significance level (alpha)
# 2. Generate sample data based on the true population mean.
# 3. Perform a t-test to compare the sample mean with the null hypothesis mean (mu0)
# 4. Make a decision based on the p-value:\
#     • If the p-value \< alpha, reject H0 (null hypothesis)\
#     • Otherwise, fail to reject H0
# 5. Output the test statistic, p value and decision.
# 6. Plot the histogram and density plot to visualize the critical region.
