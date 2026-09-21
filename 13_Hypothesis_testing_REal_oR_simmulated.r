# Lab 14: Perform Hypothesis Testing Step-by-Step Using Real or Simulated Data

# Parameters
mu0 <- 5              # Null hypothesis mean
mu1 <- 6              # True population mean
sigma <- 2            # Population standard deviation
n <- 30               # Sample size
alpha <- 0.05         # Significance level

# Generate sample data
set.seed(123)         # Ensure reproducibility
sample_data <- rnorm(n, mean = mu1, sd = sigma)  # Generate sample from H1

# Step 1: State hypotheses
print("H0: mu = 5")
print("H1: mu > 5")

# Step 2: Choose significance level
print(paste("Significance level (alpha):", alpha))

# Step 3: Calculate test statistic
t_stat <- (mean(sample_data) - mu0) / (sd(sample_data) / sqrt(n))
print(paste("Test Statistic (t):", t_stat))

# Step 4: Determine critical value or p-value
critical_value <- qt(1 - alpha, df = n - 1)
p_value <- pt(t_stat, df = n - 1, lower.tail = FALSE)

print(paste("Critical Value:", critical_value))
print(paste("P-value:", p_value))

# Step 5: Make a decision
if (t_stat > critical_value) {
  decision <- "Reject H0"              # Test statistic is in critical region
} else {
  decision <- "Fail to reject H0"      # Not enough evidence against H0
}

print(paste("Decision:", decision))

# Graphical Output
par(mfrow = c(1, 2))  # Display two plots side by side

# Histogram with sample mean and null mean
hist(sample_data,
     breaks = 30,
     col = "lightblue",
     main = "Sample Data",
     xlab = "Value",
     border = "white")

abline(v = mu0, col = "red", lwd = 2)             # Null hypothesis mean
abline(v = mean(sample_data), col = "blue", lwd = 2)  # Sample mean

# Density plot with critical value
plot(density(sample_data),
     col = "blue",
     lwd = 2,
     main = "Density Plot",
     xlab = "Value")

abline(v = critical_value,
       col = "red",
       lty = 2)  # Critical value line






# ----------lab---------
# Name of the experiment: Perform hypothesis testing step-by-step using real or simulated data.

# Theory:
# In this lab, walks through the process of hypothesis testing using real or simulated data. I am testing a one-sample t-test, where the null hypothesis is that the population mean is equal to a specific value, and the alternative hypothesis is that the population mean is greater than that value.

# Objective:
# • To perform hypothesis testing step-by-step.
# • To calculate the test statistic and p-value.
# • To make a decision on whether to reject or fail to reject the null hypothesis.
# • To visualize the sample data with critical regions using a histogram and density plot.

# Procedure:
# 1. Set parameters: μ0, μ1, sigma, n.
# 2. Generate sample data: Simulate data based on μ1.
# 3. State Hypothesis:
#    • H0: μ = μ0
#    • H1: μ > μ0
# 4. Choose significant level: set alpha = 0.05
# 5. Calculate test statistic:
   
#    t = (X̄ - μ0) / (S/√n)

# 6. Determine critical value and p-value:
#    • critical value = tα, df
#    • p-value: calculate using the p() function.
# 7. Make Decision:
#    • If t-stat > critical value, reject H0.
#    • Otherwise, fail to reject H0.
# 8. Output: Display test statistic, p-value, critical value and decision.
# 9. Plot:
#    • Histogram and density plot with critical region.
