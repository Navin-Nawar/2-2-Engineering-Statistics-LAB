# Lab 12: Derive the Best Critical Region for Simple vs. Composite Hypotheses

# Parameters
mu0 <- 5              # Null hypothesis mean
mu1 <- 6              # Alternative hypothesis mean
sigma <- 2            # Population standard deviation
n <- 30               # Sample size
alpha <- 0.05         # Significance level

# Generate sample data under H0
set.seed(123)         # Ensure reproducibility
sample_data_H0 <- rnorm(n, mean = mu0, sd = sigma)  # Generate sample under H0

# Generate sample data under H1
sample_data_H1 <- rnorm(n, mean = mu1, sd = sigma)  # Generate sample under H1

# Likelihood ratio test
likelihood_ratio <- function(data, mu0, mu1, sigma) {
  
  # Calculate log-likelihood under H1
  likelihood_H1 <- sum(dnorm(data, mean = mu1, sd = sigma, log = TRUE))
  
  # Calculate log-likelihood under H0
  likelihood_H0 <- sum(dnorm(data, mean = mu0, sd = sigma, log = TRUE))
  
  # Calculate likelihood ratio
  exp(likelihood_H1 - likelihood_H0)
}

# Critical region
critical_value <- qnorm(1 - alpha,
                         mean = mu0,
                         sd = sigma / sqrt(n))  # Critical value for sample mean

# Decision
decision_H0 <- mean(sample_data_H0) > critical_value  # Reject H0 if sample mean is large
decision_H1 <- mean(sample_data_H1) > critical_value  # Reject H0 if sample mean is large

# Output
print(paste("Critical Value:", critical_value))
print(paste("Decision under H0:", decision_H0))
print(paste("Decision under H1:", decision_H1))

# Graphical Output
par(mfrow = c(1, 2))  # Display two plots side by side

# Density plot under H0
plot(density(sample_data_H0),
     col = "blue",
     lwd = 2,
     main = "Density under H0",
     xlab = "Value")

abline(v = critical_value,
       col = "red",
       lty = 2)  # Critical value line

# Density plot under H1
plot(density(sample_data_H1),
     col = "green",
     lwd = 2,
     main = "Density under H1",
     xlab = "Value")

abline(v = critical_value,
       col = "red",
       lty = 2)  # Critical value line



# ----------lab---------
# Name of the experiment: Derive the best critical region for Simple vs. composite hypotheses.

# Theory:
# This lab demonstrates how to derive the best critical region for a hypothesis test, specifically for testing simple versus composite hypotheses using the likelihood ratio test. It explores the decision-making process using sample data.

# Objective:
# • To derive the best critical region using the likelihood ratio test for simple vs. composite hypotheses.
# • To perform hypothesis testing using the critical region.
# • To visualize the critical region in density plots for both H₀ and H₁.

# Pseudo code:
# (1) Set parameters: mu0, mu1, sigma, n, alpha

# (2) Generate Data:
#    • Sample_data H0 from mu0
#    • Sample_data H1 from mu1

# (3) Likelihood Ratio test:
#    • Calculate likelihood ratio for data

# (4) Critical value: critical_value = qnorm(1-alpha, mean=sigma / sqrt(n))

# (5) Decision:
#    • decision_H0 = mean(sample_data) > critical_value
#    • decision_H1 = mean(sample_data) > critical_value

# (6) Output: Print critical value, decisions for H0 and H1

# (7) Plot: Plot density for sample_data H0 and sample_data H1 with critical value.
