# Parameters
n <- 10                              # Sample size
N_sim <- 1000                        # Number of observations

# Generate data
set.seed(123)                        # Set seed for reproducibility
t_data <- rt(N_sim, df = n - 1)      # Generate t-distributed data
normal_data <- rnorm(N_sim)          # Generate normal-distributed data

# Graphical Output
par(mfrow = c(2, 2))                 # Arrange 4 plots in 2 × 2 layout


# Histogram of t-distribution
hist(t_data,
     breaks = 30,                    # Number of intervals
     col = "lightblue",              # Histogram color
     probability = TRUE,             # Use density scale
     main = "t-Distribution",        # Plot title
     xlab = "Value")                 # X-axis label

curve(dt(x, df = n - 1),
      add = TRUE,                    # Add theoretical t-curve
      col = "red",                   # Curve color
      lwd = 2)                       # Curve thickness


# Histogram of normal distribution
hist(normal_data,
     breaks = 30,                    # Number of intervals
     col = "lightgreen",             # Histogram color
     probability = TRUE,             # Use density scale
     main = "Normal Distribution",   # Plot title
     xlab = "Value")                 # X-axis label

curve(dnorm(x),
      add = TRUE,                    # Add theoretical normal curve
      col = "blue",                  # Curve color
      lwd = 2)                       # Curve thickness


# Density plot comparison
plot(density(t_data),
     col = "red",                    # t-distribution density
     lwd = 2,                        # Line thickness
     main = "Density Comparison",    # Plot title
     xlab = "Value")

lines(density(normal_data),
      col = "blue",                  # Normal distribution density
      lwd = 2)                       # Line thickness

legend("topright",
       legend = c("t-Distribution", "Normal Distribution"),
       col = c("red", "blue"),       # Legend colors
       lwd = 2)                      # Line thickness


# Q-Q plot for t-distribution
qqplot(qt(ppoints(N_sim), df = n - 1),
       t_data,                       # Compare theoretical and sample values
       main = "Q-Q Plot for t-Distribution",
       col = "red",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")

abline(0, 1,                        # Add y = x reference line
       col = "blue",
       lwd = 2)






# ------------Lab Report--------------
# Name of the experiment: Comparison of t-distribution with normal distribution for small sample sizes.\
#  Theory:\
#  The t-distribution, also known as student's t-distribution is used in statistical analysis when the sample size is small and the population standard deviation is unknown. It is similar to the normal distribution but with heavier tails, meaning it has a higher variability in small samples. As the sample size increases, the t-distribution approaches the normal distribution.\
#  For a t-distribution with n-1 degrees of freedom, the probability density function is:\
#  f(x) = (r^(n/2) / √(πny) Γ((n-1)/2)) (1 + x²/(n-1))⁻ⁿ⁄²
#  Objective:\
#  The objective of the experiment is to compare the t-distribution with the normal distribution for small sample sizes by:\
#  • Generating t-distribution and normal data.\
#  • Visualizing their histograms and density functions.\
#  • comparing quantiles using a Q-Q plots.\
#  Pseudocode:

#  1. set parameters : sample size and number of simulations.
# 2. Generate random values from t-distribution with degrees of freedom.
# 3. Generate random values from a standard normal distribution.
# 4. plot:\
#     • Histogram of t-distribution with theoretical density.\
#     • Histogram of normal distribution with theoretical density.\
#     • Density plots of both distributions for comparison.\
#     • Q-Q plot for t-distribution against theoretical quantiles.