# Parameters
k <- 5                              # Degrees of freedom
N_sim <- 1000                      # Number of observations

# Generate chi-squared data
set.seed(123)                       # Set seed for reproducibility
chi2_data <- rchisq(N_sim, df = k) # Generate 1000 Chi-squared values

# Compute mean and variance
mean_chi2 <- mean(chi2_data)       # Calculate sample mean
var_chi2 <- var(chi2_data)         # Calculate sample variance

print(paste("Mean:", mean_chi2))   # Display mean
print(paste("Variance:", var_chi2)) # Display variance

# Graphical Output
par(mfrow = c(1, 3))               # Arrange 3 plots in one row

# Histogram
hist(chi2_data,
     breaks = 30,                  # Number of intervals
     col = "lightblue",            # Histogram color
     probability = TRUE,           # Show density scale
     main = "Chi-Squared Distribution",
     xlab = "Value")               # X-axis label

curve(dchisq(x, df = k),
      add = TRUE,                  # Add curve to histogram
      col = "red",                 # Curve color
      lwd = 2)                     # Curve thickness

# Density plot
plot(density(chi2_data),
     col = "blue",                 # Density curve color
     lwd = 2,                      # Curve thickness
     main = "Density Plot",
     xlab = "Value")               # X-axis label

curve(dchisq(x, df = k),
      add = TRUE,                  # Add theoretical curve
      col = "red",
      lwd = 2)

# Q-Q plot
qqplot(qchisq(ppoints(N_sim), df = k),
       chi2_data,                  # Compare theoretical and simulated values
       main = "Q-Q Plot for Chi-Squared Data",
       col = "blue")

abline(0, 1,                      # Add y = x reference line
       col = "red",
       lwd = 2)







# ----------------LAB rePORT-------------
# Experiment no: 2

# Name of the experiment: Generation and
# analysis of χ²-distributed data

# Theory:
# The chi-square (χ²) distribution is widely
# used in statistics, particularly in hypothesis
# testing and confidence interval estimation.
# It is defined as the sum of the squares
# of k independent standard normal random
# variables:

# χ = Z₁² + Z₂² + ... + Zₖ²

# where Zᵢ ~ N(0,1). The mean and variance
# of χ²-distribution with K degrees of freedom
# are:
# • E[χ] = k
# • V[χ] = 2k

# Objective:
# The objective of this lab is to generate
# χ²-distribution data, compute empirical mean
# and variance, and compare them with
# theoretical values. Additionally, we visualize
# the distribution using histograms, density
# plots, and Q-Q plots.


# Pseudocode:

# 1. Set parameters: degrees of freedom and
#    number of simulations.
# 2. Generate random samples from a χ²-
#    distribution.
# 3. Compute empirical mean and variance.
# 4. Print computed mean and variance.
# 5. Generate and display:
#    • Histogram with theoretical χ² density
#      curve.
#    • Density plot with theoretical χ² curve.
#    • Q-Q plot comparing sample quantiles to
#      theoretical quantiles.
