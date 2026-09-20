# Lab 10: Derive MLEs for Binomial, Poisson, and Normal Distributions

# Binomial MLE
n_binom <- 20          # Number of trials
p_true <- 0.6          # True success probability
data_binom <- rbinom(100, size = n_binom, prob = p_true)  # Generate 100 Binomial observations
p_mle <- mean(data_binom) / n_binom  # MLE of p = sample mean / n

# Poisson MLE
lambda_true <- 3       # True Poisson parameter
data_pois <- rpois(100, lambda = lambda_true)  # Generate 100 Poisson observations
lambda_mle <- mean(data_pois)  # MLE of lambda = sample mean

# Normal MLE
mu_true <- 5           # True population mean
sigma_true <- 2        # True population standard deviation
data_norm <- rnorm(100, mean = mu_true, sd = sigma_true)  # Generate 100 Normal observations
mu_mle <- mean(data_norm)  # MLE of mu = sample mean
sigma_mle <- sqrt(mean((data_norm - mu_mle)^2))  # MLE of sigma using denominator n

# Output
print(paste("Binomial MLE for p:", p_mle))  # Display estimated p
print(paste("Poisson MLE for lambda:", lambda_mle))  # Display estimated lambda
print(paste("Normal MLE for mu:", mu_mle))  # Display estimated mu
print(paste("Normal MLE for sigma:", sigma_mle))  # Display estimated sigma

# Graphical Output
par(mfrow = c(1, 3))  # Display three plots side by side

# Binomial Histogram
hist(data_binom,
     breaks = 10,                    # Number of histogram intervals
     col = "lightblue",              # Histogram color
     main = "Binomial Distribution", # Graph title
     xlab = "Value",                 # X-axis label
     ylab = "Frequency",             # Y-axis label
     probability = TRUE)             # Show probability scale

abline(v = mean(data_binom), col = "red", lwd = 2)  # Add MLE line


# Poisson Histogram
hist(data_pois,
     breaks = 10,                   # Number of histogram intervals
     col = "lightgreen",             # Histogram color
     main = "Poisson Distribution",  # Graph title
     xlab = "Value",                # X-axis label
     ylab = "Frequency",             # Y-axis label
     probability = TRUE)             # Show probability scale

abline(v = lambda_mle, col = "red", lwd = 2)  # Add MLE line


# Normal Histogram
hist(data_norm,
     breaks = 10,                  # Number of histogram intervals
     col = "lightcoral",            # Histogram color
     main = "Normal Distribution",  # Graph title
     xlab = "Value",                # X-axis label
     ylab = "Frequency",             # Y-axis label
     probability = TRUE)             # Show probability scale

abline(v = mu_mle, col = "red", lwd = 2)  # Add MLE line