# Parameters
df1 <- 5                 # First chi-square degrees of freedom
df2 <- 10                # Second chi-square degrees of freedom
N_sim <- 1000            # Number of simulations

# Generate F-distributed data
set.seed(123)             # Set seed for reproducible results

chi2_1 <- rchisq(N_sim, df = df1)   # Generate 1000 chi-square values (df = 5)
chi2_2 <- rchisq(N_sim, df = df2)   # Generate 1000 chi-square values (df = 10)

# F = (Chi-square 1 / df1) / (Chi-square 2 / df2)
f_data <- (chi2_1 / df1) / (chi2_2 / df2)   # Generate F-distributed data


# Graphical Output
par(mfrow = c(2, 2))       # Arrange 4 plots in 2 × 2 layout


# Histogram
hist(f_data,
     breaks = 30,           # Number of histogram intervals
     col = "lightblue",     # Histogram color
     probability = TRUE,    # Use density scale
     main = "F-Distribution", # Plot title
     xlab = "Value")        # X-axis label

curve(df(x, df1 = df1, df2 = df2),
      add = TRUE,            # Add curve to histogram
      col = "red",           # Curve color
      lwd = 2)               # Curve line width


# Density Plot
plot(density(f_data),
     col = "blue",           # Density curve color
     lwd = 2,                # Curve line width
     main = "Density Plot",  # Plot title
     xlab = "Value")         # X-axis label

curve(df(x, df1 = df1, df2 = df2),
      add = TRUE,            # Add theoretical F curve
      col = "red",
      lwd = 2)


# Q-Q Plot
qqplot(qf(ppoints(N_sim), df1 = df1, df2 = df2),
       f_data,               # Compare theoretical and simulated values
       main = "Q-Q Plot for F-Distribution",
       col = "blue")

abline(0, 1,                # Add reference line y = x
       col = "red",
       lwd = 2)


# Boxplot
boxplot(f_data,
        col = "lightgreen",  # Boxplot color
        main = "Boxplot of F-Distributed Data",
        ylab = "Value")      # Y-axis label