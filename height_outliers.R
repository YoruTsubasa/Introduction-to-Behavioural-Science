set.seed(21)
n <- 1000
mu <- 1.82
sigma <- 0.1
outlier_frac <- 0.1

# Generate normally distributed data with an outlier
data_with_outliers <- c(rnorm(n * (1 - outlier_frac), mean = mu, sd = sigma), rnorm(n * outlier_frac, mean = mu + rnorm(1, mean = 0, sd = 0.5), sd = sigma))


# Identify the outlier using the median absolute deviation (MAD) method
mad <- median(abs(data_with_outliers - median(data_with_outliers)))
outlier <- which(abs(data_with_outliers - median(data_with_outliers)) > 3 * mad)
outlier2 <- which(abs(data_with_outliers - median(data_with_outliers)) > 2 * mad)

# Replace the outliers with the median of the data
data_without_outliers <- data_with_outliers
data_without_outliers[outlier] <- median(data_with_outliers)
data_without_outliers2 <- data_with_outliers
data_without_outliers2[outlier2] <- median(data_with_outliers)



# Perform one-sample t-tests on the data with and without outliers
t_with_outliers <- t.test(data_with_outliers, mu = mu)
t_without_outliers <- t.test(data_without_outliers, mu = mu)
t_without_outliers2 <- t.test(data_without_outliers2, mu = mu)

# Extract the p-values from the t-tests
p_with_outliers <- t_with_outliers$p.value
p_without_outliers <- t_without_outliers$p.value
p_without_outliers2 <- t_without_outliers2$p.value





# Compute the mean and standard deviation of the data with outliers
mean_with_outliers <- mean(data_with_outliers)
sd_with_outliers <- sd(data_with_outliers)

# Compute the mean and standard deviation of the data without outliers
mean_without_outliers <- mean(data_without_outliers)
sd_without_outliers <- sd(data_without_outliers)

mean_without_outliers2 <- mean(data_without_outliers2)
sd_without_outliers2 <- sd(data_without_outliers2)

# Plot the data with outliers
hist(data_with_outliers, breaks = 20, col = "blue", xlab = "Height (m)", main = "Distribution with Outliers")

# Add a vertical line for the true mean
abline(v = mu, col = "red", lwd = 2)

# Add a vertical line for the mean with outliers
abline(v = mean_with_outliers, col = "green", lwd = 2)

# Plot the data without outliers
hist(data_without_outliers, breaks = 20, col = "blue", xlab = "Height (m)", main = "Distribution without Outliers")

# Add a vertical line for the true mean
abline(v = mu, col = "red", lwd = 2)

# Add a vertical line for the mean without outliers
abline(v = mean_without_outliers, col = "green", lwd = 2)







# Print the results
cat("P-value of one-sample t-test with outliers:", p_with_outliers, "\n")
cat("P-value of one-sample t-test without outliers:", p_without_outliers, "\n")
cat("P-value of one-sample t-test without outliers:", p_without_outliers2, "\n")

# Test for statistical significance
if (p_without_outliers < 0.05 & p_with_outliers  >= 0.05) {
  cat("Removing the outliers leads to a statistically significant difference in the mean height.\n")
} else {
  cat("Removing the outliers does not lead to a statistically significant difference in the mean height.\n")
}
