set.seed(123)
n <- 1000
mu <- 1.82
sigma <- 0.1
outlier_frac <- 0.1

# Generate normally distributed data with an outlier
data_with_outliers <- c(rnorm(n * (1 - outlier_frac), mean = mu, sd = sigma), rnorm(n * outlier_frac, mean = mu + rnorm(1, mean = 0, sd = 0.5), sd = sigma))

# Identify the outlier using the median absolute deviation (MAD) method
mad <- median(abs(data_with_outliers - median(data_with_outliers)))
outlier <- which(abs(data_with_outliers - median(data_with_outliers)) > 3 * mad)

# Replace the outliers with the median of the data
data_without_outliers <- data_with_outliers
data_without_outliers[outlier] <- median(data_with_outliers)

# Compute the mean and standard deviation of the data with outliers
mean_with_outliers <- mean(data_with_outliers)
sd_with_outliers <- sd(data_with_outliers)

# Compute the mean and standard deviation of the data without outliers
mean_without_outliers <- mean(data_without_outliers)
sd_without_outliers <- sd(data_without_outliers)

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
