# Simulate data with an outlier
set.seed(123)
data <- c(rnorm(17, mean = 5, sd = 2), rnorm(3, mean = 20, sd = 5))

# Identify the outlier using the median absolute deviation (MAD) method
mad <- median(abs(data - median(data)))
outliers <- which(abs(data - median(data)) > 2 * mad)

# Set up a multi-panel layout for two plots side by side
par(mfrow=c(1,2))

# Plot the data with outliers
plot(data, pch = 16, cex = 1.5, col = "blue", xlab = "Observation", ylab = "Value")
if (length(outlier) > 0) {
  points(outlier, data[outlier], pch = 16, cex = 1.5, col = "red")
  legend("topleft", legend = "Outlier", pch = 16, col = "red", cex = 1.2)
}

# Replace the outliers with the median of the data
data_without_outliers <- data
data_without_outliers[outliers] <- median(data)

# Plot the data without outliers
plot(data_without_outliers, pch = 16, cex = 1.5, col = "blue", xlab = "Observation", ylab = "Value")