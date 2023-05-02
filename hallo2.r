set.seed(1)
n <- 20

# Generate sample with outliers
data_with_outliers <- c(rnorm(17, mean = 5, sd = 2), rnorm(3, mean = 20, sd = 5))

# Identify the outlier using the median absolute deviation (MAD) method
mad_with_outliers <- median(abs(data_with_outliers - median(data_with_outliers)))
outliers_with_outliers <- which(abs(data_with_outliers - median(data_with_outliers)) > 2 * mad_with_outliers)

# Compute type 1 error rate
alpha_with_outliers <- length(outliers_with_outliers) / n

# Set up a multi-panel layout for two plots side by side
par(mfrow=c(1,2))

# Plot the data with outliers
plot(data_with_outliers, pch = 16, cex = 1.5, col = "blue", xlab = "Observation", ylab = "Value")
if (length(outliers_with_outliers) > 0) {
  points(outliers_with_outliers, data_with_outliers[outliers_with_outliers], pch = 16, cex = 1.5, col = "red")
  legend("topleft", legend = "Outlier", pch = 16, col = "red", cex = 1.2)
}

# Replace the outliers with the median of the data
data_without_outliers <- data_with_outliers
data_without_outliers[outliers_with_outliers] <- median(data_with_outliers)

# Identify the outlier using the median absolute deviation (MAD) method
mad_without_outliers <- median(abs(data_without_outliers - median(data_without_outliers)))
outliers_without_outliers <- which(abs(data_without_outliers - median(data_without_outliers)) > 2 * mad_without_outliers)

# Compute type 1 error rate
alpha_without_outliers <- length(outliers_without_outliers) / n

# Plot the data without outliers
plot(data_without_outliers, pch = 16, cex = 1.5, col = "blue", xlab = "Observation", ylab = "Value")

# Print alpha values
cat("Type 1 error rate with outliers:", alpha_with_outliers, "\n")
cat("Type 1 error rate without outliers:", alpha_without_outliers, "\n")
