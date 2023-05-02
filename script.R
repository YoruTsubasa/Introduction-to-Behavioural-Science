# Removing outliers with adjusted criteria

#Type 1 error: Rejecting the null-hypothesis while it is true.
#Type 2 error: Failing to rejecting a null-hypothesis that is not true.
# QRPs are used to get the p-value below the alpha level aka p-hacking
# an outlier is 3 stddev above or below the mean
# selecting a particular criterion for the most desirable solution

# Simulate data with an outlier
set.seed(123)
data <- c(rnorm(9), 50)

# Identify the outlier using the median absolute deviation (MAD) method
mad <- median(abs(data - median(data)))
outlier <- which(abs(data - median(data)) > 3 * mad)

# Replace the outlier with the median of the data
data[outlier] <- median(data)

# Plot the data
plot(data, pch = 16, cex = 1.5, col = "blue", xlab = "Observation", ylab = "Value")
if (length(outlier) > 0) {
  points(outlier, data[outlier], pch = 16, cex = 1.5, col = "red")
  legend("topright", legend = "Outlier", pch = 16, col = "red", cex = 1.2)
}

