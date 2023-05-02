set.seed(6)
n <- 100

# Generate sample with outliers
x_with_outliers <- rnorm(n, mean = 0, sd = 1)
x_with_outliers[c(1, n)] <- c(-3, 3) # introduce outliers

# Calculate adjusted criteria with outliers
mean_with_outliers <- mean(x_with_outliers)
sd_with_outliers <- sd(x_with_outliers)
t_crit_with_outliers <- qt(0.975, n-1) # two-tailed t-test critical value
adj_crit_with_outliers <- mean_with_outliers + c(-1, 1) * t_crit_with_outliers * (sd_with_outliers / sqrt(n))

# Detect outliers with adjusted criteria
outliers_with_outliers <- x_with_outliers < adj_crit_with_outliers[1] | x_with_outliers > adj_crit_with_outliers[2]

# Compute type 1 error rate
alpha_with_outliers <- sum(outliers_with_outliers) / n

# Generate sample without outliers
x_without_outliers <- rnorm(n, mean = 0, sd = 1)

# Calculate adjusted criteria without outliers
mean_without_outliers <- mean(x_without_outliers)
sd_without_outliers <- sd(x_without_outliers)
t_crit_without_outliers <- qt(0.975, n-1) # two-tailed t-test critical value
adj_crit_without_outliers <- mean_without_outliers + c(-1, 1) * t_crit_without_outliers * (sd_without_outliers / sqrt(n))

# Detect outliers with adjusted criteria
outliers_without_outliers <- x_without_outliers < adj_crit_without_outliers[1] | x_without_outliers > adj_crit_without_outliers[2]

# Compute type 1 error rate
alpha_without_outliers <- sum(outliers_without_outliers) / n

# Print alpha values
cat("Type 1 error rate with outliers:", alpha_with_outliers, "\n")
cat("Type 1 error rate without outliers:", alpha_without_outliers, "\n")

# Plot data
par(mfrow = c(1, 2))
hist(x_with_outliers, main = "Sample with Outliers")
abline(v = adj_crit_with_outliers, col = "red")
hist(x_without_outliers, main = "Sample without Outliers")
abline(v = adj_crit_without_outliers, col = "red")
