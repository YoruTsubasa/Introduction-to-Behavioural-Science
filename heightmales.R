# Introduction to Behavioral Data Science assignment
# Janic Bijlhout, Matteo Dröge
# distribution of male height with and without outliers
# to prove that a type 1 error may occur when removing outliers (QRP)

set.seed(21) # seed 21 proves the type 1 error
n <- 1000 # size of data
mu <- 1.82 # average height in males
sigma <- 0.1 # standard dev.
fraction_of_outlier <- 0.1 # fraction for how much the outliers differ

# generate normal distributed data having outliers
data_with_outliers <- c(rnorm(n * (1 - fraction_of_outlier), mean = mu, sd = sigma),
                        rnorm(n * fraction_of_outlier, mean = mu + rnorm(1, mean = 0, sd = 0.5), sd = sigma))

# use median abs dev to find outliers in data
mad <- median(abs(data_with_outliers - median(data_with_outliers)))
outlier <- which(abs(data_with_outliers - median(data_with_outliers)) > 3 * mad)

# replace the outliers by swapping them for the median values of data 
data_without_outliers <- data_with_outliers # create new data set based of prev.
data_without_outliers[outlier] <- median(data_with_outliers) # replace outliers

# do t-tests, get p values and calc mean for both datasets
t_with_outliers <- t.test(data_with_outliers, mu = mu)# do t tests
t_without_outliers <- t.test(data_without_outliers, mu = mu)

p_with_outliers <- t_with_outliers$p.value # get p vals from t tests
p_without_outliers <- t_without_outliers$p.value 

mean_with_outliers <- mean(data_with_outliers) # get means 
mean_without_outliers <- mean(data_without_outliers) 

# plotting data with outliers
hist(data_with_outliers, breaks = 20, col = "blue", 
     xlab = "Height", main = "Distribution of data with outliers")
abline(v = mu, col = "red", lwd = 2) # the true average male height
abline(v = mean_with_outliers, col = "green", lwd = 2) # mean with outliers

# same plotting for data without outliers
hist(data_without_outliers, breaks = 20, col = "blue", 
     xlab = "Height", main = "Distribution of data without outliers")
abline(v = mu, col = "red", lwd = 2) # mean height
abline(v = mean_without_outliers, col = "green", lwd = 2) # mean without outliers


# print p vals for both data sets
cat("p-value of t-test on data with outliers:", p_with_outliers, "\n")
cat("p-value of t-test on data without outliers:", p_without_outliers, "\n")

if (p_without_outliers < 0.05 & p_with_outliers >= 0.05) {
  cat("Removing outliers leads to a statistically significant difference in the mean height
because p_without_outliers < 0.05 and p_with_outliers >= 0.05")
} else {
  cat("Removing outliers won't lead to a statistically significant difference\n")
}
