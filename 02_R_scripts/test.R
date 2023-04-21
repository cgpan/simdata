library(MASS)

# Set the number of variables
n <- 4

diag(n)

# Generate random data from a multivariate normal distribution
data <- mvrnorm(n = 1000, mu = rep(0, n), Sigma = diag(n))


# Load the mtcars dataset
data(mtcars)

# Split the data into training and testing sets
train <- mtcars[1:20,]
test <- mtcars[21:32,]

# Define a function to calculate the mean squared error (MSE)
mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Fit polynomial regression models of different degrees
degrees <- 1:10
train_mses <- numeric(length = length(degrees))
test_mses <- numeric(length = length(degrees))

for (i in 1:length(degrees)) {
  degree <- degrees[i]
  formula <- as.formula(paste("mpg ~ poly(hp, ", degree, ")", sep = ""))
  
  # Train the model on the training data
  model <- lm(formula, data = train)
  
  # Predict the training and testing data
  train_pred <- predict(model, newdata = train)
  test_pred <- predict(model, newdata = test)
  
  # Calculate the mean squared error on the training and testing data
  train_mses[i] <- mse(train$mpg, train_pred)
  test_mses[i] <- mse(test$mpg, test_pred)
}

# Plot the results
plot(degrees, train_mses, type = "l", col = "blue", xlab = "Polynomial degree", ylab = "MSE")
lines(degrees, test_mses, col = "red")
legend("topright", legend = c("Training data", "Testing data"), col = c("blue", "red"), lty = 1)
