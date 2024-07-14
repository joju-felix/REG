# View the first few rows of the dataset
head(FuelConsumption)

# Create model matrix (including dummy variables for State)
X <- model.matrix(CO2EMISSIONS ~ ., FuelConsumption)[, -1]  # Exclude the intercept term

# Separate the target variable
Y <- FuelConsumption$CO2EMISSIONS

# Define the lambda sequence
lambda <- 10^seq(10, -2, length = 100)
print(lambda)
# Split the data into training and validation sets
set.seed(567)
part <- sample(2, nrow(X), replace = TRUE, prob = c(0.7, 0.3))
X_train <- X[part == 1, ]
X_cv <- X[part == 2, ]
Y_train <- Y[part == 1]
Y_cv <- Y[part == 2]

library(glmnet)

# Perform Ridge regression
ridge_reg <- glmnet(X_train, Y_train, alpha = 0, lambda = lambda)
summary(ridge_reg)

# Find the best lambda via cross-validation
ridge_reg1 <- cv.glmnet(X_train, Y_train, alpha = 0)
bestlam <- ridge_reg1$lambda.min
print(bestlam)


# Predict on the validation set
ridge.pred <- predict(ridge_reg, s = bestlam, newx = X_cv)

# Calculate mean squared error
mse <- mean((Y_cv - ridge.pred)^2)
print(paste("Mean Squared Error:", mse))

# Calculate R2 value
sst <- sum((Y_cv - mean(Y_cv))^2)
sse <- sum((Y_cv - ridge.pred)^2)
r2 <- 1 - (sse / sst)
print(paste("R²:", r2))


# Get the Ridge regression coefficients
ridge.coef <- predict(ridge_reg, type = "coefficients", s = bestlam)
print("Ridge Coefficients:")
print(ridge.coef)


# Perform Lasso regression
lasso_reg <- glmnet(X_train, Y_train, alpha = 1, lambda = lambda)

# Find the best lambda via cross-validation
lasso_reg1 <- cv.glmnet(X_train, Y_train, alpha = 1)
bestlam <- lasso_reg1$lambda.min
bestlam

# Predict on the validation set
lasso.pred <- predict(lasso_reg, s = bestlam, newx = X_cv)

# Calculate mean squared error
mse <- mean((Y_cv - lasso.pred)^2)
print(paste("Mean Squared Error:", mse))

# Calculate R2 value
sst <- sum((Y_cv - mean(Y_cv))^2)
sse <- sum((Y_cv - lasso.pred)^2)
r2 <- 1 - (sse / sst)
print(paste("R²:", r2))

# Get the Lasso regression coefficients
lasso.coef <- predict(lasso_reg, type = "coefficients", s = bestlam)
print("Lasso Coefficients:")
print(lasso.coef)
