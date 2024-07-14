# Load necessary libraries
library(psych)
library(ggplot2)
library(DataExplorer)
library(car)
library(lmtest)
library(Metrics)
library(MASS)

View(FuelConsumption)



# Check structure and summary of the data
str(FuelConsumption)
summary(FuelConsumption)

# Check for missing values
plot_missing(FuelConsumption)

# Pairplot and correlation analysis
pairs.panels(FuelConsumption[, c("ENGINESIZE", "CYLINDERS", "FUELCONSUMPTION_CITY", "FUELCONSUMPTION_HWY", "FUELCONSUMPTION_COMB", "CO2EMISSIONS")])

plot_histogram(FuelConsumption)
plot_density(FuelConsumption)
plot_correlation(FuelConsumption)

# Set seed for reproducibility
set.seed(1234)

# Shuffle and split the data into training and testing sets
mixed <- FuelConsumption[order(runif(nrow(FuelConsumption))), ]
training <- mixed[1:round(0.7 * nrow(FuelConsumption)), ]
testing <- mixed[(round(0.7 * nrow(FuelConsumption)) + 1):nrow(FuelConsumption), ]

# Build a full model using all features
full_model <- lm(CO2EMISSIONS ~ ., data = training)
summary(full_model)



# Stepwise model selection
step_model <- stepAIC(fullmodel, direction = "both")
summary(step_model)

# Predict and evaluate on test data
full_model_pred <- predict(full_model, newdata = testing)
simple_model_pred <- predict(simple_model, newdata = testing)

# Calculate performance metrics
full_model_r2 <- summary(full_model)$r.squared
full_model_test_r2 <- cor(testing$CO2EMISSIONS, full_model_pred)^2

simple_model_r2 <- summary(simple_model)$r.squared
simple_model_test_r2 <- cor(testing$CO2EMISSIONS, simple_model_pred)^2

# Compare R-squared values
cat("Full Model - Train R2:", full_model_r2, "Test R2:", full_model_test_r2, "\n")
cat("Simplified Model - Train R2:", simple_model_r2, "Test R2:", simple_model_test_r2, "\n")
