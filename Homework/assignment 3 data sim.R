# Set the random seed for reproducibility
set.seed(123)

# Load the required packages
library(dynlm)

# Generate covariate
n <- 200 # number of observations
covariate <- rnorm(n, mean = 3)

# Generate ARMA(2,1) time series with covariate
phi1 <- 0.7
phi2 <- -0.2
theta1 <- 0.5
eps <- rnorm(n)
ar <- arima.sim(list(order = c(2,0,1), ar = c(phi1, phi2), ma = theta1), n, rand.gen = function(n) eps)
ar <- ar + 0.5*covariate + 5
ts_data <- ts(ar, start = 1, frequency = 1)

# Fit an ARDL model to the entire data set
ardl_model <- dynlm(ts_data ~ L(ts_data, 1:2) + covariate + L(covariate, 1))
summary(ardl_model)

# Make predictions on the entire data set using the ARDL model
ts_preds <- predict(ardl_model, start = 1, end = n)

# Plot the predicted values against the actual values
plot(ts_data, main = "ARMA(2,1) Time Series with Covariate")
lines(ts_preds, col = "red")
legend("topright", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = 1)

sim_series <- data.frame(y = ar, x = covariate)
