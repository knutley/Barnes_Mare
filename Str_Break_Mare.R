##########################
# Structural Break Test #
####### 17-01-2025 ######
########################

# Read in Data # 

# So, I'm just using the merged_df from the Final_Mare_Render.R because I'm just
# messing around. Now going through Nick's paper and going to replicate his 
# methodology. 

# 1. Box-Jenkins Test # 

# The Box-Jenkins methodology (also known as ARMA modeling) isn't actually a 
# single test, but rather a systematic approach to identify, estimate, and 
# diagnose time series models. The process involves analyzing a time series 
# dataset to find the best fit of an ARMA (AutoRegressive Moving Average) model.

# Identification --> Estimation --> Diagnosis --> Metadiagnosis --> Analysis 

# 1.1 Load Required Libraries # 

.libPaths("~/Documents/GitHub/Barnes_Mare")
install.packages('forecast')
library(forecast) # Error suggests there's a curl dependency; reinstall dependencies.

install.packages(c('curl', 'forecast'), dependencies = TRUE)
library(forecast) # Issue persists. 

install.packages('forecast', lib = '~/Documents/GitHub/Barnes_Mare', dependencies = TRUE)
library(forecast) # Issues persists. 

install.packages('forecast', dependencies = TRUE)
library(forecast) # Issue still persists; so what's the problem? So, is there 
# a library compatiblity issue? 

install.packages('curl', type = 'binary')
install.packages('forecast', type = 'binary')
library(forecast) # Nope... 

R.version # R version puppy cup (4.4.0), documentation for r forecast() says
# >R 3.5; going to remove and reinstall. 

remove.packages(c('curl', 'forecast'))
install.packages(c('curl', 'forecast'), dependencies = TRUE)
library(forecast) # Still, didn't work. 

install.packages(c('curl', 'forecast'), 
                 repos = "https://cloud.r-project.org/")
library(forecast) # Nope. 

install.packages(c('curl', 'forecast'), type = 'source', 
                 configure.vars = "CFLAGS=-fPIC", 
                 dependencies = TRUE)
library(forecast) # No. 

setwd("~/Documents/GitHub/Barnes_Mare")

remove.packages(c('curl', 'forecast'))
install.packages(c('curl', 'forecast'), type = 'binary')
.libPaths()  # Check current library paths

library(curl)
library(forecast)

library(tseries)
library(ggplot2)

# Function to perform Box-Jenkins analysis
box_jenkins_analysis <- function(data, seasonal = FALSE) {
  # Convert to time series object if not already
  if (!is.ts(data)) {
    data <- ts(data)
  }
  
  # Step 1: Identification
  # Check stationarity using ADF test
  adf_result <- adf.test(data)
  
  # Create ACF and PACF plots
  par(mfrow = c(2,1))
  acf(data, main = "ACF of Original Series")
  pacf(data, main = "PACF of Original Series")
  
  # Step 2: If data is non-stationary, difference it
  if (adf_result$p.value > 0.05) {
    diff_data <- diff(data)
    print("Data was non-stationary, taking first difference")
    
    # Check ACF and PACF of differenced data
    par(mfrow = c(2,1))
    acf(diff_data, main = "ACF of Differenced Series")
    pacf(diff_data, main = "PACF of Differenced Series")
  } else {
    diff_data <- data
    print("Data was already stationary")
  }
  
  # Step 3: Model Selection
  # Auto ARIMA to get best model
  if (seasonal) {
    model <- auto.arima(data, seasonal = TRUE)
  } else {
    model <- auto.arima(data, seasonal = FALSE)
  }
  
  # Step 4: Diagnostic Checking
  # Residual analysis
  residuals <- residuals(model)
  
  # Ljung-Box test for residual autocorrelation
  lb_test <- Box.test(residuals, lag = 10, type = "Ljung-Box")
  
  # Plot diagnostics
  checkresiduals(model)
  
  # Step 5: Forecasting
  forecast_values <- forecast(model, h = 12)  # Forecast next 12 periods
  plot(forecast_values)
  
  # Return results
  return(list(
    model = model,
    adf_test = adf_result,
    ljung_box_test = lb_test,
    forecasts = forecast_values
  ))
}

# Example usage with sample data
# Generate sample data
set.seed(123)
sample_data <- ts(rnorm(100) + 1:100/10, frequency = 12)

# Run analysis
results <- box_jenkins_analysis(sample_data, seasonal = TRUE)

# Print results
print("Model Summary:")
print(results$model)
print("\nADF Test Results:")
print(results$adf_test)
print("\nLjung-Box Test Results:")
print(results$ljung_box_test)