###################################
# Aggregate Structural Break Test #
########### 26-01-25 ##############

# 1. Load Data # 

library(readr)
mare_clean <- read.csv("~/Downloads/mare_clean.csv") 

# Again, this is the data that includes the smaller Manguinhos dataset, the 
# assunto_tipo variable has been standardised and the den_contra variable corrected. 

# 2. Box-Jenkins Methodology # 

# The Box-Jenkins methodology (also known as ARMA modeling) isn't actually a 
# single test, but rather a systematic approach to identify, estimate, and 
# diagnose time series models. The process involves analyzing a time series 
# dataset to find the best fit of an ARMA (AutoRegressive Moving Average) model.

# Identification --> Estimation --> Diagnosis --> Metadiagnosis --> Analysis 

## 2.1 Load Libraries ## 

library(forecast)
library(ggfortify)
library(ggplot2)
library(readr)
library(strucchange)
library(tidyverse)
library(tseries)
library(zoo)

## 2.2 Wrangle Data into Matrix ## 

# Right, so looking at the Resident Denunciation paper, his regression table
# leverages a CONQUEST variable, that is where the TCP took over ADA 
# territories in Sept 2009. And he suggests that this conquest impacted crime 
# denunciations over time. So, the matrix should include time and gang-related
# observations.

typeof(mare_clean$date) # This needs to be corraled into a date format. 
mare_clean$date <- as.Date(mare_clean$date) # Repeated the command above, responded
# that it was a "double", so it's recognised it as a date.

# Using den_contra variable as 'n' denunciations.

gang_denunciations <- mare_clean %>% 
  mutate(start = as.Date(date),
         end = as.Date(date)) %>%
  rowwise() %>%
  mutate(date = list(seq(start, end, by = "month"))) %>% 
  unnest(date) %>% 
  group_by(date) %>% 
  summarise(
    n_denunciations = sum(den_contra == 1, na.rm = TRUE)
  ) %>%
  complete(date = seq(min(date), max(date), by = "month"),
           fill = list(n_denunciations = 0))
View(gang_denunciations) 

# Turn into a times-series matrix: 

break_matrix <- data.frame(
  date = gang_denunciations$date,
  observations = gang_denunciations$n_denunciations
) %>%
  arrange(date) %>%
  mutate(
    year = format(date, "%Y"),
    month = format(date, "%m"),
    yearmonth = format(date, "%Y-%m")
  )

## 2.3 Convert to a Time Series Object ## 

ts_denunciations <- ts(break_matrix$observations, 
                       frequency = 12,  # Monthly data
                       start = c(as.numeric(format(min(break_matrix$date), "%Y")),
                                 as.numeric(format(min(break_matrix$date), "%m"))))
view(ts_denunciations)

## 2.4 Exploratory Analysis ## 

# Plot original time series: 

plot(ts_denunciations, 
     main = "Monthly Denunciations Over Time",
     ylab = "Number of Gang Denunciations",
     xlab = "Time") # Based on https://github.com/julzerinos/r-structural-breaks-with-ml
# this looks right? 

# Decompose time series: 

# This allows you to check whether the time series data is trended, seasonal, 
# or stochastic.

ts_decomposition <- decompose(ts_denunciations)
plot(ts_decomposition)

## 2.5 Stationarity Tests ## 

# Dickey-Fuller test:

adf_test <- adf.test(ts_denunciations)
# Here, I received a warning message saying 'the p-value is smaller than the 
# printed p-value'. 
print(adf_test) # The smaller p-value means that it rejects the null hypothesis 
# (the data is likely to be stationary). And then interpreting the test statistic, 
# as it's negative, that's stronger evidence against the null hypothesis meaning 
# that it is stationary. 

# KPSS Test: 

kpss_test <- kpss.test(ts_denunciations)
# Same warning here about p-values. 
print(kpss_test) # Null hyptohesis rejected, stationary data. 

# Just to note here, if the data was non-stationary, you'd have to 
# "different the series". 

## 2.6 Model Fitting ## 

# Auto ARIMA (this is integrative btw!) to determine best model:

arima_model <- auto.arima(ts_denunciations,
                          seasonal = TRUE,
                          stepwise = TRUE,
                          approximation = FALSE)
print(summary(arima_model)) 
# Okay, so this classifies the time series as an ARIMA (2,0,0) with a non-zero mean. 
# Referring to https://people.duke.edu/~rnau/411arim.htm, that means that this model 
# is a second-order auto-regressive model. Essentially, this is a statistical model 
# where the current value of a time series is predicted based on a linear combination
# of the two previous values in the series, plus a random error term. They also say
# (and idk if this is helpful), "there would be a Yt-2 term on the right as well, 
# and so on. Depending on the signs and magnitudes of the coefficients, an
# ARIMA(2,0,0) model could describe a system whose mean reversion takes place 
# in a sinusoidally oscillating fashion, like the motion of a mass on a spring 
# that is subjected to random shocks."  

## 2.7 Diagnostic Checks ## 

# Check residuals: 

checkresiduals(arima_model)

lb_test <- Box.test(residuals(arima_model), 
                    lag = min(10, length(residuals(arima_model)) - 1),
                    type = "Ljung-Box")
print(lb_test) # Gives you a Ljung-Box test here. That is a statistical
# test used within the Box Jenkins methodology to check if the residuals (errors)
# from a fitted model exhibit any significant autocorrelation, essentially 
# verifying the model's fit. 

# Looking at the high p-value (0.9822), there appears to be no significant
# evidence of autocorrelation in the residuals. This means the ARIMA (2,0,0) model
# has adequately captured the time series' structure, and the residuals look like 
# white noise (random). Total lags refers to the number of lag periods checked 
# for autocorrelation. 

## 2.8 Create Summary Output ## 

summary_stats <- data.frame(
  Metric = c("ADF p-value", "KPSS p-value", "Ljung-Box p-value", 
             "AIC", "BIC", "Mean Absolute Error"),
  Value = c(adf_test$p.value,
            kpss_test$p.value,
            lb_test$p.value,
            AIC(arima_model),
            BIC(arima_model),
            mean(abs(residuals(arima_model))))
)
print(summary_stats)

# 3. Structural Break Test # 

# Load required library for structural break analysis
library(strucchange)

# Preparing data for breakpoint analysis
X <- break_matrix$observations

# Test for structural changes
bp_test <- breakpoints(X ~ time(X), breaks = 5, h = 0.025)

# Extract breakpoints 
bp_results <- breakpoints(bp_test)

# Check if breakpoints were detected

if (!is.na(bp_results[1])) {
  print(paste("Breakpoint found at observation:", bp_results[1]))
  
  # Try to get confidence intervals
  tryCatch({
    conf_intervals <- confint(bp_test)
    print(conf_intervals)
  }, error = function(e) {
    message("Could not compute confidence intervals")
  })
} else {
  message("No structural breaks detected in the data")
} 

# There are 5 breakpoints -- 11, 22, 44, 62, and 142. 

# Plot the time series with breakpoints for better visualization
plot(X, type = "l", col = "black", lwd = 2, 
     main = "Mare Time Series with Structural Breaks",
     xlab = "Time", ylab = "Observations")

# Add the fitted breakpoints as lines
breakpoints <- c(11, 22, 44, 62, 142)
abline(v = breakpoints, col = "red", lty = 2, lwd = 2)

# Add abline for Sept 2009 for increased clarity. 

abline(v = 89, col = "green", lty = 2, lwd = 2)

# Add segment mean ablines for ease of reading. 

segments <- list(
  1:breakpoints[1],
  (breakpoints[1] + 1):breakpoints[2],
  (breakpoints[2] + 1):breakpoints[3],
  (breakpoints[3] + 1):breakpoints[4],
  (breakpoints[4] + 1):breakpoints[5],
  (breakpoints[5] + 1):length(X)
)

segment_means <- sapply(segments, function(seg) mean(X[seg]))

# Add horizontal lines for the mean of each segment
for (i in 1:length(segments)) {
  segment <- segments[[i]]
  lines(
    x = c(min(segment), max(segment)),
    y = c(segment_means[i], segment_means[i]),
    col = "blue",
    lwd = 1.5,
    lty = 1
  )
}

# Add a legend
legend("topright", 
       legend = c("Time Series", "Structural Breaks", "Segment Means", "Hypothesised Structural Break"),
       col = c("black", "red", "blue", "green"),
       lty = c(1, 2, 1),
       lwd = c(2, 2, 1.5),
       cex = 0.8)

# Window analysis to examine characteristics around September 2009
window_analysis <- function(data, date_col, obs_col, target_date, window_size = 12) {
  # Convert target date to index
  target_index <- which(data[[date_col]] >= as.Date(target_date))[1]
  
  # Create window
  window_start <- max(1, target_index - window_size)
  window_end <- min(nrow(data), target_index + window_size)
  
  # Extract window data
  window_data <- data[window_start:window_end, ]
  
  # Calculate summary statistics
  midpoint <- which(window_data[[date_col]] >= as.Date(target_date))[1]
  pre_data <- window_data[[obs_col]][1:(midpoint-1)]
  post_data <- window_data[[obs_col]][midpoint:nrow(window_data)]
  
  pre_mean <- mean(pre_data, na.rm = TRUE)
  post_mean <- mean(post_data, na.rm = TRUE)
  pre_sd <- sd(pre_data, na.rm = TRUE)
  post_sd <- sd(post_data, na.rm = TRUE)
  
  # Calculate relative change
  pct_change <- ((post_mean - pre_mean) / pre_mean) * 100
  
  return(list(
    window_data = window_data,
    pre_mean = pre_mean,
    post_mean = post_mean,
    pre_sd = pre_sd,
    post_sd = post_sd,
    pct_change = pct_change
  ))
}

# Apply the window analysis
window_results <- window_analysis(break_matrix, "date", "observations", "2009-09-01")

# Create visualization for window analysis
library(ggplot2)

ggplot(window_results$window_data, aes(x = date, y = observations)) +
  # Add the time series line with name for legend
  geom_line(aes(color = "Time Series")) +
  # Add vertical line for structural break with name for legend
  geom_vline(aes(xintercept = as.Date("2009-09-01"), 
                 color = "Hypothesised Structural Break"), 
             linetype = "dashed") +
  # Add horizontal lines for means with names for legend
  geom_hline(aes(yintercept = window_results$pre_mean, 
                 color = "Pre-mean"), 
             linetype = "dotted") +
  geom_hline(aes(yintercept = window_results$post_mean, 
                 color = "Post-mean"), 
             linetype = "dotted") +
  # Set colors manually
  scale_color_manual(name = "", 
                     values = c("Time Series" = "black",
                                "Hypothesised Structural Break" = "green",
                                "Pre-mean" = "blue",
                                "Post-mean" = "red")) +
  # Theme and labels
  theme_minimal() +
  labs(title = "Time Series Around September 2009",
       subtitle = paste("Percent change:", round(window_results$pct_change, 2), "%")) +
  # Position the legend at the top right
  theme(legend.position = "top",
        legend.justification = "right")

