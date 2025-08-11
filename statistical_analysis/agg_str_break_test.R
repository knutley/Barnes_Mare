# Title: Aggregate Structural Break Test - Resident Denunciation Project
# Author: Katelyn Nutley 
# Date: 25-07-2025

# Load required libraries
required_packages <- c("forecast", "ggfortify", "ggplot2", "lubridate", "readr",
                       "strucchange", "tidyverse", "tseries", "zoo")
lapply(required_packages, library, character.only = TRUE)

################################################################################
########################### STATIONARITY TESTS #################################
################################################################################

# Load filtered and labeled data
# df_labeled <- read.csv("data_with_labels.csv")  # Uncomment if loading from file

# Box Jenkins Methodology # 

# The Box-Jenkins methodology (also known as ARMA modeling) isn't actually a 
# single test, but rather a systematic approach to identify, estimate, and 
# diagnose time series models. The process involves analysing a time series 
# dataset to find the best fit of an ARMA (AutoRegressive Moving Average) model.

# Identification --> Estimation --> Diagnosis --> Metadiagnosis --> Analysis 

# Looking at the Resident Denunciation paper, Barnes' regression table leverages 
# a CONQUEST variable, that is where the TCP took over ADA territories in Sept 2009. 
# And he suggests that this conquest impacted crime denunciations over time. So,
# the matrix should include time and gang-related observations. Using den_contra 
# variable as 'n' denunciations.

# 1. Wrangle data into matrix

gang_denunciations <- df_labeled %>% 
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

# 2. Turn into a times-series matrix: 

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

# 3. Convert to a time series object 

ts_denunciations <- ts(break_matrix$observations, 
                       frequency = 12,  # Monthly data
                       start = c(as.numeric(format(min(break_matrix$date), "%Y")),
                                 as.numeric(format(min(break_matrix$date), "%m"))))

# 4. Exploratory analysis 

# Plot original time series: 

plot(ts_denunciations, 
     main = "Monthly Denunciations Over Time",
     ylab = "Number of Gang Denunciations",
     xlab = "Time") # Based on https://github.com/julzerinos/r-structural-breaks-with-ml
# this looks right. 

# Decompose time series: 

# This allows you to check whether the time series data is trended, seasonal, 
# or stochastic.

ts_decomposition <- decompose(ts_denunciations)
plot(ts_decomposition)

# 5. Stationarity tests 

# Dickey-Fuller test:

adf_test <- adf.test(ts_denunciations)
print(adf_test) # The time series decomposition plot reveals the underlying structure 
# of the denunciation data, while the Augmented Dickey-Fuller test results indicate
# that the time series is stationary. With a Dickey-Fuller statistic of -3.8242 and 
# a p-value of 0.01984 (less than 0.05), we can reject the null hypothesis of 
# non-stationarity at the 5% significance level. This means the denunciation time 
# series does not exhibit a unit root and has consistent statistical properties over time, 
# including stable mean and variance. The stationarity of the data is favourable for time 
# series modeling and forecasting, as it suggests the underlying patterns are relatively 
# consistent and predictable without requiring differencing or other transformations to achieve stationarity.

# TLDR;STATIONARY. Meaning you can use the Structural Break Test.   

# KPSS Test: 

kpss_test <- kpss.test(ts_denunciations)
print(kpss_test) # The KPSS test results complement the earlier ADF test findings 
# by providing additional confirmation of stationarity in the denunciation time series. 
# With a KPSS Level statistic of 0.29646 and a p-value of 0.1 (greater than 0.05), 
# we fail to reject the null hypothesis of level stationarity. This indicates that 
# the time series is stationary around a level (constant mean), meaning there are 
# no significant trends or structural breaks that would suggest non-stationarity. 
# The convergence of both the ADF test (rejecting non-stationarity) and the KPSS 
# test (accepting stationarity) provides robust evidence that the denunciation data 
# exhibits stable statistical properties over time, making it well-suited for standard 
# time series analysis techniques without requiring additional preprocessing steps like 
# differencing or detrending.

# 6. Model fitting  

# Auto ARIMA (this is integrative btw!) to determine best model:

arima_model <- auto.arima(ts_denunciations,
                          seasonal = TRUE,
                          stepwise = TRUE,
                          approximation = FALSE)
print(summary(arima_model)) 
# The auto.arima function selected an ARIMA(2,0,0) model for the denunciation time 
# series, which is essentially an autoregressive model of order 2 (AR(2)) with no 
# differencing required. This confirms the stationarity findings from the previous 
# tests, as the model doesn't need any differencing (d=0) to achieve stationarity. 
# The model indicates that current denunciation levels can be predicted based on the 
# two previous time periods, with autoregressive coefficients of 0.4569 and 0.1501, 
# and a mean level of approximately 119 denunciations. The model shows reasonable 
# fit with an RMSE of 46.51 and MAPE of 32.38%, though the relatively high error 
# measures suggest moderate forecasting accuracy. The sigma-squared value of 2209 
# indicates substantial residual variance, while the near-zero ACF1 value (0.007) 
# in the training set suggests the model has adequately captured the autocorrelation 
# structure in the data.

# 7.Diagnostic checks

# 7.1 Check residuals: 

checkresiduals(arima_model)

lb_test <- Box.test(residuals(arima_model), 
                    lag = min(10, length(residuals(arima_model)) - 1),
                    type = "Ljung-Box")
print(lb_test) # Gives you a Ljung-Box test. 

# The residual diagnostics from the ARIMA(2,0,0) model indicate excellent model 
# adequacy and suggest that the model has successfully captured the underlying time 
# series structure. Both Ljung-Box tests produce high p-values (0.8753 and 0.8352, 
# both well above 0.05), meaning we fail to reject the null hypothesis of independence 
# in the residuals. This indicates that the residuals behave like white noise with 
# no remaining autocorrelation patterns, which is exactly what we want in a well-fitted 
# time series model. The checkresiduals function tested 24 lags and found no 
# significant serial correlation (Q* = 14.688), while the additional Box-Ljung test 
# on 10 lags confirmed this finding (X-squared = 5.7578). These results validate that 
# the ARIMA(2,0,0) model has adequately captured the temporal dependencies in the 
# denunciation data, leaving behind residuals that appear to be random and unpredictable, 
# which is a key assumption for reliable forecasting.

# 8. Create summary output 

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
print(summary_stats) # Use my intrepetrations above, but please confer with someone
# more senior in the field

################################################################################
###################### STRUCTURAL BREAK ANALYSIS ###############################
################################################################################

# Structural Break Test # 

# 1. Prepare data for breakpoint analysis
X <- break_matrix$observations

# 2. Test for structural changes
bp_test <- breakpoints(X ~ time(X), breaks = 5, h = 0.025)

# 3. Extract breakpoints 
bp_results <- breakpoints(bp_test)

# 4. Check if breakpoints were detected

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

# There are 5 breakpoints -- 11, 17, 85, 102, 141. 

# 5. Plot the time series with breakpoints for better visualisation
plot(X, type = "l", col = "black", lwd = 2, 
     main = "Mare Time Series with Structural Breaks",
     xlab = "Time", ylab = "Observations")

# Add the fitted breakpoints as lines
breakpoints <- c(11, 27, 85, 102, 141)
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



# 6. Window analysis to examine characteristics around September 2009
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

# 7. Create visualisation for window analysis

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

# 8. Save both visualisations

