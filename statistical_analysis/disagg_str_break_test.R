# Title: Territory-Specific Structural Break Test - Resident Denunciation Project
# Author: Katelyn Nutley 
# Date: 26-07-2025

# If you have any questions, please refer to agg_str_break_test.R which goes through
# this bit by bit and interprets the results. 

# Load required libraries
required_packages <- c("forecast", "ggfortify", "ggplot2", "lubridate", "readr",
                       "strucchange", "tidyverse", "tseries", "zoo", "gridExtra")
lapply(required_packages, library, character.only = TRUE)

################################################################################
########################### TERRITORY-SPECIFIC ANALYSIS ########################
################################################################################

# Load filtered and labeled data
# df_labeled <- read.csv("data_with_labels.csv")  # Uncomment if loading from file

################################################################################
########################## CHOOSE TERRITORY ####################################
################################################################################

# Select territory to analyze: "TCP", "ADA", "milicia", "CVNH", or "CVPU"
TERRITORY <- "TCP"  # Change this to analyze different territories

################################################################################
############################## ANALYSIS ########################################
################################################################################

# 1. Prepare data
territory_data <- df_labeled %>% 
  filter(territory_type == TERRITORY) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date = floor_date(date, "month")) %>% 
  summarise(n_denunciations = sum(den_contra == 1, na.rm = TRUE), .groups = "drop") %>%
  complete(date = seq(min(df_labeled$date, na.rm = TRUE), 
                      max(df_labeled$date, na.rm = TRUE), by = "month"),
           fill = list(n_denunciations = 0)) %>%
  arrange(date)

# 2. Create time series
ts_data <- ts(territory_data$n_denunciations, frequency = 12,
              start = c(year(min(territory_data$date)), month(min(territory_data$date))))

# 3. Stationarity tests
adf_test <- adf.test(ts_data)
kpss_test <- kpss.test(ts_data)

cat("Stationarity Tests:\n")
cat("ADF p-value:", round(adf_test$p.value, 4), 
    "- Series is", ifelse(adf_test$p.value < 0.05, "STATIONARY", "NON-STATIONARY"), "\n")
cat("KPSS p-value:", round(kpss_test$p.value, 4), 
    "- Series is", ifelse(kpss_test$p.value > 0.05, "STATIONARY", "NON-STATIONARY"), "\n\n")

# 4. ARIMA model
arima_model <- auto.arima(ts_data, seasonal = TRUE)
lb_test <- Box.test(residuals(arima_model), type = "Ljung-Box")

cat("ARIMA Model:\n")
print(summary(arima_model))
cat("Ljung-Box p-value:", round(lb_test$p.value, 4), 
    "- Residuals are", ifelse(lb_test$p.value > 0.05, "WHITE NOISE", "AUTOCORRELATED"), "\n\n")

# 5. Structural breaks
X <- territory_data$n_denunciations
bp_test <- breakpoints(X ~ seq_along(X), breaks = 5, h = 0.025)
bp_results <- breakpoints(bp_test)

cat("Structural Breaks:\n")
if (!is.null(bp_results) && length(bp_results) > 0) {
  # Extract the actual breakpoint indices
  bp_indices <- bp_results$breakpoints
  if (!is.null(bp_indices) && !all(is.na(bp_indices))) {
    breakpoint_dates <- territory_data$date[bp_indices]
    cat("Breakpoints at observations:", paste(bp_indices, collapse = ", "), "\n")
    cat("Breakpoint dates:", paste(as.character(breakpoint_dates), collapse = ", "), "\n")
  } else {
    cat("No structural breaks detected\n")
  }
} else {
  cat("No structural breaks detected\n")
}

# 6. September 2009 analysis
sept_index <- which.min(abs(territory_data$date - as.Date("2009-09-01")))
window_size <- 12

pre_data <- X[(sept_index - window_size):(sept_index - 1)]
post_data <- X[sept_index:(sept_index + window_size)]

pre_mean <- mean(pre_data, na.rm = TRUE)
post_mean <- mean(post_data, na.rm = TRUE)
pct_change <- ((post_mean - pre_mean) / pre_mean) * 100

cat("\nSeptember 2009 Analysis:\n")
cat("Pre-Sept mean:", round(pre_mean, 2), "\n")
cat("Post-Sept mean:", round(post_mean, 2), "\n")
cat("Percentage change:", round(pct_change, 2), "%\n\n")

# 7. Plots
# Time series with breaks
plot(X, type = "l", main = paste(TERRITORY, "- Structural Breaks"),
     xlab = "Time", ylab = "Denunciations")
if (!is.null(bp_results) && length(bp_results) > 0) {
  bp_indices <- bp_results$breakpoints
  if (!is.null(bp_indices) && !all(is.na(bp_indices))) {
    abline(v = bp_indices, col = "red", lty = 2)
  }
}
abline(v = sept_index, col = "green", lty = 2)
legend("topright", c("Data", "Breaks", "Sept 2009"), 
       col = c("black", "red", "green"), lty = c(1, 2, 2)

# Window plot
window_data <- territory_data[(sept_index - window_size):(sept_index + window_size), ]
ggplot(window_data, aes(x = date, y = n_denunciations)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2009-09-01"), color = "green", linetype = "dashed") +
  geom_hline(yintercept = pre_mean, color = "blue", linetype = "dotted") +
  geom_hline(yintercept = post_mean, color = "red", linetype = "dotted") +
  labs(title = paste(TERRITORY, "- September 2009 Window"),
       subtitle = paste("Change:", round(pct_change, 1), "%")) +
  theme_minimal()

################################################################################
# To analyse another territory, change TERRITORY variable at top and re-run
################################################################################
