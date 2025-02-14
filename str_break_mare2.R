#############################
# Structural Break Test 2.0 #
####### 07-01-2025 #########
###########################

# 1. Fixing 'den_contra' Variable #

## 1.1 Reading in Data ## 

library(readr)
mare_data <- read.csv("~/Downloads/mare_split.csv")
View(mare_data)

## 1.2 Load Libraries ## 

library(dplyr)

## 1.3 If-Else Statement to Correct 'den_contra' Variable ## 

table(mare_data$den_contra) # This is to see how many 0s and 1s I have before. 
unique(mare_data$crime_type) # Making sure that the crime_type column has been
# cleaned right and I can use this to assign values to the den_contra column.
mare_data$den_contra <- ifelse(mare_data$assunto_classe %in% c("CRIMES CONTRA A PESSOA",
                                                           "SUBSTANCIAS ENTORPECENTES",
                                                           "CRIMES CONTRA O PATRIMONIO",
                                                           "ARMAS DE FOGO E ARTEFATOS EXPLOSIVOS"),
                               1, 0) # Right, this seems to have worked okay. 
table(mare_data$den_contra) # This is to see how many 0s and 1s I have after. 
# The '1s' decreased by 331. A 1.6% decrease. That seems really weird? Especially
# as I added roughly 1000 observations through the Manguinhos data. Is it possible 
# that Nick included another type of crime as exclusively gang related? . I think 
# I will need to randomly sub-sample this and maybe blind code it to make sure 
# it's right? 

# First, I'm going to do a table function for both assunto_classe and crime_type
# to make sure that I haven't made an issue while bringing data over. 

table(mare_data$assunto_classe)
# ARMAS DE FOGO E ARTEFATOS EXPLOSIVOS -- 4480
# CRIMES CONTRA O PATRIMONIO -- 4287
# SUBSTANCIAS ENTORPECENTES -- 9635
# CRIMES CONTRA A PESSOA -- 2447 
# other -- 
table(mare_data$crime_type)
# ARMAS DE FOGO E ARTEFATOS EXPLOSIVOS -- 4480
# CRIMES CONTRA O PATRIMONIO -- 4287
# SUBSTANCIAS ENTORPECENTES -- 9635
# CRIMES CONTRA A PESSOA -- 2447

# Right, there's no discrepancy between these two things... 
# There's a difference in how he spelled CRIMES CONTRA O PATRIMONIO as 
# CRIMES CONTRA O PATRIMÔNIO and SUBSTANCIAS ENTORPECENTES as SUBSTÂNCIAS 
# ENTORPECENTES. 

# Okay, so we need to clean these again. 

mare_data$assunto_classe[mare_data$assunto_classe == "CRIMES CONTRA O PATRIMÔNIO"] <- "CRIMES CONTRA O PATRIMONIO"
table(mare_data$assunto_classe) # Right, this worked; we went from having 4287 
# observations for CRIMES CONTRA O PATRIMONIO and 143 obs for CRIMES CONTRA O PATRIMÔNIO
# to having 4430. Going to repeat this with Substancias. 

mare_data$assunto_classe[mare_data$assunto_classe == "SUBSTÂNCIAS ENTORPECENTES"] <- "SUBSTANCIAS ENTORPECENTES"
table(mare_data$assunto_classe)
# Went from having 9635 obs for SUBSTANCIAS ENTORPECENTES and 237 for SUBSTÂNCIAS 
# ENTORPECENTES to having 9872 obs for SUBSTANCIAS ENTORPECENTES. Going to re-run
# the commands above and correct the den_contra column. 
 
table(mare_data$den_contra) # 0 - 7878; 1 - 20849
mare_data$den_contra <- ifelse(mare_data$assunto_classe %in% c("CRIMES CONTRA A PESSOA",
                                                           "SUBSTANCIAS ENTORPECENTES",
                                                           "CRIMES CONTRA O PATRIMONIO",
                                                           "ARMAS DE FOGO E ARTEFATOS EXPLOSIVOS"),
                               1, 0)
table(mare_data$den_contra) # 0 - 7498; 1 - 21229. Okay, so these are all the 
# crimes that Nick suggests are gang-related. Moving forward with the Box-Jenkins 
# stuff. 

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

# Right, so looking at Nick's Resident Denunciation paper, the first structural
# break test leverages a CONQUEST variable, that is where the TCP took over ADA 
# territories in Sept 2009. And he suggests that this conquest impacted crime 
# denunciations over time. So, the matrix should include time and gang-related
# observations.

typeof(mare_data$date) # This needs to be coraled into a date format. 
mare_data$date <- as.Date(mare_data$date) # Repeated the command above, responded
# that it was a "double", so I believe it's recognised it as a date.

# Now, need to figure out how to use den_contra with the '1' values as the 
# denunciations. 

# Here is the original code for comparison: 

# denunciations <- mare_data %>% 
#  mutate(start = as.Date(date),
#         end = as.Date(date)) %>%
#  rowwise() %>%
#  mutate(date = list(seq(start, end, by = "month"))) %>% 
#  unnest(date) %>% 
#  group_by(date) %>% 
#  summarise(
#    n_denunciations = n()
#  ) %>%
#  complete(date = seq(min(date), max(date), by = "month"),
#           fill = list(n_denunciations = 0))
# View(denunciations)

gang_denunciations <- mare_data %>% 
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
View(gang_denunciations) # Comparing these two tables (denunciations vs. gang 
# denunciations), the number of gang denunciations has reduced. So, I believe 
# that his has worked. 

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
     xlab = "Time") # Right, so based on https://github.com/julzerinos/r-structural-breaks-with-ml
# this looks right? 

# Decompose time series: 

# I think this allows you to check whether the time series data is trended, seasonal, 
# or stochastic? 

ts_decomposition <- decompose(ts_denunciations)
plot(ts_decomposition)

## 2.5 Stationarity Tests ## 

# Dickey-Fuller test:

adf_test <- adf.test(ts_denunciations)
# I got a warning message saying, 'the p-value smaller than the printed p-value.'
# Need to refer to Pevehouse book to figure out what that means. 
print(adf_test) # Okay, went and looked and it seems that smaller p-value means that 
# it rejects the null hypothesis (the data is likely to be stationary). And then 
# interpreting the test statistic, as it's negative, that's stronger evidence against
# the null hypothesis meaning that it is stationary. 

# KPSS Test: 

kpss_test <- kpss.test(ts_denunciations)
# Same warning here about p-values. 
print(kpss_test) # Null hyptohesis rejected, stationary data. 

# Just to note here, is the data was non-stationary, you'd apparently have to 
# "different the series". 

## 2.6 Model Fitting ## 

# Auto ARIMA (this is integrative btw!!) to determine best model:

arima_model <- auto.arima(ts_denunciations,
                          seasonal = TRUE,
                          stepwise = TRUE,
                          approximation = FALSE)
print(summary(arima_model)) 
# Okay, so this classifies the time series as an ARIMA (2,0,0) with a non-zero mean. 
# Looking at https://people.duke.edu/~rnau/411arim.htm, that means that this model 
# is a second-order auto-regressive model. Essentially, this is a statistical model 
# where the current value of a time series is predicted based on a linear combination
# of the two previous values in the series, plus a random error term. They also say
# (and idk if this is helpful), "there would be a Yt-2 term on the right as well, 
# and so on.  Depending on the signs and magnitudes of the coefficients, an
# ARIMA(2,0,0) model could describe a system whose mean reversion takes place 
# in a sinusoidally oscillating fashion, like the motion of a mass on a spring 
# that is subjected to random shocks."  

## 2.7 Diagnostic Checks ## 

# Check residuals: 

checkresiduals(arima_model)

lb_test <- Box.test(residuals(arima_model), 
                    lag = min(10, length(residuals(arima_model)) - 1),
                    type = "Ljung-Box")
print(lb_test)# Gives you a Ljung-Box test here. That is a statistical
# test used within the Box Jenkins methodology to check if the residuals (errors)
# from a fitted model exhibit any significant autocorrelation, essentially 
# verifying the model's fit. 

# Okay, so looking at the high p-value (0.9822), there appears to be no significant
# evidence of autocorrelation in the residuals. This means the ARIMA (2,0,0) model
# has adequately captured the time series' structure, and the residuals look like 
# white noise (random). Total lags refers to the number of lag periods checked 
# for autocorrelation. 

# It's apparently possible to forecast for the next twelve or so months, but Nick 
# didn't ask for that, so I'm just going to leave that off. 

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

# Create a breakpoint variable for September 2009 -- that's when Nick said there
# was a lil' breakpoint with the gang takeover. 

 break_matrix <- break_matrix %>%
   mutate(breakpoint = ifelse(date >= as.Date("2009-09-01"), 1, 0))

##  3.1 Chow Test for Structural Break ## 

# Loading required library -- Jan Ditzen used 'strucchange' R package. 

library(strucchange)

# Preparing data for breakpoint analysis: 

 X <- break_matrix$observations
# breakdates <- which(break_matrix$date >= as.Date("2009-09-01"))[1] # Assumed breakpoint. 

# Perform breakpoint test: 

# bp_test <- breakpoints(X ~ 1, breaks = 1, h = 0.1) # h controls min segment size

# if (length(breakpoints(bp_test)$breakpoints) > 0) {
#  conf_intervals <- confint(bp_test)
# } else {
  # Handle the no-breakpoints case
# message("No structural breaks detected in the data")
# }

# summary(bp_test) 
# print(bp_test)

# Right, I had a really hard time reading these outputs. So, 
# having looked into it; breakpoints show the most likely points of change. At point
# 56 (out of presumably 145 months) there was a likely structural change -- likened
# to an anomaly. I'm a bit confused here because the 56th month in this longitudinal
# data is actually December 2006? (September 2009 being the 89th month.) It also 
# suggests that the statistical evidence is not strongly conclusive bc the BIC 
# values are so similar. And the 0.386... just suggests that the break occurred 
# roughly 38.6% of the way through the data. This doesn't seem right -- ask Nick?

# Going to power through and do the confidence intervals anyway, though? To give
# Nick the option of choice. 

# Confidence intervals for breakpoint: 


# ci_bp <- stats::confint(bp_test) # Threw up an error message saying, cannot compute
# confidence interval when 'breaks = 0'. Does that mean there isn't a break? 
# print(ci_bp) UPDATE: It means that there are multiple optimal breaks, I think. 

# Visualisation: 

# plot(bp_test) # I don't think that this graph is right? 

# F-test for structural break (Chow Test): 

# chow_test <- sctest(X ~ 1, type = "Chow", point = breakdates)
# print(chow_test) # Okay, this makes slightly more sense. the p-value is > 0.05, 
# (at approx. 0.08), meaning that there's very weak evidence of a strucutral break.
# Conventionally, we would suggest there's a failure to reject the null hypothesis
# here, but because it's so close to statistical significance we should probably 
# approach it a different way. It's possible that it may be a Type II error on my 
# end. (which it probs is.)

## 3.2 Chow Test 2.0 ## 

# The first one seemed a bit wonky, so I thought that maybe it would be better 
# to maybe try a breakpoint test where you're not imposing a singular break 
# (maybe there are a few?). 

# Re-creating the breakpoint vairable for Sept. 2009: 

# break_matrix <- break_matrix %>% 
#  mutate(breakpoint = ifelse(date >= as.Date("2009-09-01"), 1, 0))

# Preparing data for breakpoint analysis: 
# bp_test2 <- breakpoints(X ~1)
# summary(bp_test2) # Using strucchange with BIC criterion to determine optimal 
# number of breaks here. Okay, so here (AS I UNDERSTAND IT), this allows for multiple
# break points -- the RSS decreases indicating a better model fit overall. If you 
# wanted a single break (m = 1), it would be at obs. 56; two breaks (m = 2), at 
# obs 22 and 56; three breaks (m = 3), at 22, 56, and 123, etc. The BIC suggests
# that the optimal number of breaks is actually around 2-3, though (according
# to the lit, that balances model complexity and fit). 

# If breaks are found, then computing confidence intervals: 

# if(bp_test2$breakpoints[1] !=0){
#  ci_bp2 <- confint(bp_test2)
#  print(ci_bp2)
#} else{
#  cat("No significant breakpoints found using BIC criterion.\n")
#} # Suggests 3-segment partition is best. I couldn't find a lot of lit on how 
# to best read this, though. Leave for Nick to look at. 

# Visualisation: 

# plot(bp_test2) # This looks much more reasonable, I think? 

# F-test for structural break (Chow Test): 

# breakdate_index <- which(break_matrix$date >= as.Date("2009-09-01"))[1]
# chow_test2 <- sctest(X ~ 1, type = "Chow", point = breakdate_index)
# print(chow_test2) # Same chow statistic as before. So, see above. 

# Still an issue here, especially when trying to render an HTML. So, I'm going to
# try to sort it. 

# First, let's try with a specific focus on Nick's hypothesised break:
breakdate_index <- which(break_matrix$date >= as.Date("2009-09-01"))[1]

# Test for structural changes around that period

bp_test3 <- breakpoints(X ~ 1, h = 0.15)  # Increased h slightly for more stability

# For visualization and comprehensive analysis
plot(X, type = "l", main = "Time Series with Breakpoints")
lines(bp_test3)
abline(v = breakdate_index, col = "red", lty = 2)  # Add line for Sept 2009

# Safer version for your R Markdown document
bp_results <- breakpoints(X ~ 1)
if (!is.null(bp_results$breakpoints) && !all(is.na(bp_results$breakpoints))) {
  ci_results <- try(confint(bp_results), silent = TRUE)
  if (!inherits(ci_results, "try-error")) {
    print(ci_results)
  } else {
    message("Could not compute confidence intervals")
  }
} else {
  message("No significant breakpoints detected")
}

# Okay, so this shows three distinct segments with two structural breaks: (1) 
# first break around obs 22, (2) second break around 56. The breakdates show when these 
# breaks occur in proportion to the time series: (1) first break around 15.2% 
# through data, (2) second break around 38.6% through data. The wide conf. intervals
# suggest considerable uncertainty about the exact timing. Interestingly, the 
# hypothesised break point (Sept 2009) isn't being detexted as one of the major
# structural breaks. 

# Going to look at:
# (1) the characteristics of time series around Sept 2009.
# (2) alt. specifications of the breakpoint test that might be more sensitive to
# changes around that period. 

## 4.1 Examine characteristics around September 2009 ## 

# Create a window analysis

window_analysis <- function(data, date_col, obs_col, target_date, window_size = 12) {
  # Convert target date to index
  target_index <- which(data[[date_col]] >= as.Date(target_date))[1]
  
  # Create window
  window_start <- target_index - window_size
  window_end <- target_index + window_size
  
  # Extract window data
  window_data <- data[window_start:window_end, ]
  
  # Calculate summary statistics
  pre_mean <- mean(window_data[[obs_col]][1:window_size], na.rm = TRUE)
  post_mean <- mean(window_data[[obs_col]][(window_size+1):(2*window_size + 1)], na.rm = TRUE)
  pre_sd <- sd(window_data[[obs_col]][1:window_size], na.rm = TRUE)
  post_sd <- sd(window_data[[obs_col]][(window_size+1):(2*window_size + 1)], na.rm = TRUE)
  
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

## 4.2 Alternative specifications for breakpoint detection ##

alternative_specs <- function(data, obs_col) {
  # Try different minimum segment sizes
  bp_h10 <- breakpoints(data[[obs_col]] ~ 1, h = 0.10)
  bp_h15 <- breakpoints(data[[obs_col]] ~ 1, h = 0.15)
  bp_h20 <- breakpoints(data[[obs_col]] ~ 1, h = 0.20)
  
  # Try with different numbers of breaks
  bp_1break <- breakpoints(data[[obs_col]] ~ 1, breaks = 1)
  bp_2breaks <- breakpoints(data[[obs_col]] ~ 1, breaks = 2)
  bp_3breaks <- breakpoints(data[[obs_col]] ~ 1, breaks = 3)
  
  # F statistics for different window sizes
  f_stats <- Fstats(data[[obs_col]] ~ 1, from = 0.1, to = 0.9)
  
  return(list(
    h10 = bp_h10,
    h15 = bp_h15,
    h20 = bp_h20,
    one_break = bp_1break,
    two_breaks = bp_2breaks,
    three_breaks = bp_3breaks,
    f_statistics = f_stats
  ))
}

## 4.3 Apply the Analyses ##

window_results <- window_analysis(break_matrix, "date", "observations", "2009-09-01")
alt_specs <- alternative_specs(break_matrix, "observations")

## 4.4 Create visualisations ## 

ggplot(window_results$window_data, aes(x = date, y = observations)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2009-09-01"), color = "red", linetype = "dashed") +
  geom_hline(yintercept = window_results$pre_mean, color = "blue", linetype = "dotted") +
  geom_hline(yintercept = window_results$post_mean, color = "green", linetype = "dotted") +
  theme_minimal() +
  labs(title = "Time Series Around September 2009",
       subtitle = paste("Percent change:", round(window_results$pct_change, 2), "%"))

# To explain what we're seeing from these analyses: window analysis (12 mos before
# Sept 2009) -- pre-period mean (average level before Sept 2009), post-period 
# mean (average level after Sept 2009), percent change between these periods, 
# variability (standard deviation) in both these periods. 

# Need to interpret, but I think it will help me understand why the original 
# bp analysis didn't work. 