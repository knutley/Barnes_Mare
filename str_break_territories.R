#############################
# Structural Break Test 3.0 #
###### 24-02-2025 ##########
############################


# 1. Load Data # 

library(readr)
mare_clean <- read.csv("~/Downloads/mare_clean.csv") # This is the data where 
# the Manguinhos data has been added, the assunto_tipo has been standardised 
# and the den_contra variable corrected. 

# Now need to cut the 'tail' off the data, from February 2013. 

library(dplyr)
library(lubridate)
mare_clean1 <- mare_clean %>%
  filter(date <= "2013-03")
View(mare_clean1) # Right, here I've cut off anything after February 2013. 

# I've already performed the Box Jenkins Methodology on the aggregate level 
# (and you have the results of that in str_break_mare2.html). So, I'm going 
# to go ahead an assume that an ARMA model can be applied to the dataset's 
# constituent parts. 

# 2. Territory Structural Breaks # 

library(forecast)
library(ggfortify)
library(ggplot2)
library(readr)
library(strucchange)
library(tidyverse)
library(tseries)
library(zoo)

## 2.1 Defining CVNH Territories ## 

CVNH_neighbourhoods <- c("NOVA HOLANDA", "PARQUE MARE", "PARQUE RUBENS VAZ")
CVNH_grouped_data <-  mare_clean1%>%
  filter(den_comunidade %in% CVNH_neighbourhoods)
View(CVNH_grouped_data)

## 2.1.2 Wrangle CVNH Data into Matrix ## 

typeof(CVNH_grouped_data$date) # This needs to be corraled into a date format. 
CVNH_grouped_data$date <- as.Date(CVNH_grouped_data$date) # Repeated the command above, responded
# that it was a "double", so I believe it's recognised it as a date.

# Using the den_contra variable as the 'n' denunciations: 

CVNH_denunciations <- CVNH_grouped_data %>% 
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
View(CVNH_denunciations) 

# Turning that into a time-series matrix: 

CVNH_break_matrix <- data.frame(
  date = CVNH_denunciations$date,
  observations = CVNH_denunciations$n_denunciations
) %>%
  arrange(date) %>%
  mutate(
    year = format(date, "%Y"),
    month = format(date, "%m"),
    yearmonth = format(date, "%Y-%m")
  )

## 2.1.3 Convert CVNH Matrix into a Times Series Object ## 

CVNH_ts_denunciations <- ts(CVNH_break_matrix$observations, 
                       frequency = 12,  # Monthly data
                       start = c(as.numeric(format(min(CVNH_break_matrix$date), "%Y")),
                                 as.numeric(format(min(CVNH_break_matrix$date), "%m"))))
view(CVNH_ts_denunciations)

## 2.1.4 CVNH Exploratory Analysis ## 

# Plot CVNH time-series: 

plot(CVNH_ts_denunciations, 
     main = "CVNH Monthly Denunciations Over Time",
     ylab = "Number of Denunciations",
     xlab = "Time") # I hope that these legends are sufficient. 

# Decompose CVNH time-series: 

CVNH_ts_decomposition <- decompose(CVNH_ts_denunciations)
plot(CVNH_ts_decomposition)

## 2.1.5 CVNH Structural Break Test ## 

# Load necessary library
library(strucchange)

# Extract the observations for breakpoint analysis
CVNHX <- CVNH_break_matrix$observations

# Perform breakpoint test with 5 possible breaks
CVNH_bp_test <- breakpoints(CVNHX ~ time(CVNHX), breaks = 5, h = 0.025)

# Extract breakpoints
CVNH_bp <- breakpoints(CVNH_bp_test)

# Check if breakpoints were detected: 

if (!is.na(CVNH_bp[1])) {
  print(paste("Breakpoint found at observation:", CVNH_bp[1]))
  
  # Try to get confidence intervals
  tryCatch({
    conf_intervals <- confint(CVNH_bp_test)
    print(conf_intervals)
  }, error = function(e) {
    message("Could not compute confidence intervals")
  })
} else {
  message("No structural breaks detected in the data")
}

# Plot the time series with breakpoints for better visualization
plot(CVNHX, type = "l", col = "black", lwd = 2, 
     main = "CVNH Time Series with Structural Breaks",
     xlab = "Time", ylab = "Observations")

# Add the fitted breakpoints as lines
CVNH_breakpoints <- c(22, 26, 44, 56, 125)
abline(v = CVNH_breakpoints, col = "red", lty = 2, lwd = 2)

# 2.2 Defining CVPU Territories #

CVPU_neighbourhood <- "PARQUE UNIAO"
CVPU_grouped_data <- mare_clean1 %>%
  filter(den_comunidade %in% CVPU_neighbourhood)
View(CVPU_grouped_data)

## 2.2.1 Wrangle CVPU Data into Matrix ## 

typeof(CVPU_grouped_data$date)  
CVPU_grouped_data$date <- as.Date(CVPU_grouped_data$date) 

# Using the den_contra variable as the 'n' denunciations: 

CVPU_denunciations <- CVPU_grouped_data %>% 
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
View(CVPU_denunciations) 

# Turning that into a time-series matrix: 

CVPU_break_matrix <- data.frame(
  date = CVPU_denunciations$date,
  observations = CVPU_denunciations$n_denunciations
) %>%
  arrange(date) %>%
  mutate(
    year = format(date, "%Y"),
    month = format(date, "%m"),
    yearmonth = format(date, "%Y-%m")
  )

## 2.2.2 Convert CVPU Matrix into a Times Series Object ## 

CVPU_ts_denunciations <- ts(CVPU_break_matrix$observations, 
                            frequency = 12,  # Monthly data
                            start = c(as.numeric(format(min(CVPU_break_matrix$date), "%Y")),
                                      as.numeric(format(min(CVPU_break_matrix$date), "%m"))))
view(CVPU_ts_denunciations)

## 2.2.3 CVPU Exploratory Analysis ## 

# Plot CVPU time-series: 

plot(CVPU_ts_denunciations, 
     main = "CVPU Monthly Denunciations Over Time",
     ylab = "Number of Denunciations",
     xlab = "Time")

# Decompose CVPU time-series: 

CVPU_ts_decomposition <- decompose(CVPU_ts_denunciations)
plot(CVPU_ts_decomposition)

## 2.2.4 CVPU Structural Break Test ## 

# Extract the observations for breakpoint analysis
CVPUX <- CVPU_break_matrix$observations

# Perform breakpoint test with 5 possible breaks
CVPU_bp_test <- breakpoints(CVPUX ~ time(CVPUX), breaks = 5, h = 0.025)

# Extract breakpoints
CVPU_bp <- breakpoints(CVPU_bp_test)

# Check if breakpoints were detected: 

if (!is.na(CVPU_bp[1])) {
  print(paste("Breakpoint found at observation:", CVPU_bp[1]))
  
  # Try to get confidence intervals
  tryCatch({
    conf_intervals <- confint(CVPU_bp_test)
    print(conf_intervals)
  }, error = function(e) {
    message("Could not compute confidence intervals")
  })
} else {
  message("No structural breaks detected in the data")
}

# Plot the time series with breakpoints for better visualization
plot(CVPUX, type = "l", col = "black", lwd = 2, 
     main = "CVPU Time Series with Structural Breaks",
     xlab = "Time", ylab = "Observations")

# Add the fitted breakpoints as lines
CVPU_breakpoints <- c(25, 28, 125)
abline(v = CVPU_breakpoints, col = "red", lty = 2, lwd = 2)

## 2.3 Defining ADA Territories ## 

ADA_neighbourhoods <- c("VILA DO JOAO", "CONJUNTO ESPERANCA", "CONJUNTO PINHEIRO",
                        "VILA DO PINHEIROS", "VILA DO PINHEIROS(PE)", "SALSA MERENGUE")
ADA_grouped_data <- mare_clean1 %>%
  filter(den_comunidade %in% ADA_neighbourhoods)
View(ADA_grouped_data)

## 2.3.1 Wrangle ADA Data into Matrix ## 

typeof(ADA_grouped_data$date)  
ADA_grouped_data$date <- as.Date(ADA_grouped_data$date) 

# Using the den_contra variable as the 'n' denunciations: 

ADA_denunciations <- ADA_grouped_data %>% 
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
View(ADA_denunciations) 

# Turning that into a time-series matrix: 

ADA_break_matrix <- data.frame(
  date = ADA_denunciations$date,
  observations = ADA_denunciations$n_denunciations
) %>%
  arrange(date) %>%
  mutate(
    year = format(date, "%Y"),
    month = format(date, "%m"),
    yearmonth = format(date, "%Y-%m")
  )

## 2.3.2 Convert ADA Matrix into a Times Series Object ## 

ADA_ts_denunciations <- ts(ADA_break_matrix$observations, 
                            frequency = 12,  # Monthly data
                            start = c(as.numeric(format(min(ADA_break_matrix$date), "%Y")),
                                      as.numeric(format(min(ADA_break_matrix$date), "%m"))))
view(ADA_ts_denunciations)

## 2.3.3 ADA Exploratory Analysis ## 

# Plot ADA time-series: 

plot(ADA_ts_denunciations, 
     main = "ADA Monthly Denunciations Over Time",
     ylab = "Number of Denunciations",
     xlab = "Time")

# Decompose ADA time-series: 

ADA_ts_decomposition <- decompose(ADA_ts_denunciations)
plot(ADA_ts_decomposition)

## 2.3.4 ADA Structural Break Test ## 

# Extract the observations for breakpoint analysis
ADAX <- ADA_break_matrix$observations

# Perform breakpoint test with 5 possible breaks
ADA_bp_test <- breakpoints(ADAX ~ time(ADAX), breaks = 5, h = 0.025)

# Extract breakpoints
ADA_bp <- breakpoints(ADA_bp_test)

# Check if breakpoints were detected: 

if (!is.na(ADA_bp[1])) {
  print(paste("Breakpoint found at observation:", ADA_bp[1]))
  
  # Try to get confidence intervals
  tryCatch({
    conf_intervals <- confint(ADA_bp_test)
    print(conf_intervals)
  }, error = function(e) {
    message("Could not compute confidence intervals")
  })
} else {
  message("No structural breaks detected in the data")
}

# Plot the time series with breakpoints for better visualization
plot(ADAX, type = "l", col = "black", lwd = 2, 
     main = "ADA Time Series with Structural Breaks",
     xlab = "Time", ylab = "Observations")

# Add the fitted breakpoints as lines
ADA_breakpoints <- c(24, 29, 54, 85, 91)
abline(v = ADA_breakpoints, col = "red", lty = 2, lwd = 2)

# NB: The structural breaks at 85 and 91 were not the first order breaks; 
# the first order break was at 27, then consequently shifted to 24 and 29 
# respectively as the number of breaks was increased. So, my point is that
# even though the TCP consolidated this territory around this time, these
# were not the strongest shifts we see in the data writ large. 

## 2.4 Defining TCP Territories ## 

TCP_neighbourhoods <- c("NOVA MARE", "BAIXA DO SAPATEIRO", "MORRO DO TIMBAU",
                        "BENTO RIBEIRO DANTAS")
TCP_grouped_data <- mare_clean1 %>%
  filter(den_comunidade %in% TCP_neighbourhoods)
View(TCP_grouped_data)

## 2.4.1 Wrangle TCP Data into Matrix ## 

typeof(TCP_grouped_data$date)  
TCP_grouped_data$date <- as.Date(TCP_grouped_data$date) 

# Using the den_contra variable as the 'n' denunciations: 

TCP_denunciations <- TCP_grouped_data %>% 
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
View(TCP_denunciations) 

# Turning that into a time-series matrix: 

TCP_break_matrix <- data.frame(
  date = TCP_denunciations$date,
  observations = TCP_denunciations$n_denunciations
) %>%
  arrange(date) %>%
  mutate(
    year = format(date, "%Y"),
    month = format(date, "%m"),
    yearmonth = format(date, "%Y-%m")
  )

## 2.4.2 Convert TCP Matrix into a Times Series Object ## 

TCP_ts_denunciations <- ts(TCP_break_matrix$observations, 
                           frequency = 12,  # Monthly data
                           start = c(as.numeric(format(min(TCP_break_matrix$date), "%Y")),
                                     as.numeric(format(min(TCP_break_matrix$date), "%m"))))
view(TCP_ts_denunciations)

## 2.4.3 TCP Exploratory Analysis ## 

# Plot TCP time-series: 

plot(TCP_ts_denunciations, 
     main = "TCP Monthly Denunciations Over Time",
     ylab = "Number of Denunciations",
     xlab = "Time")

# Decompose TCP time-series: 

TCP_ts_decomposition <- decompose(TCP_ts_denunciations)
plot(TCP_ts_decomposition)

## 2.4.4 TCP Structural Break Test ## 

# Extract the observations for breakpoint analysis
TCPX <- TCP_break_matrix$observations

# Perform breakpoint test with 5 possible breaks
TCP_bp_test <- breakpoints(TCPX ~ time(TCPX), breaks = 5, h = 0.025)

# Extract breakpoints
TCP_bp <- breakpoints(TCP_bp_test)

# Check if breakpoints were detected: 

if (!is.na(TCP_bp[1])) {
  print(paste("Breakpoint found at observation:", TCP_bp[1]))
  
  # Try to get confidence intervals
  tryCatch({
    conf_intervals <- confint(TCP_bp_test)
    print(conf_intervals)
  }, error = function(e) {
    message("Could not compute confidence intervals")
  })
} else {
  message("No structural breaks detected in the data")
}

# Plot the time series with breakpoints for better visualization
plot(TCPX, type = "l", col = "black", lwd = 2, 
     main = "TCP Time Series with Structural Breaks",
     xlab = "Time", ylab = "Observations")

# Add the fitted breakpoints as lines
TCP_breakpoints <- c(32, 44, 47)
abline(v = TCP_breakpoints, col = "red", lty = 2, lwd = 2)

# NB: I attempted to render a structural break function with 1, 2, 3, 4, and 
# 5 breakpoints, but it will only recognise 3 breakpoints. Meaning that the shifts
# in your data here are sort of significantly frontloaded. According to the 
# regression you sent me, this was the only relationship of statistical significance. 
# The data, however, would suggest that this was due, in large part, to the selection
# bias implicit to partitioning the data at Sept 2009. 

