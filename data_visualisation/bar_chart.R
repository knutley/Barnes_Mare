# Title: Bar Plot for Denunciations, Police Interventions, and Shootouts 
# Author: Katelyn Nutley 
# Date: 31-07-2025

# Load Libraries
required_packages <- c("dplyr", "ggplot2", "ggridges", "lubridate", "readr", "tidyr",
                       "scales")
lapply(required_packages, library, character.only = TRUE)

# Load Data
df_labeled <- read_csv("~/Documents/Github/Barnes_Mare/data_with_labels.csv")
# Un-hash this if you need to upload it

# 1. Identify Variable Names 
colnames(df_labeled) 
# den_operação is the variable denoting whether there was a police operation 
View(df_labeled)
# "control F-ed" the term "tiroteio" which means "shootout in Portuguese.  
# Now, looking at the data, the term "tiroteio" shows up in the variable assunto_tipo. 
# And then denunciations are just a simple count of every observation, which have been  
# cleaned already. 

# 2. Create summary data for plotting 
plot_data <- df_labeled %>%
  mutate(
    year = year(date),
    month = month(date),
    year_month = floor_date(date, "month")  # Create year-month grouping
  ) %>%
  group_by(year_month, year, month) %>%
  summarise(
    Denunciations = n(),  # Count all observations
    Police_Operations = sum(den_operação == 1, na.rm = TRUE),
    Shootouts = sum(grepl("tiroteio", assunto_tipo, ignore.case = TRUE), na.rm = TRUE),
    .groups = 'drop'
  ) 
View(plot_data)

# 3. Create Chart Function 

crime_chart <- function(plot_data) {
  
  # Calculate scale factor
  max_denunciations <- max(plot_data$Denunciations, na.rm = TRUE)
  max_events <- max(c(plot_data$Police_Operations, plot_data$Shootouts), na.rm = TRUE)
  scale_factor <- ifelse(max_events > 0, max_denunciations / max_events, 1)
  
  # Prepare long format data for dodging
  bars_data <- plot_data %>%
    select(year_month, Police_Operations, Shootouts) %>%
    pivot_longer(cols = c("Police_Operations", "Shootouts"),
                 names_to = "Event_Type", values_to = "Count") %>%
    mutate(
      Scaled_Count = Count * scale_factor,
      Event_Type = factor(Event_Type,
                          levels = c("Police_Operations", "Shootouts"),
                          labels = c("Police Operations", "Shootouts"))
    )
  
  # Create plot with side-by-side bars
  p <- ggplot() +
    
    # Dodged bars
    geom_col(data = bars_data,
             aes(x = year_month, y = Scaled_Count, fill = Event_Type),
             position = position_dodge(width = 25),
             width = 20,
             alpha = 0.8) +
    
    # Denunciations line
    geom_line(data = plot_data,
              aes(x = year_month, y = Denunciations), 
              color = "#1A202C", size = 1.5, alpha = 0.9) +
    
    # Color scheme
    scale_fill_manual(values = c("Police Operations" = "#4A5568", 
                                 "Shootouts" = "#E53E3E")) +
    
    # Axes
    scale_y_continuous(
      name = "Number of Denunciations",
      labels = comma_format(),
      sec.axis = sec_axis(
        trans = ~ . / scale_factor,
        name = "Police Operations & Shootouts",
        labels = comma_format()
      )
    ) +
    
    scale_x_date(
      name = "Time Period",
      date_labels = "%Y",
      date_breaks = "1 year",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    
    # Theme
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 13, color = "#718096", hjust = 0.5, margin = margin(b = 20)),
      panel.grid.major = element_line(color = "#E2E8F0", size = 0.5),
      panel.grid.minor = element_line(color = "#F7FAFC", size = 0.3),
      axis.text = element_text(color = "#4A5568", size = 11),
      axis.title = element_text(color = "#2D3748", size = 12),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.caption = element_text(size = 9, color = "#718096", hjust = 0.5, 
                                  margin = margin(t = 10)),
      plot.margin = margin(20, 20, 15, 15)
    ) +
    
    labs(
      title = "Crime Events Analysis",
      subtitle = "Temporal patterns with side-by-side police operations and shootouts",
      caption = "Bars scaled for visilibility",
      fill = "Event Type"
    )
  
  return(p)
}
crime_chart(plot_data)
