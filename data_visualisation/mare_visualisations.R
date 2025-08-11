# Title: Mare Crime Visualisation - Resident Denunciation Project
# Author: Katelyn Nutley
# Date: 01-08-2025

# Load Libraries
required_packages <- c("dplyr", "ggplot2", "ggridges", "ggstream", "extrafont", 
                       "lubridate", "readr", "tidyr", "scales", "ggpubr", "purrr")
lapply(required_packages, library, character.only = TRUE)

# Load Data
df_labeled <- read_csv("~/Documents/Github/Barnes_Mare/data_with_labels.csv")

# Color palette and labels
pal2 <- c("#003f5c", "#d45087", "#ffa600", "#665191", "#ff7c43")

# 1. Calculate Count and Percentage 

total_area_data <- df_labeled %>%
  group_by(date, crime_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(date) %>%
  mutate(percentage = count / sum(count))
View(total_area_data) # This looks good. If you're curious as to how this breaks
# down, please look at this table. 

# 2. Visualise by Count

mare_stream2 <- total_area_data %>%
  arrange(count) %>%   
  ggplot(aes(date, count, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "ridge", bw = 0.185) + 
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives",
                                              "Assault", 
                                              "Property Crime",
                                              "Narcotic Substances")) +   
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +   
  coord_cartesian(clip = "off") +   
  labs(
    title = "Maré Crime by Denunciation Count, Time, and Type",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "none") + 
  # Annotate Narcotics
  annotate("text", x = max(date), y = 100,
           label = "Narcotics",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[5]) +
  # Annotate Property Crime
  annotate("text", x = max(date), y = 215,
           label = "Property \nCrime",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[4]) +
  # Annotate Assault
  annotate("text", x = max(date), y = 270,
           label = "Assault",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[3]) +
  # Annotate Firearms and Explosives
  annotate("text", x = max(date), y = 300,
           label = "Firearms & \nExplosives",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[2]) +
  # Annotate Other
  annotate("text", x = max(date), y = 350,
           label = "Other",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[1])
mare_stream2 

# 6 month intervalisation: 

mare_stream3 <- total_area_data %>%
  arrange(count) %>%   
  ggplot(aes(date, count, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "ridge", bw = 0.185) + 
  scale_fill_manual(values = pal2) +   
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +   
  coord_cartesian(clip = "off") +   
  labs(
    title = "Maré Crime by Denunciation Count, Time, and Type",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "none")

mare_stream4 <- mare_stream3 +
  # Annotate Narcotics
  annotate("text", x = max(date), y = 100,
           label = "Narcotics",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[5]) +
  # Annotate Property Crime
  annotate("text", x = max(date), y = 215,
           label = "Property \nCrime",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[4]) +
  # Annotate Assault
  annotate("text", x = max(date), y = 270,
           label = "Assault",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[3]) +
  # Annotate Firearms and Explosives
  annotate("text", x = max(date), y = 300,
           label = "Firearms & \nExplosives",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[2]) +
  # Annotate Other
  annotate("text", x = max(date), y = 350,
           label = "Other",
           hjust = 0, 
           angle = 0,
           size = 2.75, 
           lineheight = 0.8,
           fontface = "bold",
           family = "gill sans", 
           color = pal2[1])
mare_stream4 

# 3. Visualise by Percentage 

mare_percentage <- total_area_data %>%
  arrange(percentage) %>%   
  ggplot(aes(date, percentage, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "proportional", bw = 0.185) + 
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives",
                                              "Assault", 
                                              "Property Crime",
                                              "Narcotic Substances")) +   
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +   
  coord_cartesian(clip = "off") +   
  labs(
    title = "Maré Crime by Denunciation Percent, Time, and Type",
    x = "Date",
    y = "Denunciation Percent", 
    fill = "Type of Crime"
  ) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
mare_percentage

# 6 month intervalisation: 

mare_percentage2 <- total_area_data %>%
  arrange(percentage) %>%   
  ggplot(aes(date, percentage, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "proportional", bw = 0.185) + 
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives",
                                              "Assault", 
                                              "Property Crime",
                                              "Narcotic Substances")) +   
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +   
  coord_cartesian(clip = "off") +   
  labs(
    title = "Maré Crime by Denunciation Percent, Time, and Type",
    x = "Date",
    y = "Denunciation Percent", 
    fill = "Type of Crime"
  ) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
mare_percentage2 

# 4. Visualise Territories 

# Function to create territory data summaries
create_territory_data <- function(df, territory) {
  df %>%
    filter(territory_type == territory) %>%
    group_by(date, crime_type) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(date) %>%
    mutate(percentage = count / sum(count))
}

# Function to create count plots
create_count_plot <- function(data, title, show_takeover = FALSE, takeover_date = NULL, takeover_label = NULL) {
  p <- data %>%
    arrange(count) %>%
    ggplot(aes(date, count, fill = crime_type)) +
    geom_area(na.rm = TRUE, position = "fill") +
    geom_stream(type = "ridge", bw = 0.185) +
    scale_fill_manual(values = pal2, labels = c("Other",
                                                "Firearms and Explosives",
                                                "Assault", 
                                                "Property Crime",
                                                "Narcotic Substances")) +   
    scale_color_manual(values = pal2) + 
    scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +   
    coord_cartesian(clip = "off") +   
    labs(
      title = title,
      x = "Date",
      y = "Denunciation Count", 
      fill = "Type of Crime"
    ) +   
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text = element_text(size = 12, family = "gill sans")) +
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          legend.position = "bottom")
  
  if(show_takeover) {
    p <- p +
      geom_vline(xintercept = as.Date(takeover_date), color = "black", linewidth = 0.5) +
      annotate("text", 
               x = as.Date(takeover_date), 
               y = 120,
               label = takeover_label,
               hjust = -0.15, 
               size = 3.5,
               family = "gill sans",  
               color = "black")
  }
  
  return(p)
}

# Function to create percentage plots
create_percent_plot <- function(data, title, show_takeover = FALSE, takeover_date = NULL, takeover_label = NULL) {
  p <- data %>%
    ggplot(aes(x = date, y = percentage, fill = crime_type)) +
    geom_area(na.rm = TRUE, position = "fill") +
    geom_stream(type = "proportional", color = "white", bw = .35) +
    scale_x_date(date_breaks = "1 year", date_labels = "%b%Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    theme(axis.text.y = element_text(angle = 45)) +
    labs(
      title = title,
      x = "Date",
      y = "Percentage",
      fill = "Type of Crime") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = pal2, labels = c("Other",
                                                "Firearms and Explosives", 
                                                "Assault", 
                                                "Property Crime", 
                                                "Narcotic Substances"))+
    theme(text = element_text(size = 12, family = "gill sans")) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  
  if(show_takeover) {
    p <- p +
      geom_vline(xintercept = as.Date(takeover_date), color = "black", linewidth = 0.5) +
      annotate("text", 
               x = as.Date(takeover_date), 
               y = 1.02,
               label = takeover_label,
               hjust = -0.15, 
               size = 3.5,
               family = "gill sans",  
               color = "black")
  }
  
  return(p)
}

## 4.1 Create Territory Data ##
CVNH_data <- create_territory_data(df_labeled, "CVNH")
CVPU_data <- create_territory_data(df_labeled, "CVPU")
ADA_data <- create_territory_data(df_labeled, "ADA")
TCP_data <- create_territory_data(df_labeled, "TCP")
milicia_data <- create_territory_data(df_labeled, "milicia")

## 4.2 Create Count Plots ##
CVNH_count_plot <- create_count_plot(CVNH_data, "CVNH")
CVPU_count_plot <- create_count_plot(CVPU_data, "CVPU")
ADA_count_plot <- create_count_plot(ADA_data, "ADA/TCP", 
                                    show_takeover = TRUE, 
                                    takeover_date = "2009-09-01", 
                                    takeover_label = "TCP Takeover")
TCP_count_plot <- create_count_plot(TCP_data, "Historic TCP")
milicia_count_plot <- create_count_plot(milicia_data, "Milicia Crime by Count",
                                        show_takeover = TRUE,
                                        takeover_date = "2006-11-01",
                                        takeover_label = "Milicia Takeover")

## 4.3 Create Percentage Plots ##
CVNH_percent_plot <- create_percent_plot(CVNH_data, "CVNH")
CVPU_percent_plot <- create_percent_plot(CVPU_data, "CVPU")
ADA_percent_plot <- create_percent_plot(ADA_data, "ADA/TCP",
                                        show_takeover = TRUE,
                                        takeover_date = "2009-09-01",
                                        takeover_label = "TCP Takeover")
TCP_percent_plot <- create_percent_plot(TCP_data, "Historic TCP")
milicia_percent_plot <- create_percent_plot(milicia_data, "Milicia Crime by Percent",
                                            show_takeover = TRUE,
                                            takeover_date = "2006-11-01",
                                            takeover_label = "Milicia Takeover")

## 5. Combined Territory Visualizations ##

### 5.1 All Territory Count Visualization ###
all_terr_count_viz <- ggarrange(
  CVNH_count_plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank()),
  CVPU_count_plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()),
  ADA_count_plot + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()),
  TCP_count_plot + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()),
  legend = "bottom",
  common.legend = TRUE,
  nrow = 2, 
  ncol = 2
) %>%
  annotate_figure(left = text_grob("Count", vjust = 1, rot = 90, family = "gill sans")) %>%
  annotate_figure(top = text_grob("Gang Territories", size = 16, family = "gill sans"))

### 5.2 All Territory Percentage Visualization ###
all_terr_percent_viz <- ggarrange(
  CVNH_percent_plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank()),
  CVPU_percent_plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()),
  ADA_percent_plot + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()),
  TCP_percent_plot + 
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()),
  legend = "bottom",
  common.legend = TRUE,
  nrow = 2, 
  ncol = 2
) %>%
  annotate_figure(left = text_grob("Percentage", vjust = 1, rot = 90, family = "gill sans")) %>%
  annotate_figure(top = text_grob("Gang Territories", size = 16, family = "gill sans"))

## 6. Display Plots ##
print(all_terr_count_viz)
print(all_terr_percent_viz)

