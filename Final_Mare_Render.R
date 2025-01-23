##########################
### Final Mare Render ### 
###### 07-01-2025 #######

# So, first, Nick asked that I merge the Manguinhos data which he sent along. 
# There's a bit of an issue with it in that there are like 19 observations vs the 
# 70 something of the bigger dataset, so I have to figure out a way to best merge 
# them. (Hard to do because there's no codebook and they're named different things.)

# 1. Read in Datasets # 

library(readr)
df1 <- read.csv('/Users/katienutley/Downloads/DD_Maré_22072016.csv')
View(df1) # Bigger Mare dataset here. 

df2 <- read.csv('/Users/katienutley/Downloads/mare_from_manguinhos.csv')
View(df2) #Manguinhos dataset here. Again, ~60 less columns.

# Guess I'll rename what I can by hand? 

# 2. Merging the Datasets # 

# 2A. Cleaning Manguinhos Data for Merging # 

colnames(df2)[colnames(df2) == 'Den.Numero'] <- 'den_numero'
View(df2) # All right, this worked. So, now to repeat it. 

unique(colnames(df2))

# Right, so, there's a column for year and month in the Manguinhos data. I think 
# you'll have to turn that into some kind of of MM/YYYY date later. But, for right
# now, let's just try to rename things to make the merge easier. 

# Cla.Ds seems to be assunto_clase and Tpa.Ds is assunto_tipo

colnames(df2)[colnames(df2) == 'Cla.Ds'] <- 'assunto_classe'
colnames(df2)[colnames(df2) == 'Tpa.Ds'] <- 'assunto_tipo'
unique(colnames(df2))

# Den.Logr.Tp is den_logr_tp and Den.Logr.DS is den_logr_ds

colnames(df2)[colnames(df2) == 'Den.Logr.Tp'] <- 'den_logr_tp'
colnames(df2)[colnames(df2) == 'Den.Logr.Ds'] <- 'den_logr_ds'
unique(colnames(df2))

# Den.Logr.Num is den_logr_num and Den.Logr.Cmpl is den_logr_cmpl

colnames(df2)[colnames(df2) == 'Den.Logr.Num'] <- 'den_logr_num'
colnames(df2)[colnames(df2) == 'Den.Logr.Cmpl'] <- 'den_logr_cmpl'
unique(colnames(df2))

# Den.Logr.Barrio is den_logr_bairro and Den.Texto is den_texto

colnames(df2)[colnames(df2) == 'Den.Logr.Bairro'] <- 'den_logr_bairro'
colnames(df2)[colnames(df2) == 'Den.Texto'] <- 'den_texto'
unique(colnames(df2))

# Env.Nome is env_nome and Env.Vulgo is env_vulgo

colnames(df2)[colnames(df2) == 'Env.Nome'] <- 'env_nome'
colnames(df2)[colnames(df2) == 'Env.Vulgo'] <- 'env_vulgo'
unique(colnames(df2))

# Env.Caract is env_caract, Env.Logr.Ds is env_logr_ds, Env.Logr.Num is 
# env_logr_num, and Env.Logr.Bairro is env_logr_bairro. 

colnames(df2)[colnames(df2) == 'Env.Caract'] <- 'env_caract'
unique(colnames(df2)) # I started having some issues here, it just began deleting
# whole columns for whatever reason. So, I began breaking the renaming up. 

colnames(df2)[colnames(df2) == 'Env.Logr.Ds'] <- 'env_logr_ds'
unique(colnames(df2)) # Fixed it here, I had accidentally named the command 
# further up env_logr_ds when it was meant to be den_logr_ds. 

colnames(df2)[colnames(df2) == 'Env.Logr.Num'] <- 'env_logr_num'
colnames(df2)[colnames(df2) == 'Env.Logr.Bairro'] <- 'env_logr_bairro'
unique(colnames(df2)) # Right, so everything in order now. We have the same
# number of columns we began with and they're all commiserately named. 

# So, in addition to matching these columns by name, I also made sure to look 
# through the data in the columns to make sure that they were meant to capture
# the same variable. I may need Nick to check this and make sure that I've done 
# this right. Of the 19 columns, 3 remained unmatched to columns in the bigger
# dataset (df1). The unmatched columns are Ano.de.Den.Dt.Rec, Mes.de.Den.Dt.Rec
# and X. The first two are year and month, so I can just turn this into a date 
# column, I think. 

# Making an appropriate time column for df2. 

library(tidyr)
library(lubridate)
df2$date <- "0" # Creating a new column, called 'date'. 
# df2$date <- as.Date(paste(df2$Ano.de.Den.Dt.Rec, df2$Mês.de.Den.Dt.Rec, "01", sep="-")) 
# Hmm, this didn't work. Maybe the date format is incorrect? 
# df2$date <- as.Date(paste(
  # as.numeric(df2$Ano.de.Den.Dt.Rec),
  # as.numeric(df2$Mês.de.Den.Dt.Rec),"01", sep="-"
# )) # This still didn't work. Going to try to troubleshoot things. 

# Checking data types
str(df2$Ano.de.Den.Dt.Rec) # This is fine. 
str(df2$Mês.de.Den.Dt.Rec) # Ah, here we go. I need to translate these for R. 

# Portuguese to numbers
month_map <- c("janeiro" = 1, "fevereiro" = 2, "março" = 3, "abril" = 4, 
               "maio" = 5, "junho" = 6, "julho" = 7, "agosto" = 8,
               "setembro" = 9, "outubro" = 10, "novembro" = 11, "dezembro" = 12)

#Trying again... 
df2$date <- as.Date(paste(
  df2$Ano.de.Den.Dt.Rec,
  month_map[df2$Mês.de.Den.Dt.Rec],
  "01", sep="-"
)) # This seemed to work; it didn't throw up any error messages at least. 

str(df2$date) # Great, it recognises it as a date. 
typeof(df2$date) # Confirmed with this because it says that it's a 'double' variable. 

# 2B. Cleaning Mare Data for Merging # 

# Right, so because I already have a date column in df2, I need to also make one 
# in df1. And then I'll merge them. 

library(lubridate)
library(dplyr)
date <- "0" # Creating a new date column for df1. 
date <- as.Date(df1$den_dt_rec, format = "%m/%d/%y")
print(first(date)) #This removed the time (%H: %M)
dates_without_day <- format(as.Date(date), "%Y-%m")
print(first(dates_without_day)) #This removed the specific day. 
typeof(dates_without_day) #BUTTTT, it's back to a character. Very fun.
string_date <- format(dates_without_day)
complete_date_string <- paste0(string_date, "-01") #Make it a complete date. 
print(first(complete_date_string))
date_variable <- as.Date(complete_date_string, format = "%Y-%m-%d")
print(first(date_variable))
typeof(date_variable) #Says double, so it's worked. 
df1$date <- date_variable
View(df1) # Perfection. 

# 2C. Merge Datasets # 

library(dplyr)
unique(colnames(df1))
unique(colnames(df2))
df <- merge(df1, df2, by = "den_numero", all.y = TRUE) # Right, so this worked 
# but it all.x and all.y = TRUE threw up error messages for some reason. 

# df <- rbind(df1, df2) # Thought I would start with Base R just to see and because 
# they have different numbers of columns it won't work, which is what I thought. 

unique(colnames(df2))
common_cols <- intersect(names(df1), names(df2))
print(common_cols) # Just doing this to make sure that they actually match! 

# Do the join using all common columns
# merged_df <- df1 %>%
  # left_join(df2, by = common_cols) # There's an issue here because x$env_logr_num
# is a <character> and y$env_logr_num is an <integer> 

df1$env_logr_num <- as.numeric(df1$env_logr_num)
merged_df <- df1 %>%
  left_join(df2, by = common_cols)
View(merged_df) # There is an issue here because while it does now have the 
# correct number of variables (82); it doesn't have the correct number of 
# observations. So, df1 had 27,814 obs of 79 variables and df2 had 1017 of 20 
# variables. It should then be a merged df with 28,831 obs and 79 variables. 

merged_df <- df1 %>%
  full_join(df2, by = common_cols)
dim(merged_df) # This has worked. Thank Jesu. 

# 3. Grouping Data According to Crime # 

unique(merged_df$assunto_classe)
# Spoke to Nick about what he wanted to include and it was: 
# 1. CRIMES CONTRA A PESSOA - Assault 
# 2. SUBSTANCIAS ENTORPECENTES - Narcotic Substances 
# 3. CRIMES CONTRA O PATRIMONIO - Property Crimes 
# 4. ARMAS DE FOGO E ARTEFATOS EXPLOSIVOS - Firearms and Explosives 
# Everything else is just "other". 

other_crimes <- c("CRIMES CONTRA CRIANCA E O ADOLESCENTE", "CRIMES PRATICADOS POR FUNC. PUBLICOS",
                  "CALAMIDADE PUBLICA", "DAS FALSIFICACOES E ADULTERACOES", 
                  "CRIMES CONTRA A LIBERDADE SEXUAL", "PERTURBACAO DA ORDEM PUBLICA",
                  "CRIMES CONTRA A ADM DA JUSTICA", "MAU ATENDIMENTO EM ONIBUS", 
                  "CRIMES CONTRA O MEIO AMBIENTE", "CRIMES CONTRA A ADMINISTRACAO PUBLICA",
                  "CRIMES CONTRA A SAUDE PUBLICA", "SUBSTANCIAS TOXICAS / EXPLOSIVAS",
                  "DEFESA DO CIDADAO", "RIMES DE TRANSITO", "OUTROS")

# This means the inclusion values are: "SUBSTANCIAS ENTORPECENTES", "CRIMES CONTRA O PATRIMONIO",
# "ARMAS DE FOGO E ARTEFATOS EXPLOSIVOS", and "CRIMES CONTRA A PESSOA".

merged_df$crime_type <- "0"
View(merged_df) # Right, so this isn't the issue. 

merged_df <- merged_df %>%
  mutate(crime_type = case_when(
    grepl(paste(other_crimes, collapse = "|"), assunto_classe, ignore.case = TRUE) ~ "other",
    TRUE ~ assunto_classe
  ))
unique(merged_df$crime_type) # There's an issue here. It's not acknowledging 
# the case_when command. So, issue with the Boolean argument? 


pattern <- paste0("\\b(", paste(other_crimes, collapse = "|"), ")\\b")
merged_df <- merged_df %>%
  mutate(crime_type = case_when(
    grepl(pattern, assunto_classe, ignore.case = TRUE) ~ "other", 
    TRUE ~ assunto_classe                                       
  ))
unique(merged_df$crime_type) # This didn't fix it. 

View(merged_df)

# Going to try to shift around the inclusion values. 

valid_categories <- c("SUBSTANCIAS ENTORPECENTES", "CRIMES CONTRA O PATRIMONIO", 
                      "ARMAS DE FOGO E ARTEFATOS EXPLOSIVOS", "CRIMES CONTRA A PESSOA")

merged_df <- merged_df %>%
  mutate(crime_type = case_when(
    assunto_classe %in% valid_categories ~ assunto_classe,  # Keep valid categories as they are
    TRUE ~ "other"                                         # Assign "other" to everything else
  ))
unique(merged_df$crime_type) # This hasn't worked because it's only got three 
# unique values; seems to have also assigned Substancias Entorpecentes and Crimes
# Contra O Patrimonio as "other", too. Maybe it's a whitespace issue. 

merged_df <- merged_df %>%
  mutate(assunto_classe = trimws(toupper(assunto_classe)))
valid_categories <- c("SUBSTANCIAS ENTORPECENTES", "CRIMES CONTRA O PATRIMONIO", 
                      "ARMAS DE FOGO E ARTEFATOS EXPLOSIVOS", "CRIMES CONTRA A PESSOA")
merged_df <- merged_df %>%
  mutate(crime_type = case_when(
    assunto_classe %in% valid_categories ~ assunto_classe,  
    TRUE ~ "other"                                        
  ))
unique(merged_df$crime_type) #It was a fucking whitespace issue. Christ. 
View(merged_df) # Yeah, this is perfect. No issues. 

##################################################################################
# Now that I have the right data, I'm going to basically import 
# Revisualisation_Mare_Gangs1.R. That's the last and sort of finalised visualisation
# that Nick approved. It has all the aesthetic changes he wanted.
#################################################################################

# 3. Visualising Data # 

## 3.1 Read in Packages and Define Colour Palette ## 

library(ggplot2)
library(ggstream)
library(extrafont)
library(dplyr)

pal2 <- c("#003f5c",
          "#d45087",
          "#ffa600",
          "#665191",
          "#ff7c43") # I played around with the colour combinations and this 
# seems to give the most contrast in between crime categories. Nick asked for higher
# contrast between the streams. 

### 3.1.1 Calculate Count and Percentage ### 

total_area_data <- merged_df %>%
  group_by(date, crime_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(date) %>%
  mutate(percentage = count / sum(count))
View(total_area_data) # This looks good. If you're curious as to how this breaks
# down, please look at this table. 

## 3.2 Visualise by Count ## 

mare_stream <-  total_area_data %>%
  arrange(count) %>%   
  ggplot(aes(date, count, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "ridge", bw = 0.4) +   
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives",
                                              "Assault", 
                                              "Property Crime",
                                              "Narcotic Substances")) +   #adding English legend
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") + #changed from 6 mos to 1 year
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +   
  coord_cartesian(clip = "off") +   
  labs(
    title = "Maré Crime by Denunciation Count, Time, and Type",
    x = "Date",
    y = "Denunciation Count", #changed to denunciation count
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) 
mare_stream # This has the colour contrasts that Nick asked for, but doesn't have 
# the one month intervalisation. It also has issues with the background and font. 
# Nick also mentioned that he was a bit worried there was too much smoothing in 
# this initial graph, so I'm going to fix that below. 

mare_stream1 <- total_area_data %>%
  arrange(count) %>%   
  ggplot(aes(date, count, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "ridge", bw = 0.185) +  #altered the bandwith parameter here 
  # to show peaks. It needs to be >/= 0.2 to make the peaks more visible. 
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
  theme(plot.title = element_text(hjust = 0.5)) 

mare_stream1 #Okay, so I messed with the bandwith parameters here to make it 
# top out over 400. I thought about perhaps changing the x axis to min-max
# because I think the numbers should be what they are. Happy with that, now to 
# do the more aesthetic stuff. 

library(extrafont)

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
        legend.position = "bottom")

mare_stream2

mare_stream3 <- total_area_data %>%
  arrange(count) %>%   
  ggplot(aes(date, count, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "ridge", bw = 0.185) + 
  scale_fill_manual(values = pal2) +   
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
        legend.position = "none") # Just removing the legend because I don't 
# need it if I have floating descriptors. 

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
mare_stream4 # Great, so this looks lovely but Nick said he wanted to see what 
# it looked like with different intervalisations, too. 

# 1 month intervalisation: 

mare_stream5 <- total_area_data %>%
  arrange(count) %>%   
  ggplot(aes(date, count, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "ridge", bw = 0.185) + 
  scale_fill_manual(values = pal2) +   
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
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

mare_stream6 <- mare_stream5 +
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
mare_stream6 # The intervalisation that Nick asked for. 

# Quarterly intervalisation:

mare_stream7 <- total_area_data %>%
  arrange(count) %>%   
  ggplot(aes(date, count, fill = crime_type)) +   
  geom_area(na.rm = TRUE, position = "fill") +   
  geom_stream(type = "ridge", bw = 0.185) + 
  scale_fill_manual(values = pal2) +   
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
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

mare_stream8 <- mare_stream7 +
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
mare_stream8 # I think this looks better, but it's still a bit cramped. 

# 6 month intervalisation: 

mare_stream9 <- total_area_data %>%
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

mare_stream10 <- mare_stream9 +
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
mare_stream10 # This looks slight better as it's less cramped, but need to 
# discuss with Nick. 

## 3.3 Visualise by Percentage ## 

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

# 1 month intervalisation:

mare_percentage1 <- total_area_data %>%
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
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
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
mare_percentage1

# Quarterly intervalisation: 

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
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
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

# 6 month intervalisation: 

mare_percentage3 <- total_area_data %>%
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
mare_percentage3 # This or the yearly intervalisation is probably best, but 
# again, need to discuss with Nick. 

# 4. Visualise Territories # 

## 4.1 CVNH Territories ## 

### 4.1.1 Group by CVNH Territory ### 

CVNH_neighbourhoods <- c("NOVA HOLANDA", "PARQUE MARE", "PARQUE RUBENS VAZ")
CVNH_grouped_data <-  merged_df %>%
  filter(den_comunidade %in% CVNH_neighbourhoods)
View(CVNH_grouped_data)

### 4.1.2 Convert to CVNH Percentages ### 

CVNH_subsetted_data <- CVNH_grouped_data %>%
  group_by(date, crime_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(date) %>%
  mutate(percentage = count / sum(count)) 
View(CVNH_subsetted_data)

### 4.1.3 Visualise CVNH Territory ### 

#### 4.1.3.1 Visualise CVNH Territory by Count #### 

CVNH_count_plot <- CVNH_subsetted_data %>%
  arrange(count) %>%
  ggplot(aes(date,count, fill = crime_type)) +
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
    title = "CVNH",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
CVNH_count_plot 

# 1 month intervalisation: 
 
CVNH_count_plot1 <- CVNH_subsetted_data %>%
  arrange(count) %>%
  ggplot(aes(date,count, fill = crime_type)) +
  geom_area(na.rm = TRUE, position = "fill") +
  geom_stream(type = "ridge", bw = 0.185) +
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives",
                                              "Assault", 
                                              "Property Crime",
                                              "Narcotic Substances")) +   
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +   
  coord_cartesian(clip = "off") +   
  labs(
    title = "CVNH",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
CVNH_count_plot1 

# Quarterly intervalisation: 

CVNH_count_plot2 <- CVNH_subsetted_data %>%
  arrange(count) %>%
  ggplot(aes(date,count, fill = crime_type)) +
  geom_area(na.rm = TRUE, position = "fill") +
  geom_stream(type = "ridge", bw = 0.185) +
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives",
                                              "Assault", 
                                              "Property Crime",
                                              "Narcotic Substances")) +   
  scale_color_manual(values = pal2) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +   
  coord_cartesian(clip = "off") +   
  labs(
    title = "CVNH",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
CVNH_count_plot2

# 6 month intervalisation: 

CVNH_count_plot3 <- CVNH_subsetted_data %>%
  arrange(count) %>%
  ggplot(aes(date,count, fill = crime_type)) +
  geom_area(na.rm = TRUE, position = "fill") +
  geom_stream(type = "ridge", bw = 0.185) +
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
    title = "CVNH",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
CVNH_count_plot3 # Right, so, these are different intervalisation options for the
# smaller graphs, but because they're smaller I'd recommend doing it on a yearly 
# basis. Need to discuss this with Nick. Just have to change date_breaks command.

#### 4.1.3.2 Visualise CVNH Territory by Percentage #### 

CVNH_percent_plot <- CVNH_subsetted_data %>%
  ggplot(aes(x = date, y = percentage, fill = crime_type)) +
  geom_area(na.rm = TRUE, position = "fill") +
  geom_stream(type = "proportional", color = "white", bw = .35) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(axis.text.y = element_text(angle = 45)) +
  labs(
    title = "CVNH",
    x = "Date",
    y = "Percentage",
    fill = "Type of Crime") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives", 
                                              "Assault", 
                                              "Property Crime", 
                                              "Narcotic Substances"))+
  theme(text=element_text(size=12,  family="gill sans")) + # We can change the font, too. 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
CVNH_percent_plot

## 4.2 CVPU Territories ## 

### 4.2.1 Group by CVPU Territory ###

CVPU_neighbourhood <- "PARQUE UNIAO"
CVPU_grouped_data <- merged_df %>%
  filter(den_comunidade %in% CVPU_neighbourhood)
View(CVPU_grouped_data)

### 4.2.2 Convert to CVPU Percentages ### 

CVPU_subsetted_data <- CVPU_grouped_data %>%
  group_by(date, crime_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(date) %>%
  mutate(percentage = count / sum(count)) 
View(CVPU_subsetted_data)

### 4.2.3 Visualise CVPU Territory ### 

#### 4.2.3.1 Visualise CVPU Territory by Count #### 

CVPU_count_plot <- CVPU_subsetted_data %>%
  arrange(count) %>%
  ggplot(aes(date,count, fill = crime_type)) +
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
    title = "CVPU",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
CVPU_count_plot 

#### 4.2.3.2 Visualise CVPU Territory by Percentage #### 

CVPU_percent_plot <- CVPU_subsetted_data %>%
  ggplot(aes(x = date, y = percentage, fill = crime_type)) +
  geom_area(na.rm = TRUE, position = "fill") +
  geom_stream(type = "proportional", color = "white", bw = .35) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(axis.text.y = element_text(angle = 45)) +
  labs(
    title = "CVPU",
    x = "Date",
    y = "Percentage",
    fill = "Type of Crime") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives", 
                                              "Assault", 
                                              "Property Crime", 
                                              "Narcotic Substances"))+
  theme(text=element_text(size=12,  family="gill sans")) + # We can change the font, too. 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
CVPU_percent_plot

## 4.3 ADA Territories ##

### 4.3.1 Group by ADA Territories ### 

ADA_neighbourhoods <- c("VILA DO JOAO", "CONJUNTO ESPERANCA", "CONJUNTO PINHEIRO",
                        "VILA DO PINHEIROS", "VILA DO PINHEIROS(PE)", "SALSA MERENGUE")
ADA_grouped_data <- merged_df %>%
  filter(den_comunidade %in% ADA_neighbourhoods)
View(ADA_grouped_data)

### 4.3.2 Convert to ADA Percentages ###

ADA_subsetted_data <- ADA_grouped_data %>%
  group_by(date, crime_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(date) %>%
  mutate(percentage = count / sum(count)) 
View(ADA_subsetted_data)

### 4.3.3 Visualise ADA Territories ### 

#### 4.3.3.1 Visualise by Count #### 

ADA_count_plot <- ADA_subsetted_data %>%
  arrange(count) %>%
  ggplot(aes(date,count, fill = crime_type)) +
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
    title = "ADA/TCP",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
ADA_count_plot # Need to add the ADA/TCP drama, though.  

ADA_count_plot1 <- ADA_count_plot +
  geom_vline(xintercept = as.Date("2009-09-01"), color = "black", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2009-09-01"), 
           y = 120,
           label = "TCP Takeover",
           hjust = -0.15, 
           size = 3.5,
           family = "gill sans",  
           color = "black")

ADA_count_plot1 

#### 4.3.3.2 Visualise by Percentage #### 

ADA_percent_plot <- ADA_subsetted_data %>%
  ggplot(aes(x = date, y = percentage, fill = crime_type)) +
  geom_area(na.rm = TRUE, position = "fill") +
  geom_stream(type = "proportional", color = "white", bw = .35) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(axis.text.y = element_text(angle = 45)) +
  labs(
    title = "ADA/TCP",
    x = "Date",
    y = "Percentage",
    fill = "Type of Crime") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives", 
                                              "Assault", 
                                              "Property Crime", 
                                              "Narcotic Substances"))+
  theme(text=element_text(size=12,  family="gill sans")) + # We can change the font, too. 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
ADA_percent_plot

# Need to insert ADA/TCP drama though. 

ADA_percent_plot1 <- ADA_percent_plot +
  geom_vline(xintercept = as.Date("2009-09-01"), color = "black", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2009-09-01"), 
           y = 1.02,
           label = "TCP Takeover",
           hjust = -0.15, 
           size = 3.5,
           family = "gill sans",  
           color = "black")

ADA_percent_plot1

## 4.4 TCP Territories ## 

### 4.4.1 Group by TCP Territories ### 

TCP_neighbourhoods <- c("NOVA MARE", "BAIXA DO SAPATEIRO", "MORRO DO TIMBAU",
                        "BENTO RIBEIRO DANTAS")
TCP_grouped_data <- merged_df %>%
  filter(den_comunidade %in% TCP_neighbourhoods)
View(TCP_grouped_data)

### 4.4.2 Convert to TCP Territories ###

TCP_subsetted_data <- TCP_grouped_data %>%
  group_by(date, crime_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(date) %>%
  mutate(percentage = count / sum(count)) 
View(TCP_subsetted_data)

### 4.4.3 Visualise TCP Territories ### 

#### 4.4.3.1 Visualise by Count #### 

TCP_count_plot <- TCP_subsetted_data %>%
  arrange(count) %>%
  ggplot(aes(date,count, fill = crime_type)) +
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
    title = "Historic TCP",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
TCP_count_plot

#### 4.4.3.2 Visualise by Percentage #### 

TCP_percent_plot <- TCP_subsetted_data %>%
  ggplot(aes(x = date, y = percentage, fill = crime_type)) +
  geom_area(na.rm = TRUE, position = "fill") +
  geom_stream(type = "proportional", color = "white", bw = .35) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(axis.text.y = element_text(angle = 45)) +
  labs(
    title = "Historic TCP",
    x = "Date",
    y = "Percentage",
    fill = "Type of Crime") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives", 
                                              "Assault", 
                                              "Property Crime", 
                                              "Narcotic Substances"))+
  theme(text=element_text(size=12,  family="gill sans")) + # We can change the font, too. 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

TCP_percent_plot

# *** Important to note here that I've now changed the ADA plot to something 
# called the ADA/TCP graph, kept the geom line, and then changed the TCP plot t
# a plot called 'TCP Historic Terriroties' as Nick asked. So, that should be us 
# squared away. 

## 4.5 Milicia Territories ##

# Worth noting here is that I am aware Nick will likely exclude these from his 
# original paper/ may drag these over to a new paper, but I thought I should 
# re-visualise them because I merged the Manguinhos data and the previous renderings
# might now be outdated. 

### 4.5.1 Group by Milicia Territories ### 

milicia_neighbourhoods <- c("ROQUETE PINTO", "PRAIA DE RAMOS")
milicia_grouped_data <- merged_df %>%
  filter(den_comunidade %in% milicia_neighbourhoods)
View(milicia_grouped_data)

### 4.5.2 Convert to Milicia Percentages ### 

milicia_subsetted_data <- milicia_grouped_data %>%
  group_by(date, crime_type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(date) %>%
  mutate(percentage = count / sum(count)) 
View(milicia_subsetted_data)

### 4.5.3 Visualise Milicia Territory ### 

#### 4.5.3.1 Visualise by Count #### 

milicia_count_plot <- milicia_subsetted_data %>%
  arrange(count) %>%
  ggplot(aes(date,count, fill = crime_type)) +
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
    title = "Milicia Crime by Count",
    x = "Date",
    y = "Denunciation Count", 
    fill = "Type of Crime"
  ) +   
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 12, family = "gill sans")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.position = "bottom")
milicia_count_plot # Need to add the takeover line? 

milicia_count_plot1 <- milicia_count_plot +
  geom_vline(xintercept = as.Date("2006-11-01"), color = "black", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2006-11-01"), 
           y = 60,
           label = "Milicia Takeover",
           hjust = -0.15, 
           size = 3.5,
           family = "gill sans",  
           color = "black")
milicia_count_plot1

#### 4.5.3.2 Visualise by Percentage #### 

milicia_percent_plot <- milicia_subsetted_data %>%
  ggplot(aes(x = date, y = percentage, fill = crime_type)) +
  geom_area(na.rm = TRUE, position = "fill") +
  geom_stream(type = "proportional", color = "white", bw = .35) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(axis.text.y = element_text(angle = 45)) +
  labs(
    title = "Milicia Crime by Percent",
    x = "Date",
    y = "Percentage",
    fill = "Type of Crime") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = pal2, labels = c("Other",
                                              "Firearms and Explosives", 
                                              "Assault", 
                                              "Property Crime", 
                                              "Narcotic Substances"))+
  theme(text=element_text(size=12,  family="gill sans")) + # We can change the font, too. 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

milicia_percent_plot

milicia_percent_plot1 <- milicia_percent_plot +
  geom_vline(xintercept = as.Date("2006-11-01"), color = "black", linewidth = 0.5) +
  annotate("text", 
           x = as.Date("2006-11-01"), 
           y = 1.02,
           label = "Milicia Takeover",
           hjust = -0.15, 
           size = 3.5,
           family = "gill sans",  
           color = "black")
milicia_percent_plot1

# 5. Stitching Together Territory Visualisations # 

# Again, we're going to exclude milicia, but I thought it important to at 
# least visualise it first. 

library(ggpubr)

## 5.1 All Territory Count Visualisation ## 

all_terr_count_viz <- ggarrange(CVNH_count_plot +
                                  theme(axis.title.x  = element_blank(),
                                        axis.text.x = element_blank()),
                                CVPU_count_plot +
                                  theme(axis.title.x = element_blank(),
                                        axis.text.x = element_blank(),
                                        axis.title.y = element_blank(),
                                        axis.text.y = element_blank()),
                                ADA_count_plot1,
                          TCP_count_plot +
                            theme(axis.title.y  = element_blank(),
                                  axis.text.y = element_blank()),
                          legend = "bottom",
                          common.legend = TRUE)
all_terr_count_viz # Superfluous axis text Y; also needs a supratitle. 

all_terr_count_viz1 <- ggarrange(
  CVNH_count_plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank()),
  CVPU_count_plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()),
  # Bottom plots - keep x-axis labels but hide title 
  ADA_count_plot1 + 
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
all_terr_count_viz1 


## 5.2 All Territory Percentage Visualisation ## 

all_terr_percent_viz <- ggarrange(CVNH_percent_plot +
                            theme(axis.title.x  = element_blank(),
                                  axis.text.x = element_blank()),
                        CVPU_percent_plot + 
                            theme(axis.title.x = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  axis.text.y = element_blank()),
                          ADA_percent_plot1, 
                          TCP_percent_plot +
                            theme(axis.title.y  = element_blank(),
                                  axis.text.y = element_blank()),
                          legend = "bottom",
                          common.legend = TRUE)
all_terr_percent_viz # Again, this looks great but the one issue is that there 
# should be a common x-axis. 

all_terr_percent_viz1 <- ggarrange(
  # Top plots - hide x-axis labels and title
  CVNH_percent_plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank()),
  CVPU_percent_plot + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()),
  # Bottom plots - keep x-axis labels but hide title 
  ADA_percent_plot1 + 
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
all_terr_percent_viz1 

# 6. Run-Down of Visualisations # 

## 6.1 Agg. Mare by Count ## 

mare_stream10 # Have the option here to visualise by 1 month, 3 months, or 1 year, too.
# But I took the half yearly just because I think it has more granular detail without 
# looking too cramped. 

## 6.2 Agg. Mare by Percent ##

mare_percentage3 # Same intervalisation choices as above. Need Nick to decide. 

## 6.3 Disagg. Mare by Count ##

all_terr_count_viz1 # Yearly is the only way forward here because you're visualising
# the plots side-by-side and otherwise it begins to look untidy. 

## 6.4 Disagg. Mare by Percent ##

all_terr_percent_viz1 # See comment above. 

## 6.5 Bonus: Milicia ##

### 6.5.1 Milicia by Count ### 

milicia_count_plot1

### 6.5.2 Milicia by Percent ### 

milicia_percent_plot1

# 7. Convert to HTML # 

# I'm having an issue turning this into an HTML through Rmarkdown. It just keeps spinning. 






  
  
