# Title: Data Preprocessing - Resident Denunciation Project
# Author: Katelyn Nutley
# Date: 18-07-2025

# There were two separate datasets required for analysis; the first, called Mare or
# "df1" here, is the larger, more complete of the two. The second, Manguinhos or "df2", 
# was obtained at a later date and is more, or less, an addendum to the first. 

# Load required libraries

required_packages <- c("readr", "tidyr", "dplyr", "lubridate")
lapply(required_packages, library, character.only = TRUE)


################################################################################
######################## DATA CLEANING AND MERGING ############################
################################################################################

# 1. Read in Datasets
df1 <- read.csv('/Users/katienutley/Downloads/DD_Maré_22072016.csv')
df2 <- read.csv('/Users/katienutley/Downloads/mare_from_manguinhos.csv')

# 2. Standardise Column Names in Manguinhos Dataset (df2)
column_mapping <- c(
  "Den.Numero" = 'den_numero',
  "Cla.Ds" = 'assunto_classe',
  'Tpa.Ds' = 'assunto_tipo',
  'Den.Logr.Tp' = 'den_logr_tp',
  'Den.Logr.Ds' = 'den_logr_ds',
  'Den.Logr.Num' = 'den_logr_num',
  'Den.Logr.Cmpl' = 'den_logr_cmpl',
  'Den.Logr.Bairro' = 'den_logr_bairro',
  'Den.Texto' = 'den_texto',
  'Env.Nome' = 'env_nome',
  'Env.Vulgo' = 'env_vulgo',
  'Env.Caract' = 'env_caract',
  'Env.Logr.Ds' = 'env_logr_ds',
  'Env.Logr.Num' = 'env_logr_num',
  'Env.Logr.Bairro' = 'env_logr_bairro'
)

# Apply column name mapping
for (old_name in names(column_mapping)) {
  if (old_name %in% colnames(df2)) {
    colnames(df2)[colnames(df2) == old_name] <- column_mapping[old_name]
  }
}

# 3. Create Date Variables

## 3.1. Create date for Manguinhos data (df2)
month_map <- c(
  "janeiro" = 1, "fevereiro" = 2, "março" = 3, "abril" = 4,
  "maio" = 5, "junho" = 6, "julho" = 7, "agosto" = 8,
  "setembro" = 9, "outubro" = 10, "novembro" = 11, "dezembro" = 12
)

df2$date <- as.Date(paste(
  df2$Ano.de.Den.Dt.Rec,
  month_map[df2$Mês.de.Den.Dt.Rec],
  "01", 
  sep = "-"
))

## 3.2. Create date for Mare data (df1)
df1$date <- as.Date(df1$den_dt_rec, format = "%m/%d/%y")
df1$date <- as.Date(paste0(format(df1$date, "%Y-%m"), "-01"))

# 4. Harmonise Data Types
df1$env_logr_num <- as.numeric(df1$env_logr_num)

# 5. Merge Datasets
common_cols <- intersect(names(df1), names(df2))
df_merged <- df1 %>%
  full_join(df2, by = common_cols)

# 6. Remove Duplicates
# Remove duplicates based on denunciation number, crime class, crime type, and suspect code
df_cleaned <- df_merged %>%
  distinct(den_numero, assunto_classe, assunto_tipo, env_cd, .keep_all = TRUE)

# 7. Descriptive Statistics
cat("Original Mare dataset rows:", nrow(df1), "\n")
cat("Original Manguinhos dataset rows:", nrow(df2), "\n")
cat("Merged dataset rows:", nrow(df_merged), "\n")
cat("Cleaned dataset rows (after deduplication):", nrow(df_cleaned), "\n")
cat("Duplicates removed:", nrow(df_merged) - nrow(df_cleaned), "\n")
