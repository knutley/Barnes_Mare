# Title: Data Partitioning by Neighbourhood - Resident Denunciation Project
# Author: Katelyn Nutley
# Date: 24-07-2025

# Load required libraries
required_packages <- c("readr", "tidyr", "dplyr", "lubridate")
lapply(required_packages, library, character.only = TRUE)

################################################################################
###################### DATA PARTITIONING BY NEIGHBOURHOOD ####################
################################################################################

# Load cleaned data (assumes df_cleaned is available)
# df_cleaned <- read.csv("cleaned_dataset.csv")  # Uncomment if loading from file

# 1. Define neighbourhood territories
CVNH_neighbourhoods <- c("NOVA HOLANDA", "PARQUE MARE", "PARQUE RUBENS VAZ")
CVPU_neighbourhoods <- "PARQUE UNIAO"
ADA_neighbourhoods <- c("VILA DO JOAO", "CONJUNTO ESPERANCA", "CONJUNTO PINHEIRO",
                        "VILA DO PINHEIROS", "VILA DO PINHEIROS(PE)", "SALSA MERENGUE",
                        "SALSA E MERENGUE")
TCP_neighbourhoods <- c("NOVA MARE", "BAIXA DO SAPATEIRO", "MORRO DO TIMBAU",
                        "BENTO RIBEIRO DANTAS")
milicia_neighbourhoods <- c("ROQUETE PINTO", "PRAIA DE RAMOS")

# 2. Create territory classification column
df_partitioned <- df_cleaned %>%
  mutate(
    territory_type = case_when(
      den_comunidade %in% milicia_neighbourhoods ~ "milicia",
      den_comunidade %in% ADA_neighbourhoods ~ "ADA",
      den_comunidade %in% CVNH_neighbourhoods ~ "CVNH",
      den_comunidade %in% CVPU_neighbourhoods ~ "CVPU",
      den_comunidade %in% TCP_neighbourhoods ~ "TCP",
      TRUE ~ "unassigned"
    )
  )


# 3. Create separate datasets for each territory
# This is optional and only necessary if you'd like to work with the territories
# on a disaggregated level. 

# CVNH_data <- df_partitioned %>%
#  filter(territory_type == "CVNH")

# CVPU_data <- df_partitioned %>%
#  filter(territory_type == "CVPU")

# ADA_data <- df_partitioned %>%
#  filter(territory_type == "ADA")

# TCP_data <- df_partitioned %>%
#  filter(territory_type == "TCP")

# milicia_data <- df_partitioned %>%
#  filter(territory_type == "milicia")

# unassigned_data <- df_partitioned %>%
#  filter(territory_type == "unassigned")

# Display territory distribution
territory_distribution <- table(df_partitioned$territory_type)
print("Territory Distribution:")
print(territory_distribution)

# Display unassigned neighbourhoods for review
if (nrow(unassigned_data) > 0) {
  cat("\nUnassigned neighbourhoods:\n")
  unassigned_neighbourhoods <- unique(unassigned_data$den_comunidade)
  print(unassigned_neighbourhoods)
}

# 5. Save partitioned datasets (optional)
# write.csv(CVNH_data, "CVNH_data.csv", row.names = FALSE)
# write.csv(CVPU_data, "CVPU_data.csv", row.names = FALSE)
# write.csv(ADA_data, "ADA_data.csv", row.names = FALSE)
# write.csv(TCP_data, "TCP_data.csv", row.names = FALSE)
# write.csv(milicia_data, "milicia_data.csv", row.names = FALSE)
# write.csv(df_partitioned, "data_with_territories.csv", row.names = FALSE)