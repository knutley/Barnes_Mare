# Title: Data Filtering and Labelling - Resident Denunciation Project 
# Author: Katelyn Nutley 
# Date: 24-07-2025

# Load required libraries

required_packages <- c("readr", "tidyr", "dplyr", "lubridate")
lapply(required_packages, library, character.only = TRUE)


################################################################################
############################### DATA FILTERING #################################
################################################################################

# Load cleaned data (assumes df_cleaned is available from cata_cleaning.R)
# df_cleaned <- read.csv("cleaned_dataset.csv")  # Uncomment if loading from file

# 1. Filter cleaned data according to date
df_filtered <- df_cleaned %>%
  filter(date <= "2014-04-01") # This is a date chosen by Dr. Barnes in line with
# his understanding of the dataset. 

################################################################################
############################# DATA PARTITIONING ################################
################################################################################

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
df_partitioned <- df_filtered %>%
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

# 4. Check territory distribution
territory_distribution <- table(df_partitioned$territory_type)
print("Territory Distribution:")
print(territory_distribution)

# 5. Check unassigned neighbourhoods:
if (nrow(unassigned_data) > 0) {
  cat("\nUnassigned neighbourhoods:\n")
  unassigned_neighbourhoods <- unique(unassigned_data$den_comunidade)
  print(unassigned_neighbourhoods)
} # Mare is too broad of a designation to belong to any one community, and Marcilio
# Dias is actually outside Mare, so everything seems in order. 

# 6. Save partitioned datasets (optional)
# write.csv(CVNH_data, "CVNH_data.csv", row.names = FALSE)
# write.csv(CVPU_data, "CVPU_data.csv", row.names = FALSE)
# write.csv(ADA_data, "ADA_data.csv", row.names = FALSE)
# write.csv(TCP_data, "TCP_data.csv", row.names = FALSE)
# write.csv(milicia_data, "milicia_data.csv", row.names = FALSE)
write.csv(df_partitioned, "data_with_territories.csv", row.names = FALSE)

################################################################################
############################### DATA LABELLING #################################
################################################################################

# 1. Define other crimes for exclusion
other_crimes <- c("CRIMES CONTRA CRIANCA E O ADOLESCENTE", "CRIMES PRATICADOS POR FUNC. PUBLICOS",
                  "CALAMIDADE PUBLICA", "DAS FALSIFICACOES E ADULTERACOES", "CRIMES CONTRA A LIBERDADE SEXUAL",
                  "PERTURBACAO DA ORDEM PUBLICA", "CRIMES CONTRA A ADM DA JUSTICA", "MAU ATENDIMENTO EM ONIBUS",
                  "CRIMES CONTRA O MEIO AMBIENTE", "CRIMES CONTRA A ADMINISTRACAO PUBLICA", 
                  "CRIMES CONTRA A SAUDE PUBLICA", "SUBSTANCIAS TOXICAS / EXPLOSIVAS", "DEFESA DO CIDADAO",
                  "CRIMES DE TRANSITO", "OUTROS", "CRIMES CONTRA A ADMINISTRAÇÃO PÚBLICA", "CRIMES DE TRÂNSITO",
                  "PERTURBAÇÃO DA ORDEM PÚBLICA", "DEFESA DO CIDADÃO", "CRIMES CONTRA A ADM DA JUSTIÇA",
                  "DAS FALSIFICAÇÕES E ADULTERAÇÕES", "CRIMES CONTRA A SAÚDE PÚBLICA", "CALAMIDADE PÚBLICA",
                  "CRIMES CONTRA CRIANÇA E O ADOLESCENTE", "MAU ATENDIMENTO EM ÔNIBUS", "SUBSTÂNCIAS TÓXICAS / EXPLOSIVAS")

# 2. Classify crime types
pattern <- paste0("\\b(", paste(other_crimes, collapse = "|"), ")\\b")
df_labeled <- df_partitioned %>%
  mutate(crime_type = case_when(
    grepl(pattern, assunto_classe, ignore.case = TRUE) ~ "other", 
    TRUE ~ assunto_classe                                       
  ))

# 3. Standardise crime type names
df_labeled <- df_labeled %>% 
  mutate(crime_type = str_trim(crime_type)) %>%  # Remove leading/trailing whitespace
  mutate(crime_type = case_when(
    crime_type == "CRIMES CONTRA O PATRIMÔNIO" ~ "CRIMES CONTRA O PATRIMONIO",
    crime_type == "SUBSTÂNCIAS ENTORPECENTES" ~ "SUBSTANCIAS ENTORPECENTES",
    TRUE ~ crime_type
  ))

# 4.Check Crime Distribution 
unique(df_labeled$crime_type) # These columns collapsed the right way here; 
crime_table <- table(df_labeled$crime_type)
print(crime_table)

# 5. Save Data 
write.csv(df_labeled, "data_with_labels.csv", row.names = FALSE)

** PI applied manual data adjustments post-processing. 





