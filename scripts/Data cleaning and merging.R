pacman::p_load(
  janitor,    
  lubridate,  
  matchmaker, 
  epikit,     
  tidyverse,  
  skimr,      
  tidyverse,  
  worrms,     
  rgbif,      
  phyloseq,   
  file2meco,  
  microeco,   
  httr, 
  jsonlite
)

# Load and process register data sets-------------------------------------------
pattern <-"^[A-Z][a-z]+\\s[A-Z]?[a-z]+$"
path <- "inventory_data/" 

list <- list.files(path = path, 
                   pattern = "\\.csv$",
                   full.names = TRUE)

processed_data <- list() 

for (file in list) {
  data <- read.csv(file)
  processed <- data %>%
    filter(!is.na(Species)) %>%
    select(Species) |> 
    distinct(Species, .keep_all = TRUE)
  processed_data[[file]] <- processed
}

df_register <- bind_rows(
  processed_data
) |>
  distinct(Species) |>
  filter(grepl(pattern,
               Species))


# eDNA taxonomic data
df_meta <- read_csv("data/eDNA_kraken2.tsv") |>
  select(-taxRank, -Max, -lineage)

df_shot <- read_csv("data/eDNA_shotgun.csv") |>
  select(-taxRank, -Max, -lineage)

df_shot$Species <- gsub("\\[|\\]", "",
                          df_shot$Species)
  
df_mifis <- read_csv("data/mifish.csv")

# Merge data sets
df_barcod <- merge(df_meta, df_mifis,
                   by = c("Species", "taxID"),
                   all = TRUE)

df_merged <- merge(df_shot, df_barcod,
                  by = c("Species", "taxID"),
                  all = TRUE)

df_eDNA <- df_merged |>
  mutate(otu = paste0("otu_", taxID)) |>
  filter(grepl(pattern, Species)) |>
  select(-taxID) |>
  mutate(across(S26:S40, ~
                  ifelse(is.na(.), 0, .))) |>
  distinct(Species, .keep_all = TRUE)
