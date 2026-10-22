pacman::p_load(
  janitor,    
  lubridate,  
  tidyverse
  )

#Non-binomial nomenclature filter
pattern <-"^[A-Z][a-z]+\\s[A-Z]?[a-z]+$"

# Load inventoried data sets
gbif <- read_csv("inventory_data/gbif_.csv") |>
  select(species) |>
  drop_na() |>
  unique() |>
  rename("scienticname" = "species")

obis <- read_csv("inventory_data/obis.csv") |>
  select(scientificname) |>
  unique()

fishbase <- read_csv("inventory_data/fishbase.csv") |>
  janitor::clean_names() |>
  select(species) |>
  unique() |>
  rename("scientificname" = "species")

worm <- read_csv("inventory_data/WoRMS.csv") |>
  janitor::clean_names() |> 
  select(scientific_name) |>
  unique() |>
  rename("scientificname" = "scientific_name")

lit <- read_csv("inventory_data/litsurv.csv") |>
  janitor::clean_names() |> 
  select(taxon_name) |>
  drop_na() |>
  unique() |>
  rgbif::name_backbone_checklist()

lite <- lit |>
  select(species) |>
  drop_na() |> 
  rename("scientificname" = "species")

#List of data to merge
inventory <- list(
  gbif, obis, fishbase,
  worm, lite
)

#Standardize column names for all data sets
inventory <- lapply(inventory, function(df) {
  colnames(df) <- "scientificname"
  return(df)
})

#Merge all the data sets
df_inventory <- do.call(rbind, inventory) |>
  unique()

#Filter and remove non-binomial nomenclature names
df_inventory <- df_inventory |>
  filter(grepl(pattern, scientificname))

df_match <- df_eDNA |>
  select(scientific_name)

#Add a new column in A to store match results
df_match$match_results <- ifelse(df_eDNA$scientific_name
                                 %in% df_inventory$scientificname,
                                 "Expected", "Unexpected")
