# Load the necessary packages
pacman::p_load(worrms,rgbif,
               purrr, dplyr)

df_egbif <- name_backbone_checklist(
  df_eDNA$scientific_name)

df_egbif_clean <- df_egbif |>
  select(kingdom, phylum, class,
         order, family, genus, verbatim_name) |> 
  rename(species = verbatim_name)


df_na_class <- df_egbif |> 
  filter(is.na(class))
species_list <- df_egbif |>
  distinct() |>
  janitor::clean_names() |> 
  select(verbatim_name) |>
  pull() 


# Break into chunks of 1000
species_chunks <- split(species_list,
                        ceiling(seq_along(species_list) / 1000))

# Break into sub-chunks of 50 names
match_worms_subchunk <- function(names_50) {
  tryCatch({
    Sys.sleep(5)
    wm_records_names(names_50)
  }, error = function(e) {
    message("Error: ", conditionMessage(e))
    return(NULL)
  })
}

# Function to process one 1000-chunk
process_chunk <- function(species_chunk) {
  sub_chunks <- split(species_chunk,
                      ceiling(seq_along(species_chunk) / 50))
  map_dfr(sub_chunks, match_worms_subchunk)
}

# Process all 1000-chunks
all_matches <- map_dfr(species_chunks,
                       process_chunk)

df_worms <- all_matches |> 
  select(
    kingdom, phylum, class, order,
    family, genus, scientificname,
    isMarine, isBrackish, isFreshwater,
    isTerrestrial, isExtinct
  ) |> 
  rename(species = scientificname) |>
  mutate(across(isMarine:isExtinct, ~replace_na(., 0))) |> 
  mutate(
    Habitat = case_when(
      isMarine == 1 & isBrackish == 1 & isFreshwater == 1 & isTerrestrial == 1 ~ "Euryhaline",
      isMarine == 1 & isBrackish == 1 & isFreshwater == 1 ~ "Euryhaline",
      isMarine == 1 & isBrackish == 1 ~ "Euryhaline",
      isMarine == 1 & isFreshwater == 1 & isTerrestrial == 1 ~ "Euryhaline",
      isTerrestrial == 1 & isMarine != 1 & isBrackish != 1 ~ "Euryhaline",
      isMarine == 1 & isBrackish != 1 & isFreshwater != 1 & isTerrestrial != 1 ~ "Marine",
      isMarine != 1 & isBrackish == 1 & isFreshwater == 1 & isTerrestrial != 1 ~ "Euryhaline",
      isMarine == 1 & isBrackish != 1 & isFreshwater != 1 & isTerrestrial == 1 ~ "Euryhaline",
      isMarine != 1 & isBrackish == 1 & isFreshwater == 1 & isTerrestrial == 1 ~ "Brackish",
      isMarine == 1 & isBrackish != 1 & isFreshwater == 1 & isTerrestrial != 1 ~ "Euryhaline",
      isMarine != 1 & isBrackish == 1 & isFreshwater != 1 & isTerrestrial != 1 ~ "Brackish",
      isMarine != 1 & isBrackish == 1 & isFreshwater != 1 & isTerrestrial == 1 ~ "Brackish",
      TRUE ~ NA_character_
    )
  )

df_feDNA <- df_egbif_clean |>
  left_join(df_worms |>
              select(species, class, Habitat),
            by = "species", suffix = c(".clean", ".worms")) |> 
  mutate(class = coalesce(class.clean, class.worms))  |> 
  select(-class.clean, -class.worms) |>
  rename(scientific_name = species)

# Merge all the datasets 
df_manuscript <- full_join(df_feDNA,
                           df_eDNA,
                           by = "scientific_name") |>
  filter(otu != 0) |>
  mutate(otu = paste0("otu_", otu)) |>
  rename(status = match_results) |>
  mutate(Habitat = if_else(kingdom =="Fungi", "Ubiquitous", Habitat)) |>
  mutate(Habitat = if_else(phylum =="Arthropoda", "Ubiquitous", Habitat)) |>
  mutate(Habitat = if_else(class =="Aves", "Terrestrial", Habitat)) |>
  mutate(Habitat = if_else(kingdom =="Plantae", "Terrestrial", Habitat)) |>
  mutate(Habitat = if_else(family == "Cyprinidae", "Brackish", Habitat)) |>
  mutate(Habitat = if_else(class == "Squamata", "Terrestrial", Habitat)) |>
  mutate(Habitat = if_else(class == "Carnivora", "Terrestrial", Habitat)) |>
  mutate(otu = make.unique(as.character(otu)))
