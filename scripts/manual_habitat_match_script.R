
# Import the habitat data 
sf_habitat <- read_csv("data/manual_habitat.csv") |>
  select(scientific_name, Habitat)

# First, ensure the data frames have the columns
if(!all(c("scientific_name", "Habitat") %in% colnames(df_project))) {
  stop("df_project must have columns: scientific_name, Habitat")
}
if(!all(c("scientific_name", "Habitat") %in% colnames(sf_habitat))) {
  stop("sf_habitat must have columns: scientific_name, Habitat")
}


# Perform a left join for the two data sets
df_project_updated <- df_project |> 
  left_join(sf_habitat |>
              select(scientific_name, Habitat_sf = Habitat),
            by = "scientific_name") |> 
  mutate(Habitat = if_else(is.na(Habitat),
                           Habitat_sf, Habitat)) |> 
  select(-Habitat_sf)

# Update the new data frame
df_project <- df_project_updated |>
  distinct(otu, .keep_all = TRUE)

sf_phylum <- read_csv("data/phylum.csv") |>
  select(scientific_name, phylum)

# First, ensure the data frames have the columns
if(!all(c("scientific_name", "phylum") %in% colnames(df_project))) {
  stop("df_project must have columns: scientific_name, phylum")
}
if(!all(c("scientific_name", "phylum") %in% colnames(sf_phylum))) {
  stop("sf_phylum must have columns: scientific_name, phylum")
}

df_project_phylum <- df_project |> 
  left_join(sf_phylum |>
              select(scientific_name, phylum_sf = phylum),
            by = "scientific_name") |> 
  mutate(phylum = if_else(is.na(phylum),
                           phylum_sf, phylum)) |> 
  select(-phylum_sf)

# Update the new data frame
df_project <- df_project_phylum |>
  distinct(otu, .keep_all = TRUE)
