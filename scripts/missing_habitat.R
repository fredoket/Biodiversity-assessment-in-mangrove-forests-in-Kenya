# Create a named vector for habitat assignments
df_legg <- read_csv("data/taxon_habitat.csv") |>
  janitor::clean_names() |> 
  select(-notes_summary_justification)


# Create a named character vector:habitat values named by species
habitat_vec <- setNames(df_legg$taxon, df_legg$habitat)

# Prepare character vector of formatted strings
formatted <- paste0('"', names(habitat_vec), '" = "',
                    habitat_vec, '",')

species_map <- formatted

# Print each line to console
cat(paste(formatted, collapse = "\n"))

habitat_lookup <- c(
  "Ailuropoda melanoleuca" = "Terrestrial",
  "Ambassis agassizii" = "Aquatic",
  "Aneides hardii" = "Terrestrial",
  # (add all species here following the above table)
  "Zhangixalus omeimontis" = "Terrestrial"
)

# Assuming df$species contains your species names:
df <- df %>%
  mutate(Habitat = habitat_lookup[species]) %>%
  # Optionally fill unknown with NA or "Unknown"
  mutate(Habitat = if_else(is.na(Habitat), "Unknown", Habitat))
