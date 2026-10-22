pacman::p_load(worrms, rgbif,
               dplyr, purrr,
               tibble, tidyverse)

#Load data
taxa_list <- read_csv("data/lit_rev_0.1.csv") |>
  distinct() |>
  janitor::clean_names() |> 
  select(scientific_name) |>
  pull()

df_taxa <- read_csv("data/lit_rev_0.1.csv") |>
  distinct() |>
  janitor::clean_names() |>
  select(1:13) |>
  rename("species" = "scientific_name") |>
  mutate(Method = case_when(
    grepl("net|sein|gillnet|castnet|flyke|stake", tools, ignore.case = TRUE) ~ "Use of nets",
    grepl("direct observation", tools, ignore.case = TRUE) ~ "Direct observation",
    grepl("DNA", tools, ignore.case = TRUE) ~ "Metabarcoding",
    grepl("review", tools, ignore.case = TRUE) ~ "Literature review",
    grepl("transect", tools, ignore.case = TRUE) ~ "Direct observation",
    grepl("trap", tools, ignore.case = TRUE) ~ "Trap",
    grepl("microscopy", tools, ignore.case = TRUE) ~ "Microscopy",
    TRUE ~ NA_character_
  ))

# Split taxa into chunks of 50
taxa_chunks <- split(taxa_list, ceiling(seq_along(taxa_list)/50))

# match taxa in the WORMS database
match_worms_batch <- function(taxa) {
  map_dfr(taxa, function(name) {
    res <- tryCatch(wm_records_name(name), error = function(e) NULL)
    if (!is.null(res) && length(res) > 0) {
      as_tibble(res)[1, ] %>% mutate(original_name = name)
    } else {
      tibble(original_name = name, AphiaID = NA)
    }
  })
}

# Collate the batches of result from WORMS
worms_matches <- map_dfr(taxa_chunks, match_worms_batch)

df_worms <- worms_matches |>
  select(kingdom, phylum, class,
         order, family, genus, original_name) |>
  rename("species" = "original_name")

# set 60 seconds
httr::set_config(httr::timeout(60))  


# Match taxa in GBIF database
df_gbif <- lapply(taxa_list, function(x) {
  tryCatch(name_backbone(name = x), error = function(e) NULL)
})

# Convert list to dataframe safely
df_gbif <- df_gbif |> 
  compact() |>                 
  map_df(~as.data.frame(.x, stringsAsFactors = FALSE), .id = "query_id")



df_rgibf <- df_gbif |>
  select(kingdom, phylum, class,
         order, family, genus, verbatim_name) |>
  rename("species" = "verbatim_name")

#Remove duplicates to get one row per species
df_rgibf_unique <- df_rgibf |> 
  filter(!is.na(class))  |> 
  distinct(species, .keep_all = TRUE)


df_combined <- df_worms |> 
  left_join(
    df_rgibf_unique |> 
      select(species, kingdom, phylum, class, order, family, genus),
    by = "species",
    suffix = c(".worms", ".rgibf")
  )  |> 
  mutate(
    kingdom = coalesce(kingdom.worms, kingdom.rgibf),
    phylum  = coalesce(phylum.worms, phylum.rgibf),
    class   = coalesce(class.worms, class.rgibf),
    order   = coalesce(order.worms, order.rgibf),
    family  = coalesce(family.worms, family.rgibf),
    genus   = coalesce(genus.worms, genus.rgibf)
  )  |> 
  select(-ends_with(".worms"), -ends_with(".rgibf"))

df_clean <- full_join(df_taxa,
                      df_combined, by = "species") 
