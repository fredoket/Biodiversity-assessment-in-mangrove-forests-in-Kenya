# Load library
library(dplyr)

# Load data from match_data_in_WORMS_GBIF
df_sankey <- df_clean |>
  distinct() |>
  filter(
    author != "Mwamburi et al,. 2023"
  ) |>
  mutate(
    document_class = case_when(
      document_class == "Peer-reviewed" ~ "Peer reviewed",
      document_class == "Manual" ~ "Grey source",
      document_class == "Research article" ~ "Peer reviewed",
      document_class == "Grey source" ~ "Grey source"
    )
  ) |>
  replace_na(
    list(
      class = "Teleostei",
      species = "fish",
      kingdom = "Animalia"
    )
  )


# For generating sankey plot
df_sank <- df_sankey


# Count unique sources surveyed
docu_counts <- df_sankey |>
  select(
    document_class,
    document_title
  ) |>
  distinct() |>
  group_by(document_class) |>
  summarise(
    n = n_distinct(document_title),
    .groups = "drop"
  ) |>
  mutate(
    n = if_else(n == 17, 18L, n)
  )

df_sank <- df_sank |>
  left_join(
    docu_counts,
    by = "document_class"
  ) |>
  mutate(
    Sources = paste0(document_class, " n=", n)
  ) |>
  select(-n)


# Count unique species per kingdom
kingdom_counts <- df_sankey |>
  select(
    kingdom,
    species
  ) |>
  distinct() |>
  group_by(kingdom) |>
  summarise(
    n = n_distinct(species),
    .groups = "drop"
  )

df_sank <- df_sank |>
  left_join(
    kingdom_counts,
    by = "kingdom"
  ) |>
  mutate(
    Kingdom_count = paste0(kingdom, " n=", n)
  ) |>
  select(-n)


# Count unique species per class
source("scripts/class_function.R")


# Count the number of techniques used
tech_count <- df_sankey |>
  select(
    Method,
    document_title
  ) |>
  distinct() |>
  group_by(Method) |>
  summarise(
    n = n_distinct(document_title),
    .groups = "drop"
  )

df_sank <- df_sank |>
  left_join(
    tech_count,
    by = "Method"
  ) |>
  mutate(
    method_c = paste0(Method, " n=", n)
  ) |>
  select(-n)


# Count unique documents per site
site_counts <- df_sankey |>
  select(
    site,
    document_title
  ) |>
  distinct() |>
  group_by(site) |>
  summarise(
    n = n_distinct(document_title),
    .groups = "drop"
  )

df_sank <- df_sank |>
  left_join(
    site_counts,
    by = "site"
  ) |>
  mutate(
    site_c = paste0(site, " n=", n)
  ) |>
  select(-n)


ystudy <- df_sankey |>
  select(
    year_of_publication,
    document_title
  ) |>
  distinct() |>
  group_by(year_of_publication) |>
  summarise(
    n = n_distinct(document_title),
    .groups = "drop"
  )

df_sank <- df_sank |>
  left_join(
    ystudy,
    by = "year_of_publication"
  ) |>
  mutate(
    year_c = paste0(year_of_publication, " n=", n)
  ) |>
  select(-n)


mtotal <- site_counts |>
  filter(
    n < 1
  ) |>
  summarise(
    n = sum(n, na.rm = TRUE)
  ) |>
  pull(n)


df_sank <- df_sank |>
  mutate(
    site_num = as.numeric(
      str_extract(
        site_c,
        "(?<=n=)\\d+"
      )
    ),
    site_c = if_else(
      site_num < 1,
      paste0("Others n=", mtotal),
      site_c
    )
  ) |>
  select(-site_num)


# Number of class taxa per site
taxa_class_count <- df_sankey |>
  select(
    site,
    class
  ) |>
  distinct() |>
  group_by(site) |>
  summarise(
    n = n_distinct(class),
    .groups = "drop"
  )

df_sank <- df_sank |>
  left_join(
    taxa_class_count,
    by = "site"
  ) |>
  mutate(
    taxa_class = paste0(site, " n=", n)
  ) |>
  select(-n)


# Select variables for sankey plot
df_sankf <- df_sank |>
  select(
    year_of_publication,
    year_c,
    Sources,
    site_c,
    method_c,
    class_c,
    taxa_class
  ) |>
  dplyr::rename(
    "Year of publication" = "year_of_publication",
    "Forest location" = "site_c",
    "Survey technique" = "method_c",
    "Documet type" = "Sources",
    "Documented taxa" = "class_c"
  ) |>
  mutate(
    `Year of publication` = factor(
      `Year of publication`,
      levels = sort(
        unique(`Year of publication`)
      )
    )
  )
