library(dplyr)

df_sankey <- df_lite |>
  select(year_of_publication, document_class, site,
         kingdom, class_29, scientific_name_13,
         document_title) |>
  rename("Year" = "year_of_publication",
         "Class" = "class_29",
         "Species" = "scientific_name_13") |> 
  drop_na()

# Helper function to compute and label unique species counts
add_species_count_label <- function(df_main, df_ref, group_col, label_col) {
  counts <- df_ref |>
    select({{ group_col }}, Species) |>
    distinct() |>
    group_by({{ group_col }}) |>
    summarise(n_species = n_distinct(Species), .groups = "drop")
  
  df_main |>
    left_join(counts, by = rlang::as_string(rlang::ensym(group_col))) |>
    mutate({{ label_col }} := paste0({{ group_col }}, " (n =", n_species, ")")) |>
    select(-n_species)
}

# Apply the function to create labels for each grouping
df_sank <- df_sankey |> 
  add_species_count_label(df_sankey, kingdom, Count) |>
  add_species_count_label(df_sankey, Class, class_c) |>
  add_species_count_label(df_sankey, site, site_c)


# Count unique document per document_class and update Count
docu_counts <- df_sankey |>
  select(document_class, document_title) |>
  distinct() |>
  group_by(document_class) |>
  summarise(n = n_distinct(document_title), .groups = "drop")

df_sank <- df_sank |>
  left_join(docu_counts, by = "document_class") |>
  mutate(docu = paste0(document_class, " (n = ", n, ")")) |>
  select(-n)

# Select variables for sankey plot
df_sank <- df_sank |>
  select(Year, docu, site_c, Count, class_c) |>
  rename("Kingdom" = "Count",
         "Forest" = "site_c",
         "Source" = "docu",
         "Class" = "class_c")

# Count unique species per per Class
class_counts <- df_sankey |>
  select(Class, Species) |>
  distinct() |>
  group_by(Class) |>
  summarise(n = n_distinct(Species), .groups = "drop")

# Get the sum of others
total <- class_counts %>%
  filter(n < 20) %>%
  summarise(total_n = sum(n, na.rm = TRUE)) %>%
  pull(total_n)


# Replace low count taxa with others
df_sank <- df_sank %>%
  mutate(
    class_num = as.numeric(str_extract(Class, "\\d+(?=\\))")),
    Class = if_else(class_num < 20, paste0("Others (", total, ")"), Class)
  ) %>%
  select(-class_num) |>
  mutate(Year = factor(Year, levels = sort(unique(Year))))
