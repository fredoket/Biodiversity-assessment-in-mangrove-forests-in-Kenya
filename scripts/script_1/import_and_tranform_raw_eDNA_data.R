pacman::p_load(phyloseq, biomformat, tidyverse,
               rgbif, tidyr, worrms, microeco,
               phyloseq, magrittr, aplot, metacoder)


# Load the data
df1 <- read_csv("data/taxa.csv")
df2 <- read_csv("data/taxar.csv")

# Check similarity (optional diagnostics)
similar_columns <- intersect(names(df1),
                             names(df2))
different_columns <- setdiff(union(names(df1),
                                   names(df2)),
                             similar_columns)

cat("Shared columns:",
    paste(similar_columns, collapse = ", "), "\n")
cat("Unique columns:",
    paste(different_columns, collapse = ", "), "\n")

# Merge files by a common identifier 
merged_df <- merge(df1, df2, by = "name", all = TRUE)

# Extract and sort column names from column 3 to 113
sorted_cols <- sort(names(merged_df)[1:116])

# Apply column selection and reordering
df_deDNA <- merged_df |>
  dplyr::select(1:2, all_of(sorted_cols),
                everything()) |>
  dplyr::select(-lineage.x, -taxRank.y,
                -taxID.y,
                -lineage.y, -Max.x, -Max.y,
                -taxRank.x) |>
  mutate(name = gsub("\\[|\\]", "", name)) |>
  rename_with(~ str_remove(., "\\.cladeReads$"),
              .cols = everything()) |>
  relocate(taxID.x, .before = 2) |>
  mutate(across(3:109, ~ replace(., is.na(.)|. < 5, 0))) |>
  filter(if_any(.cols = 3:109, ~. !=0)) |>
  rename(scientific_name = name,
         otu = taxID.x) |>
  dplyr::select(scientific_name, otu, everything()) |>
  drop_na(otu)