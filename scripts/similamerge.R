# Merged data: Find shared and unique columns
shared_cols <- intersect(names(df_bacteria), 
                         names(df_manuscript))

unique_bacteria <- setdiff(names(df_bacteria),
                           shared_cols)


unique_manuscript <- setdiff(names(df_manuscript),
                             shared_cols)

# Add missing unique columns filled with 0 to each data frame
df_bacteria_extended <- df_bacteria


df_manuscript_extended <- df_manuscript |> 
  mutate(across(all_of(unique_bacteria), ~ 0))
