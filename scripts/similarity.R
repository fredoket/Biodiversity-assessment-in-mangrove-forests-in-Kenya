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
