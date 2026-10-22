# Load libraries 
pacman::p_load(
  microeco, phyloseq,
  gt, dplyr, ggplot2,
  purrr, stringr, vegan
)

# Load data
df_microeco <- readRDS(
  "data/df_microeco_2.rds"
  )

# Remove OTUs with fewer than 20 contigs
df_divstat <- clone(df_microeco)

df_divstat$sample_table <- subset(
  df_divstat$sample_table,
  system == "Mangrove"
)

df_divstat$filter_taxa(
  rel_abund = 0.00000102,
  freq = 1
)
df_divstat$tidy_dataset()

# Remove "Unexpected" and none marine taxa
df_marine <- clone(df_divstat)

df_marine$tax_table <- subset(
  df_marine$tax_table,
  Habitat != "Terrestrial" &
    Habitat != "Freshwater" &
    status == "Expected"
)

# Clean and remove the filtered metadata
df_marine$tidy_dataset()

# Clone data
df_phylamarine <- clone(df_marine)


# OTU table as a matrix (taxa × samples)
otu_tab <- df_phylamarine$otu_table  

# If you want a data frame
otu_df <- as.data.frame(otu_tab)

# Add OTU IDs as a column
otu_df <- tibble::rownames_to_column(otu_df, "OTU")

# Taxonomy table as a matrix (OTUs × taxonomic ranks)
tax_tab <- df_phylamarine$tax_table  

# Convert to data frame
tax_df <- as.data.frame(tax_tab)

# Add OTU IDs as a column
tax_df <- tibble::rownames_to_column(tax_df,
                                     "OTU")

# Merge the data
merged_df4 <- merge(tax_df, otu_df,
                    by = "OTU",
                    all.x = TRUE) 

merged_df2 <- merged_df4 |> 
  mutate(
    Vanga = rowSums(across(c(VCS, VCH, VCH_12S)), na.rm = TRUE),
    Mwena = rowSums(across(c(MCS, MCH, MCH_12S)), na.rm = TRUE),
    Marereni = rowSums(across(c(RCS, RCH, RCH_12S)), na.rm = TRUE),
    Sii = rowSums(across(c(SCS, SCH, SCH_12S)), na.rm = TRUE)
  ) |> 
  select(-c(VCS, VCH, VCH_12S,
            MCS, MCH, MCH_12S,
            RCS, RCH, RCH_12S,
            SCS, SCH, SCH_12S)) %>%
  { 
    df <- .
    tax_cols <- c("OTU", "status",
                  "Habitat","kingdom",
                  "phylum","class",
                  "order","family",
                  "genus","scientific_name")
    
    site_cols <- setdiff(colnames(df), tax_cols)
    
    # Normalize names: strip suffix numbers & underscores
    site_base <- str_replace(site_cols, "_[0-9]+$|_12S$", "")
    
    # Collapse by site base name
    bind_cols(
      df[, tax_cols],
      map_dfc(unique(site_base), function(sb) {
        cols <- site_cols[site_base == sb]
        tibble(!!sb := rowSums(select(df, all_of(cols)), na.rm = TRUE))
      })
    )
  }

# Number of specie of interest

nmd <- merged_df4 |>
  dplyr::select(class, family, OTU) |>
  drop_na() |>
  group_by(class, family) |>
  summarise(count = n_distinct(OTU)) |>
  ungroup()
  
