# Load libraries 
pacman::p_load(
  microeco, phyloseq, tidyverse,
  gt, dplyr, ggplot2, vegan,
  purrr, stringr, vegan, indicspecies
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

# Transform--------------------------------------------------------------------- 
edna_df <- merged_df2 |>
  select(family, 11:27) |>
  pivot_longer(
    cols = -family,             
    names_to = "site",         
    values_to = "count"         
  ) |>
  distinct()

edna_df$method <- "eDNA"

edna_df <- edna_df |>
  mutate(SampleID = paste(site,
                          method, sep = "_")) |>
  janitor::clean_names()
#-------------------------------------------------------------------------------

pomm_matrix <- edna_df |> 
  group_by(sample_id, family) |> 
  summarise(count = sum(count), .groups = "drop") |> 
  pivot_wider(
    names_from = family,
    values_from = count,,
    values_fill = 0
  )

# Extract site info
metadata_1 <- pomm_matrix |> 
  select(sample_id) |> 
  separate(sample_id, into = c("site", "method"),
           sep = "_", remove = FALSE) |>
  filter(method == "eDNA")


comm_matrix <- edna_df |> 
  group_by(sample_id, family) |> 
  summarise(count = sum(count), .groups = "drop") |> 
  pivot_wider(
    names_from = family,
    values_from = count,
    values_fill = 0
  )

# Remove ID and metadata cols to get numeric matrix
comm_abund <- comm_matrix |> 
  select(-sample_id)

# Bray-Curtis distance
dist_bc <- vegdist(comm_abund, method = "jaccard")

# PERMANOVA (adonis2)
permanova_res <- adonis2(dist_bc ~ site, data = metadata_1)
print(permanova_res)


richness_df <- edna_df |> 
  group_by(site, method) |> 
  summarise(richness = n_distinct(family[count > 0]), .groups = "drop")

# One-way ANOVA
anova_res <- aov(richness ~ site, data = richness_df)
summary(anova_res)



indval_res <- multipatt(comm_abund, metadata_1$site, func = "IndVal.g", duleg = TRUE, control = how(nperm=999))
summary(indval_res)


# Extract site info
metadata_ <- pomm_matrix |> 
  select(sample_id) |> 
  separate(sample_id, into = c("site", "method"), sep = "_", remove = FALSE)

# Remove ID and metadata cols to get numeric matrix
comm_abund_1 <- comm_matrix |>
  filter(str_detect(sample_id, "_eDNA$")) |> 
  select(-sample_id)

comm_abund_1 <- as.data.frame(lapply(comm_abund_1, as.numeric))
comm_abund_1 <- as.matrix(comm_abund_1)


# SIMPER to identify contributing OTUs
simper_res <- simper(comm_abund_1, group = metadata_1$site, permutations = 999)
# View top contributors for each group comparison
summary(simper_res, ordered = TRUE)


# Suppose simper_res is your simper object
simper_df <- map_df(names(simper_res), function(contrast) {
  df <- as.data.frame(simper_res[[contrast]])
  df$Taxon <- rownames(df)
  df$Contrast <- contrast
  df
})

simper_sig <- simper_df  |> 
  group_by(Contrast)  |> 
  arrange(desc(average), .by_group = TRUE)  |> 
  slice_head(n = 10)  |> 
  ungroup() |>
  arrange(desc(average))

# Draw a significant graph

sig <- ggplot(simper_sig, aes(
  x = reorder(Taxon, average),
  y = average,
  fill = Contrast
)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0))+
  labs(
    x = "Family (OTU)",
    y = "Average contribution"
  ) +
  scale_fill_viridis_d(option = "magma") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, face = "bold.italic"),
    strip.text.y = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  )

sig
