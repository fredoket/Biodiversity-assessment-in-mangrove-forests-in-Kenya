pacman::p_load(
  janitor,    
  lubridate,  
  tidyverse,  
  phyloseq,
  microeco,
  file2meco
)

# Match species and add habitat information
df_final <- read_csv("data/df_final - df_final.csv") |> 
  drop_na(otu) |>
  filter(kingdom != "Bacteria") |> 
  mutate(across(.cols = 9:103, ~
                  ifelse(is.na(.), 0, .)))



# Transform to S4 class data format
df_otutax <- df_final |>
  select(otu, Habitat, Occurance,
         kingdom, phylum, class, order,
         family, genus, Species) |>
  tibble::column_to_rownames("otu")

# Taxa abundance 
df_abundance <- df_final |>
  select(otu, 9:103) |>
  tibble::column_to_rownames("otu")

# Load metadata
df_metadata <- read_csv("metadata/Metadata.csv") |>
  janitor::clean_names() |> 
  tibble::column_to_rownames("sample_id") 




#Transform into matrices otu and tax tables 
df_otutax <- as.matrix(df_otutax)
df_abundance <- as.matrix(df_abundance)

OTU <- otu_table(df_abundance,
                 taxa_are_rows = TRUE)
TAX <- tax_table(df_otutax)
sample <- sample_data(df_metadata)

df_phylo <- phyloseq(OTU, TAX, sample)

# Save to the local directory
saveRDS(df_phylo, "data/df_phylo.rds")




# Transform to R6 class
otu_table <- df_abundance |>
  as.data.frame()

tax_table <- df_final |>
  select(otu, Habitat, Occurance,
         kingdom, phylum, class, order,
         family, genus, Species) |>
  tibble::column_to_rownames("otu") |>
  as.data.frame()

df_microeco <- microtable$new(
  otu_table = otu_table,
  tax_table = tax_table,
  sample_table = df_metadata
)

# Save to the local directory
saveRDS(df_microeco, "data/df_microeco.rds")
