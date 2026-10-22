pacman::p_load(phyloseq,
               biomformat,
               tidyverse,
               rgbif,
               tidyr,
               worrms,
               microeco,
               phyloseq,
               magrittr,
               aplot,
               metacoder)


# Load the data
df1 <- read.csv("data/taxa.csv",
                stringsAsFactors = FALSE)
df2 <- read.csv("data/taxar.csv",
                stringsAsFactors = FALSE)



# Check column similarity
source("scripts/similarity.R")



# Apply column selection and reordering
df_eDNA <- merged_df |>
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
  mutate(across(3:109, ~ replace(., is.na(.)|. < 4, 0))) |>
  filter(if_any(.cols = 3:109, ~. !=0)) |>
  rename(scientific_name = name,
         otu = taxID.x) |>
  select(scientific_name, otu, everything()) |>
  drop_na(otu)


# Prepare the register from the downloads

source("scripts/inventory.R")



#Merge the match and the df_data
df_eDNA <- full_join(df_eDNA, df_match,
                     by = "scientific_name") 
  

# Habitat function
source("scripts/worrms_cleaning.R")


# Microbial community
batch1 <- read_csv("data/batch_1_bacteria.csv")
batch2 <- read_csv("data/batch_2_bacteria.csv")


df_bacteria <- merge(batch1,
                      batch2, by = "name",
                      all = TRUE) |>
  rename_with(~ str_remove(., "\\.cladeReads$"),
              .cols = everything()) |>
  select(-taxRank.x, -Max.x) |>
  rename("scientific_name" = "name") |>
  rename_with(~ str_remove(., "\\.x$"),
              .cols = everything()) |>
  mutate(Habitat = "ubiquitous") |>
  relocate(Habitat, .before = 4) |> 
  mutate(status = "Expected") |>
  relocate(status, .before = 7) |>
  select(-matches("\\.y$")) |>
  rename("otu" = "taxID") |>
  rename("kingdom" = "Kingdom",
         "phylum" = "Phylum",
         "class" = "Class",
         "order" = "Order",
         "family" = "Family",
         "genus" = "Genus") |>
  mutate(otu = paste0("otu_", otu)) |>
  mutate(across(11:113, ~ replace(., is.na(.)|. < 50, 0))) |>
  filter(if_any(.cols = 11:113, ~. !=0))


# Correct data for merging bacteria and Eukaryotes
source("scripts/similamerge.R")
 


# Now merge (inner join on the shared columns)
df_project <- bind_rows(df_bacteria_extended,
                       df_manuscript_extended) |>
  mutate(across(11:117, ~ ifelse(is.na(.), 0, .))) |>
  mutate(Habitat = case_when(
    Habitat == "ubiquitous" ~ "Ubiquitous",
    TRUE ~ Habitat)) |>
  drop_na(otu) |>
  distinct(otu, .keep_all = TRUE)


# Assess the structure of the data
sh_habita <- df_project |>
  filter(is.na(Habitat)) |>
  select(scientific_name)

write.csv(sh_habita, "data/sh_habitat.csv")


# Filling the missing habitat
source("scripts/manual_habitat_match_script.R")
source("scripts/exploratory_graphs.R")

#Obtain OTU from the data frame
tax_table <- df_project |>
  select(otu, Habitat, status,
         kingdom, phylum, class, order,
         family, genus, scientific_name) |>
  tibble::column_to_rownames("otu")


#Obtain taxa abundance data frame 
df_abundance <- df_project |>
  select(otu, 11:117) |>
  tibble::column_to_rownames("otu")


df_metadata <- read.csv("data/meta.csv") |>
  janitor::clean_names() |>
  filter(!str_ends(sample_id, "b")) |>
  mutate(sample_id =
           make.unique(as.character(sample_id))) |>
  tibble::column_to_rownames("sample_id")


#Transform to R6 class
otu_table <- df_abundance |>
  as.data.frame()

tax_table <- df_project |>
  select(otu, status, Habitat, kingdom,
         phylum, class, order,
         family, genus, scientific_name) |>
  tibble::column_to_rownames("otu") |>
  as.data.frame()

df_microeco <- microtable$new(
  otu_table = otu_table,
  tax_table = tax_table,
  sample_table = df_metadata
)    

#Transform into matrices otu and tax tables 
df_otutax <- as.matrix(tax_table)
df_abundance <- as.matrix(df_abundance)

#Transform to S4 data class
OTU <- otu_table(df_abundance,
                 taxa_are_rows = TRUE)
TAX <- tax_table(df_otutax)
sample <- sample_data(df_metadata)


df_phylo <- phyloseq(OTU,
                     TAX, sample)


# Diversity and statistical analysis

source("scripts/diversity_n_statistical_analysis.R")


