pacman::p_load(
  janitor,    
  lubridate,  
  tidyverse,  
  skimr,      
  tidyverse,  
  rgbif,      
  httr, 
  jsonlite,
  phyloseq,
  microeco,
  file2meco
  )

# Load and process register data sets
pattern <-"^[A-Z][a-z]+\\s[A-Z]?[a-z]+$"
path <- "inventory_data/" 

list <- list.files(path = path, 
                   pattern = "\\.csv$",
                   full.names = TRUE)

processed_data <- list() 

for (file in list) {
  data <- read.csv(file)
  processed <- data %>%
    filter(!is.na(Species)) %>%
    select(Species) |> 
    distinct(Species, .keep_all = TRUE)
  processed_data[[file]] <- processed
}

df_register <- bind_rows(
  processed_data
) |>
  distinct(Species) |>
  filter(grepl(pattern,
               Species))


# Load and process eDNA data
df_mangrove <- read_csv("data/eDNA_kraken2.csv") |>
  select(-taxRank, -Max, -lineage) |>
  rename_with(~ gsub("\\.cladeReads$", "", .),
    .cols = 3:97) |>
  mutate(otu = paste0("otu_", taxID)) |>
  rename("Species" = "name") |> 
  filter(grepl(pattern, Species)) |>
  select(-taxID) |>
  mutate(across(.cols = 3:97, ~
                  ifelse(is.na(.), 0, .))) |>
  distinct(Species, .keep_all = TRUE)


#Retrieve higher taxonomic names from GBIF
df_rgibif <- rgbif::name_backbone_checklist(
  df_mangrove$Species,
  curlopts = list(timeout = 120))

df_gbif <- df_rgibif |>
  select(kingdom,
         phylum, class, order,
         family, genus,
         verbatim_name) |>
  rename("Species" = "verbatim_name")


# Load WoRMS register data
df_worm <- read_csv("data/worms.csv") |>
  select(scientificname, class) |>
  drop_na() |>
  rename("Species" = "scientificname")


# Fill in class for fish species
df_class <- df_worm |> 
  full_join(df_gbif, by = "Species",
            suffix = c("_A", "_B")) |> 
  mutate(
    class = coalesce(class_A, class_B) 
  ) |> 
  select(-class_A, -class_B) |>  
  distinct(Species, .keep_all = TRUE) 

# Filter function
check <- c("kingdom", "phylum",
           "class", "order", "family",
           "genus")

# Assuming df_class and check are defined
df_filt <- df_class |> 
  rowwise()  |> 
  filter(!all(is.na(across(all_of(check))))) |>
  ungroup()


# Match to the regional register
df_eDNA <- df_filt |>
  mutate(Occurance = if_else(Species %in%
                            df_register$Species,
                          "registered", "unexpected"))


df_clean <- list(df_eDNA, df_mangrove) |> 
  reduce(full_join, by = "Species") |> 
  distinct(Species, .keep_all = TRUE) |> 
  filter(!if_all(all_of(check), is.na)) |> 
  ungroup()

# Add habitat information
df_habitat <- read_csv("data/df_habitat.csv") |>
  select(Species, Habitat)

# Match species and add habitat information
df_final <- df_clean |> 
  left_join(df_habitat, by = "Species") |> 
  mutate(Habitat = coalesce(Habitat, NA_character_)) |>
  drop_na(otu) |>
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
