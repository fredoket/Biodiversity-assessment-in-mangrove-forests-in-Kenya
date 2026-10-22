# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)

# Add information on system/habitat---------------------------------------------
api_key <- "API_Key"

# Function to retrieve habitat data for a species
get_habitat <- function(species_name) {
  base_url <- "https://apiv3.iucnredlist.org/api/v3"
  url <- paste0(base_url,
                "/habitats/species/name/",
                URLencode(species_name), "?token=", api_key)
  
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    data_json <- fromJSON(data, flatten = TRUE)
    if (!is.null(data_json$result) && length(data_json$result) > 0) {
      return(data_json$result[1, c("code", "habitat", "season")]) 
    } else {
      return(data.frame(code = NA, habitat = NA, season = NA))
    }
  } else {
    warning(paste("Failed to retrieve data for species:", species_name))
    return(data.frame(code = NA, habitat = NA, season = NA))
  }
}

# Define the species data frame
df_species <- df_final  
species_list <- df_species$Species

output_dir <- "habitat_chunks"
if (!dir.exists(output_dir)) dir.create(output_dir)

processed_files <- list.files(output_dir, pattern = "chunk_\\d+\\.csv")
processed_chunks <- as.integer(sub("chunk_(\\d+)\\.csv", "\\1",
                                   processed_files))
start_chunk <- if (length(processed_chunks) > 0)
  max(processed_chunks) + 1 else 1

# Process species in chunks
chunk_size <- 200
for (i in seq(start_chunk, ceiling(length(species_list) / chunk_size))) {
  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, length(species_list))
  species_chunk <- species_list[start_idx:end_idx]
  
  # Retrieve habitat data for the chunk
  habitat_data <- do.call(rbind, lapply(species_chunk, function(Species) {
    habitat <- get_habitat(Species)
    habitat$Species <- Species
    return(habitat)
  }))
  
  # Create a new column 'System' based on the conditions
  habitat_data$System <- with(habitat_data, 
    ifelse(grepl("Forest|Terrestrial|Artificial|Grassland|Urban",
                 habitat, ignore.case = TRUE), "Terrestrial",
    ifelse(grepl("Marine|Coastal", habitat, ignore.case = TRUE),
           "Marine",
    ifelse(grepl("Freshwater", habitat, ignore.case = TRUE),
           "Freshwater", NA))))
  
  # Save the chunk to a CSV file
  chunk_file <- file.path(output_dir, paste0("chunk_", i, ".csv"))
  write.csv(habitat_data, chunk_file, row.names = FALSE)
  
  # Print status
  cat("Processed chunk", i, "and saved to", chunk_file, "\n")
  
  # Pause for 2 seconds
  Sys.sleep(2)
}

# Merge all saved chunks into a single CSV file
all_files <- list.files(output_dir, pattern = "chunk_\\d+\\.csv",
                        full.names = TRUE)
merged_data <- do.call(rbind, lapply(all_files, read.csv))

# Save the merged data to a single file
final_output <- "merged_habitat_data.csv"
write.csv(merged_data, final_output, row.names = FALSE)

cat("All chunks have been merged into", final_output, "\n")

df_habitat <- read_csv("merged_habitat_data.csv")

# Match species in df_worms and df_habitat
df_habitat$System <- ifelse(
  df_habitat$Species %in% df_worm$Species & is.na(df_habitat$System), 
  "Marine", 
  df_habitat$System
)

df_habitat$df_worm <- NA

# Match species in df_habitat$Species with df_worms$Species
df_habitat$df_worm <- ifelse(
  df_habitat$Species %in% df_worm$Species, 
  "Marine",                                
  NA                                      
)


