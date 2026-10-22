pacman::p_load(
  janitor, lubridate, tidyverse, readr, dplyr,
  purrr, stringr, rgbif)

# Filter out taxa which does not confom to binomial nomenclature
pattern <-"^[A-Z][a-z]+\\s[A-Z]?[a-z]+$"

# Load inventoried data sets
gbif <- read_csv("inventory_data/gbif_.csv") |>
  dplyr::select(species) |>
  drop_na() |>
  unique() |>
  rename("scienticname" = "species")

obis <- read_csv("inventory_data/obis.csv") |>
  dplyr::select(scientificname) |>
  unique()

fishbase <- read_csv("inventory_data/fishbase.csv") |>
  janitor::clean_names() |>
  dplyr::select(species) |>
  unique() |>
  dplyr::rename("scientificname" = "species")

worm <- read_csv("inventory_data/WoRMS.csv") |>
  janitor::clean_names() |> 
  dplyr::select(scientific_name) |>
  unique() |>
  rename("scientificname" = "scientific_name")

# GBIF function ...............................................................
# Read and clean data ---
lit <- read_csv("inventory_data/litsurv.csv",
                show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::select(taxon_name) |>
  drop_na() |>
  distinct() |>
  filter(
    !str_detect(taxon_name, regex("\\b(sp|spp|sp1)(\\.|$)",
                                  ignore_case = TRUE)),
    str_count(taxon_name, "\\S+") > 1
  )

# Batch function gbif database quering function
check_names_batch <- function(names, batch_size = 100,
                              output_file = "lit_checked_partial.csv") {
  batches <- split(names, ceiling(seq_along(names) / batch_size))
  total_batches <- length(batches)
  
  results <- map2_dfr(batches, seq_along(batches), function(batch, idx) {
    message("\n🔎 Processing batch ", idx, " of ", total_batches, " (",
            length(batch), " names)")
    
    # Attempt query with retry logic
    for (attempt in 1:3) {
      res <- tryCatch(
        name_backbone_checklist(data.frame(name = batch)),
        error = function(e) e
      )
      
      if (!inherits(res, "error")) {
        message("✅ Batch ", idx, " completed successfully.")
        
        # --- Ensure consistent column types ---
        res <- res |> mutate(across(everything(), as.character))
        
        if (!is.null(output_file)) {
          if (file.exists(output_file)) {
            existing <- read_csv(output_file, show_col_types = FALSE) |>
              mutate(across(everything(), as.character))
            combined <- bind_rows(existing, res)
            write_csv(combined, output_file)
          } else {
            write_csv(res, output_file)
          }
        }
        
        Sys.sleep(runif(1, 1.5, 3.5))  # polite random delay
        return(res)
      } else {
        message("⚠️ Attempt ", attempt, " failed for batch ", idx, ": ",
                conditionMessage(res))
        Sys.sleep(5)
      }
    }
    
    message("❌ Batch ", idx, " failed after 3 attempts, skipping.")
    tibble()  # return empty if all failed
  })
  
  return(results)
}

# Run the query safely ---
lit_checked <- check_names_batch(lit$taxon_name, batch_size = 100)
##------------------------------------------------------------------------------




lite <- lit_checked |> 
  dplyr::select(species) |>
  drop_na() |> 
  rename("scientificname" = "species")

#List of data to merge
inventory <- list(
  gbif, obis, fishbase,
  worm, lite
)

#Standardize column names for all data sets
inventory <- lapply(inventory, function(df) {
  colnames(df) <- "scientificname"
  return(df)
})

#Merge all the data sets
df_inventory <- do.call(rbind, inventory) |>
  unique()

#Filter and remove non-binomial nomenclature names
df_inventory <- df_inventory |>
  dplyr::filter(grepl(pattern, scientificname))

df_match <- df_deDNA |>
  dplyr::select(scientific_name)

#Add a new column in A to store match results
df_match$match_results <- ifelse(df_deDNA$scientific_name
                                 %in% df_inventory$scientificname,
                                 "Expected", "Unexpected")
