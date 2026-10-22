library(tidyverse)
library(rgbif)
library(dplyr)

# --- Step 1: Read and clean data ---
lit <- read_csv("inventory_data/litsurv.csv", show_col_types = FALSE) |>
  janitor::clean_names() |>
  dplyr::select(taxon_name) |>
  drop_na() |>
  distinct() |>
  filter(
    !str_detect(taxon_name, regex("\\b(sp|spp|sp1)(\\.|$)", ignore_case = TRUE)),
    str_count(taxon_name, "\\S+") > 1
  )

# --- Step 2: Robust batch function ---
check_names_batch <- function(names, batch_size = 50, output_file = "lit_checked_partial.csv") {
  batches <- split(names, ceiling(seq_along(names) / batch_size))
  total_batches <- length(batches)
  
  results <- map2_dfr(batches, seq_along(batches), function(batch, idx) {
    message("\n🔎 Processing batch ", idx, " of ", total_batches, " (", length(batch), " names)")
    
    # Attempt query with retry logic
    for (attempt in 1:3) {
      res <- tryCatch(
        name_backbone_checklist(data.frame(name = batch)),
        error = function(e) e
      )
      
      # If successful, save and break retry loop
      if (!inherits(res, "error")) {
        message("✅ Batch ", idx, " completed successfully.")
        
        if (!is.null(output_file)) {
          if (file.exists(output_file)) {
            existing <- read_csv(output_file, show_col_types = FALSE)
            write_csv(bind_rows(existing, res), output_file)
          } else {
            write_csv(res, output_file)
          }
        }
        Sys.sleep(runif(1, 1.5, 3.5))  # polite random delay
        return(res)
      } else {
        message("⚠️ Attempt ", attempt, " failed for batch ", idx, ": ", conditionMessage(res))
        Sys.sleep(5)
      }
    }
    
    message("❌ Batch ", idx, " failed after 3 attempts, skipping.")
    tibble()  # return empty if all failed
  })
  
  return(results)
}

# --- Step 3: Run the query safely ---
lit_checked <- check_names_batch(lit$taxon_name, batch_size = 50)

