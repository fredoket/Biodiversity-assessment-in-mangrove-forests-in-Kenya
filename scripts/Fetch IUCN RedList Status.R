# Install necessary packages
if (!requireNamespace("rredlist", quietly = TRUE)) {
  install.packages("rredlist")
}

library(rredlist)

# IUCN API Key
iucn_api_key <- 'API_Key'



get_species_status <- function(species_name, api_key) {
  tryCatch({
    result <- rl_search(species_name, key = api_key)
    
    if (length(result$result) > 0) {
      status <- result$result$category
      cat("Species:", species_name, "\nStatus:", status, "\n\n")
      return(status)
    } else {
      cat("Species:", species_name, "not found on the IUCN Red List.\n\n")
      return(NA)
    }
  }, error = function(e) {
    cat("Error fetching data for species:", species_name, "\n")
    return(NA)
  })
}

# Import data
df_data <- readr::read_csv("iucn.csv") |>
  janitor::clean_names()

species_list <- df_data$scientific_name


statuses <- sapply(species_list, function(species) {
  get_species_status(species, iucn_api_key)
})
results <- data.frame(Species = species_list, Status = statuses)
print(results)
