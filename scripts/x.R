library(dplyr)
library(stringr)

df_fasta_header <- readr::read_csv("data/all_headers.csv") |>
  dplyr::distinct(header, .keep_all = TRUE) |>
  filter(!str_detect(header, regex("UNVERIFIED", ignore_case = TRUE))) |>
  mutate(
    name = str_remove_all(header, "\\[|\\]"),
    name = str_remove(header, "TPA_asm:\\s*"),
    name = str_remove(name, "TPA:\\s*"),
    scientific_name = header
    
  ) |>
  distinct(header, .keep_all = TRUE)|>
  
  mutate(
    cleaned = name |>
      str_remove(accession) |>
      str_remove(",.*$") |>
      str_remove("\\b(chloroplast|cultivar|isolate|genome|plastid|complete|partial|voucher|sequence)\\b.*$") |>
      str_squish()
  ) |>
  mutate(
    cleaned = cleaned %>%
      # Remove bracketed taxonomic sections e.g. (sect. Taraxacum)
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      
      # Retain Genus + species OR Genus + sp.
      str_extract("^[A-Z][a-zA-Z-]+\\s+(?:sp\\.|[a-z-]+)") %>%
      
      # Trim whitespace
      str_trim()
  ) |> 
  mutate(
    fasta_header = str_extract(
      name,
      "^\\S+\\s+\\S+\\s+\\S+"
    )
  ) |>
  mutate(
    accession_number = str_extract(name, "^\\S+")
  ) |>
  select(accession_number, fasta_header) |>
  filter(
    str_count(fasta_header, "\\S+") == 3,
    !word(fasta_header, 3) %in% c("sp", "sp.")
  )


  



distinct(cleaned, .keep_all = TRUE) |>
  filter(!str_detect(cleaned, "^[A-Z][a-zA-Z-]+\\s+sp\\.?$")) |>
  dplyr::select(file_name, accession, cleaned) |>
  mutate(faster_header = paste(accession, cleaned))

