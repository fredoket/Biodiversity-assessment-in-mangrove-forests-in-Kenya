library(dplyr)
library(stringr)

df_fasta_header <- readr::read_csv("all_chloroplast_metadata.csv") |>
  dplyr::distinct(species, .keep_all = TRUE) |>
  filter(!str_detect(title, regex("UNVERIFIED", ignore_case = TRUE))) |> 
  mutate(species = gsub("\\[|\\]", "", species)) |>
  filter(!str_detect(species, "\\s+x\\s+")) |> 
  filter(!str_detect(species, "\\b(subsp\\.?|var\\.?|sp\\.?)\\b")) |>
  filter(
    !(
      (group == "nonangiosperm" & (genome_size_bp < 80000 | genome_size_bp > 220000)) |
        (group == "angiosperm"    & (genome_size_bp < 120000 | genome_size_bp > 170000))
    )
  ) |> 
  filter(
    str_detect(species, "^[A-Z][a-z]+\\s+[a-z-]+$"),
    !str_detect(
      species,
      regex("sp\\.|subsp\\.|var\\.|hybrid|cultivar|cv\\.|clone|cf\\.|aff\\.|unknown|unidentified|sample",
            ignore_case = TRUE)
    )
  )
