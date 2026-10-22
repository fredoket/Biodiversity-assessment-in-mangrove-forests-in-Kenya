df_project <- df_project_phylum |>
  distinct(otu, .keep_all = TRUE) |>
  mutate(
    # Reclassify habitat values
    Habitat = case_when(
      Habitat %in% c("Euryhaline", "Parasite") ~ "Brackish",
      TRUE ~ Habitat
    ),
    
    # Fill missing class values
    class = case_when(
      is.na(class) & scientific_name == "Amoeboaphelidium protococcarum" ~ "Aphelidea",
      is.na(class) & scientific_name == "Ancoracysta twista" ~ "Nebulidea",
      is.na(class) & scientific_name == "Cafileria marina" ~ "Bicosoecida",
      is.na(class) & scientific_name == "Euplotes vanleeuwenhoeki" ~ "Spirotrichea",
      is.na(class) & scientific_name == "Cyprinodontiformes" ~ "Teleostei",
      is.na(class) & scientific_name == "Fundulus dispar" ~ "Teleostei",
      is.na(class) & scientific_name == "Periophthalmus magnuspinnatus" ~ "Teleostei",
      is.na(class) & scientific_name == "Priapulus caudatus" ~ "Priapulimorpha",
      is.na(class) & scientific_name == "Tetraodon biocellatus" ~ "Teleostei",
      TRUE ~ class
    ),
    
    # Fill missing family values
    family = case_when(
      is.na(family) & scientific_name == "Cafileria marina" ~ "Bicosoecidae",
      is.na(family) & scientific_name == "Cloacina communis" ~ "Chabertiidae",
      is.na(family) & scientific_name == "Edaphochlamys debaryana" ~ "Chlamydomonadaceae",
      is.na(family) & scientific_name == "Euplotes vanleeuwenhoeki" ~ "Euplotidae",
      is.na(family) & scientific_name == "Gonopsimorpha nigrosignata" ~ "Pentatomidae",
      is.na(family) & scientific_name == "Harringtonia lauricola" ~ "Ophiostomataceae",
      is.na(family) & scientific_name == "Hyaloraphidium curvatum" ~ "Hyaloraphidiaceae",
      is.na(family) & scientific_name == "Jaminaea angkorensis" ~ "Chaetomiaceae",
      is.na(family) & scientific_name == "Limnomonas spitsbergensis" ~ "Chlamydomonadaceae",
      is.na(family) & scientific_name == "Madagascaria erythrocladioides" ~ "Erythrotrichiaceae",
      is.na(family) & scientific_name == "Madurella mycetomatis" ~ "Lophiostomataceae",
      is.na(family) & scientific_name == "Navis striatus" ~ "Muridae",
      is.na(family) & scientific_name == "Pharyngomonas kirbyi" ~ "Heterolobosea",
      is.na(family) & scientific_name == "Pseudoerythrocladia kornmannii" ~ "Erythrotrichiaceae",
      is.na(family) & scientific_name == "Sphenophorus sp. BYU-CO246" ~ "Dryophthoridae",
      is.na(family) & scientific_name == "Tessaromerus quadriarticulatus" ~ "Urostylidae",
      is.na(family) & scientific_name == "Trebouxiophyceae sp. MX-AZ01" ~ "Trebouxiophyceae",
      is.na(family) & scientific_name == "Trichogaster fasciata" ~ "Osphronemidae",
      is.na(family) & scientific_name == "Trichonotus setiger" ~ "Trichonotidae",
      is.na(family) & scientific_name == "Vermamoeba vermiformis" ~ "Vermamoebidae",
      TRUE ~ family
    )
  )