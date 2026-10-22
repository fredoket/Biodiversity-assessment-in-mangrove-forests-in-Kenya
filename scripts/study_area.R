# ============================================================
# COASTAL KENYA MANGROVE MAP
# Global Mangrove Watch v4.1.12 | 2025
# ============================================================

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(patchwork)
library(R.utils)


# ============================================================
# 1. DIRECTORIES AND FILES
# ============================================================

dir.create("data_2", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

kenya_file <- "data_2/kenya_mangroves_2025.gpkg"


# ============================================================
# 2. DOWNLOAD AND PROCESS GMW DATA IF REQUIRED
# ============================================================

if (file.exists(kenya_file)) {
  
  message("Kenya mangrove dataset found. Skipping download and processing.")
  
} else {
  
  message("Kenya mangrove dataset not found. Processing GMW v4.1.12...")
  
  url <- paste0(
    "https://zenodo.org/records/21346457/files/",
    "gmw_v4112_2025_mng_ext_cntry_info_vec.gpkg.gz",
    "?download=1"
  )
  
  gz_file <- "data_2/gmw_v4112_2025_mng_ext_cntry_info_vec.gpkg.gz"
  gpkg_file <- "data_2/gmw_v4112_2025_mng_ext_cntry_info_vec.gpkg"
  
  
  # Download GMW vector
  if (!file.exists(gz_file)) {
    
    message("Downloading GMW v4.1.12 2025 vector...")
    
    download.file(
      url,
      gz_file,
      mode = "wb"
    )
    
  }
  
  
  # Extract GeoPackage
  if (!file.exists(gpkg_file)) {
    
    message("Extracting GeoPackage...")
    
    R.utils::gunzip(
      gz_file,
      destname = gpkg_file,
      overwrite = TRUE,
      remove = FALSE
    )
    
  }
  
  
  # Extract Kenya mangrove extent
  message("Extracting Kenya mangrove extent...")
  
  gmw <- st_read(
    gpkg_file,
    quiet = TRUE
  )
  
  kenya_mangrove <- gmw |>
    filter(cntry_name == "Kenya")
  
  
  # Save Kenya dataset
  st_write(
    kenya_mangrove,
    kenya_file,
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message("Kenya mangrove dataset saved.")
  
}


# ============================================================
# 3. LOAD KENYA MANGROVE DATA
# ============================================================

kenya_mangrove <- st_read(
  kenya_file,
  quiet = TRUE
)


# ============================================================
# 4. DEFINE COASTAL STUDY EXTENT
# ============================================================

coast_bbox <- c(
  xmin = 39.0,
  xmax = 41.5,
  ymin = -4.8,
  ymax = -1.5
)


# ============================================================
# 5. CREATE KENYA BASEMAP AND COASTLINE
# ============================================================

kenya <- ne_countries(
  country = "Kenya",
  scale = "medium",
  returnclass = "sf"
) |>
  st_transform(4326)

kenya_coastline <- ne_coastline(
  scale = "medium",
  returnclass = "sf"
) |>
  st_transform(4326)


# ============================================================
# 6. CROP DATA TO COASTAL STUDY REGION
# ============================================================

coastal_mangrove <- st_crop(
  kenya_mangrove,
  st_bbox(coast_bbox)
)

coastal_coastline <- st_crop(
  kenya_coastline,
  st_bbox(coast_bbox)
)


# ============================================================
# 7. READ SAMPLING SITES
# ============================================================

samples_sf <- read_csv(
  "data_2/cords.csv",
  show_col_types = FALSE
) |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

samples_labels <- samples_sf |>
  st_coordinates() |>
  as.data.frame() |>
  bind_cols(
    samples_sf |>
      st_drop_geometry() |>
      select(site)
  )


# ============================================================
# 8. CREATE MAIN COASTAL MAP
# ============================================================

main_map <- ggplot() +
  
  geom_sf(
    data = kenya,
    fill = "grey95",
    colour = NA
  ) +
  
  geom_sf(
    data = coastal_coastline,
    colour = "grey30",
    linewidth = 0.5
  ) +
  
  geom_sf(
    data = coastal_mangrove,
    fill = "darkgreen",
    colour = NA,
    alpha = 0.85
  ) +
  
  geom_sf(
    data = samples_sf,
    shape = 21,
    size = 3.5,
    fill = "white",
    colour = "red",
    stroke = 1
  ) +
  
  geom_text_repel(
    data = samples_labels,
    aes(
      x = X,
      y = Y,
      label = site
    ),
    size = 3.5,
    fontface = "plain",
    box.padding = 0.4,
    point.padding = 0.3,
    min.segment.length = 0,
    segment.linewidth = 0.3,
    max.overlaps = Inf
  ) +
  
  coord_sf(
    xlim = c(
      coast_bbox["xmin"],
      coast_bbox["xmax"]
    ),
    ylim = c(
      coast_bbox["ymin"],
      coast_bbox["ymax"]
    ),
    expand = FALSE
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.25
    ),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5)
  )


# ============================================================
# 9. CREATE KENYA INSET
# ============================================================

study_box <- st_as_sfc(
  st_bbox(
    coast_bbox,
    crs = st_crs(4326)
  )
)

inset_map <- ggplot() +
  
  geom_sf(
    data = kenya,
    fill = "grey95",
    colour = "grey30",
    linewidth = 0.4
  ) +
  
  geom_sf(
    data = kenya_mangrove,
    fill = "darkgreen",
    colour = NA
  ) +
  
  geom_sf(
    data = study_box,
    fill = NA,
    colour = "red",
    linewidth = 0.5
  ) +
  
  coord_sf(
    expand = FALSE
  ) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = "grey30",
      linewidth = 0.4
    ),
    plot.margin = margin(2, 2, 2, 2)
  )


# ============================================================
# 10. COMBINE MAIN MAP AND INSET
# ============================================================

final_map <- main_map +
  inset_element(
    inset_map,
    left = 0.015,
    bottom = 0.64,
    right = 0.34,
    top = 0.98
  )

