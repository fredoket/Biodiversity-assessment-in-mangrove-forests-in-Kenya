# ============================================================
# COASTAL KENYA MANGROVE MAP: VANGA TO KIUNGA/LAMU
# Global Mangrove Watch v4.1.12 | 2025
#
# Land boundaries: GADM
# Rivers: HydroRIVERS
# Basemap: Esri World Shaded Relief
#
# Map elements:
#   - Kenya location inset: top left
#   - Compass rose: top right
#   - Custom legend: bottom right
#   - Black-and-white scale bar: bottom left
# ============================================================


# ============================================================
# 0. PACKAGES AND GENERAL SETTINGS
# ============================================================

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(R.utils)
library(geodata)
library(terra)
library(maptiles)
library(ggspatial)

sf_use_s2(FALSE)
options(timeout = 1200)


# ============================================================
# 1. DIRECTORIES AND FILES
# ============================================================

dir.create("data_2", showWarnings = FALSE, recursive = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create("data_2/tiles", showWarnings = FALSE, recursive = TRUE)

kenya_file      <- "data_2/kenya_mangroves_2025.gpkg"
rivers_file     <- "data_2/kenya_coast_rivers.gpkg"
local_kenya_shp <- "data_2/gadm41_KEN_0.shp"

# Minimum mean river discharge in m3/s
min_discharge <- 10

# Terrain basemap settings
basemap_provider <- "Esri.WorldShadedRelief"
basemap_zoom     <- 10
basemap_agg      <- 2

# Map colours
sea_col       <- "#DCEBF5"
land_col      <- "grey96"
river_col     <- "#1F78B4"
mangrove_col  <- "#007A33"
mangrove_edge <- "#003D1A"
sample_col    <- "#D7191C"


# ============================================================
# 2. DOWNLOAD AND PROCESS GLOBAL MANGROVE WATCH DATA
# ============================================================

if (file.exists(kenya_file)) {
  
  message("Kenya mangrove dataset found. Skipping download.")
  
} else {
  
  message("Kenya mangrove dataset not found. Processing GMW v4.1.12...")
  
  gmw_url <- paste0(
    "https://zenodo.org/records/21346457/files/",
    "gmw_v4112_2025_mng_ext_cntry_info_vec.gpkg.gz",
    "?download=1"
  )
  
  gz_file <- "data_2/gmw_v4112_2025_mng_ext_cntry_info_vec.gpkg.gz"
  gpkg_file <- "data_2/gmw_v4112_2025_mng_ext_cntry_info_vec.gpkg"
  
  if (!file.exists(gz_file)) {
    
    message("Downloading GMW v4.1.12 2025 vector data...")
    
    download.file(
      gmw_url,
      gz_file,
      mode = "wb"
    )
  }
  
  if (!file.exists(gpkg_file)) {
    
    message("Extracting GeoPackage...")
    
    R.utils::gunzip(
      gz_file,
      destname = gpkg_file,
      overwrite = TRUE,
      remove = FALSE
    )
  }
  
  message("Extracting Kenya mangrove polygons...")
  
  gmw <- st_read(gpkg_file, quiet = TRUE)
  
  kenya_mangrove <- gmw |>
    filter(cntry_name == "Kenya")
  
  st_write(
    kenya_mangrove,
    kenya_file,
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message("Kenya mangrove dataset saved.")
}


# ============================================================
# 3. LOAD AND VALIDATE KENYA MANGROVE DATA
# ============================================================

kenya_mangrove <- st_read(
  kenya_file,
  quiet = TRUE
) |>
  st_transform(4326) |>
  st_make_valid()


# ============================================================
# 4. DEFINE THE COASTAL STUDY EXTENT
# ============================================================

coast_bbox <- c(
  xmin = 39.10,
  xmax = 41.60,
  ymin = -4.80,
  ymax = -1.60
)

coast_bbox_sf <- st_bbox(
  coast_bbox,
  crs = st_crs(4326)
)

coast_bbox_geo <- st_as_sfc(coast_bbox_sf)


# ============================================================
# 5. LOAD LAND BOUNDARIES
# ============================================================

get_gadm <- function(iso) {
  
  geodata::gadm(
    country = iso,
    level = 0,
    path = "data_2"
  ) |>
    st_as_sf() |>
    st_transform(4326) |>
    st_make_valid() |>
    select(geometry)
}


# Use a local Kenya boundary if available
kenya <- if (file.exists(local_kenya_shp)) {
  
  st_read(
    local_kenya_shp,
    quiet = TRUE
  ) |>
    st_transform(4326) |>
    st_make_valid() |>
    st_geometry() |>
    st_sf()
  
} else {
  
  get_gadm("KEN")
}


# Include neighbouring countries to preserve geographical context
land <- bind_rows(
  kenya,
  get_gadm("TZA"),
  get_gadm("SOM")
)


# ============================================================
# 6. LOAD AND PREPARE HYDRORIVERS DATA
# ============================================================

find_rivers_shp <- function() {
  
  matches <- list.files(
    "data_2",
    pattern = "HydroRIVERS_v10_af\\.shp$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(matches) == 0) {
    return(NA_character_)
  }
  
  matches[1]
}


if (file.exists(rivers_file)) {
  
  message("Cached coastal rivers found.")
  
  rivers <- st_read(
    rivers_file,
    quiet = TRUE
  )
  
} else {
  
  zip_file <- "data_2/HydroRIVERS_v10_af_shp.zip"
  
  shp_path <- find_rivers_shp()
  
  if (is.na(shp_path)) {
    
    if (!file.exists(zip_file)) {
      
      message("Downloading HydroRIVERS Africa data...")
      
      download.file(
        "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_af_shp.zip",
        zip_file,
        mode = "wb"
      )
    }
    
    message("Extracting HydroRIVERS data...")
    
    unzip(
      zip_file,
      exdir = "data_2"
    )
    
    shp_path <- find_rivers_shp()
  }
  
  if (is.na(shp_path)) {
    stop("HydroRIVERS shapefile could not be located in data_2/.")
  }
  
  message("Reading HydroRIVERS data: ", shp_path)
  
  rivers <- st_read(
    shp_path,
    wkt_filter = st_as_text(
      st_as_sfc(
        st_bbox(
          c(
            xmin = 38.5,
            xmax = 42.0,
            ymin = -5.2,
            ymax = -1.2
          ),
          crs = st_crs(4326)
        )
      )
    ),
    quiet = TRUE
  ) |>
    st_transform(4326) |>
    filter(DIS_AV_CMS >= 1) |>
    select(DIS_AV_CMS)
  
  # Retain river sections intersecting land
  rivers <- suppressWarnings(
    st_intersection(
      rivers,
      st_union(land)
    )
  ) |>
    st_collection_extract("LINESTRING")
  
  st_write(
    rivers,
    rivers_file,
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message("Coastal rivers saved.")
}


# Select major rivers
major_rivers <- rivers |>
  filter(DIS_AV_CMS >= min_discharge)


# ============================================================
# 7. DOWNLOAD THE TERRAIN BASEMAP
# ============================================================

basemap <- tryCatch({
  
  pad <- 0.05
  
  tile_area <- st_as_sfc(
    st_bbox(
      c(
        xmin = coast_bbox[["xmin"]] - pad,
        xmax = coast_bbox[["xmax"]] + pad,
        ymin = coast_bbox[["ymin"]] - pad,
        ymax = coast_bbox[["ymax"]] + pad
      ),
      crs = st_crs(4326)
    )
  )
  
  tiles <- maptiles::get_tiles(
    tile_area,
    provider = basemap_provider,
    zoom = basemap_zoom,
    crop = TRUE,
    cachedir = "data_2/tiles",
    verbose = FALSE
  )
  
  terra::project(
    tiles,
    "EPSG:4326",
    method = "bilinear"
  )
  
}, error = function(e) {
  
  message(
    "Terrain basemap unavailable: ",
    conditionMessage(e),
    ". Using plain land polygons instead."
  )
  
  NULL
})


# Convert raster tiles into a data frame for ggplot2
basemap_layers <- if (!is.null(basemap)) {
  
  bm <- if (basemap_agg > 1) {
    terra::aggregate(
      basemap,
      fact = basemap_agg
    )
  } else {
    basemap
  }
  
  d <- as.data.frame(
    bm,
    xy = TRUE,
    na.rm = FALSE
  )
  
  names(d)[3:5] <- c("r", "g", "b")
  
  d <- d[
    stats::complete.cases(d[, c("r", "g", "b")]),
  ]
  
  d$col <- rgb(
    pmin(pmax(d$r, 0), 255),
    pmin(pmax(d$g, 0), 255),
    pmin(pmax(d$b, 0), 255),
    maxColorValue = 255
  )
  
  list(
    geom_raster(
      data = d,
      aes(x = x, y = y, fill = col)
    ),
    scale_fill_identity()
  )
  
} else {
  
  list(
    geom_sf(
      data = land,
      fill = land_col,
      colour = "white",
      linewidth = 0.15
    )
  )
}


# ============================================================
# 8. CROP MANGROVES AND READ SAMPLING LOCATIONS
# ============================================================

coastal_mangrove <- suppressWarnings(
  st_crop(
    kenya_mangrove,
    coast_bbox_sf
  )
)


samples_data <- read_csv(
  "data_2/cords.csv",
  show_col_types = FALSE
)

# Check the required columns before creating spatial points
required_columns <- c(
  "longitude",
  "latitude",
  "site"
)

if (!all(required_columns %in% names(samples_data))) {
  stop(
    "cords.csv must contain these columns: ",
    paste(required_columns, collapse = ", ")
  )
}

samples_sf <- samples_data |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )


# Coordinates for site labels
samples_labels <- samples_sf |>
  st_coordinates() |>
  as.data.frame() |>
  bind_cols(
    samples_sf |>
      st_drop_geometry() |>
      select(site)
  )


# ============================================================
# 9. MAIN MAP
# ============================================================

main_map <- ggplot() +
  
  # Terrain basemap
  basemap_layers +
  
  # Major rivers
  geom_sf(
    data = major_rivers,
    aes(linewidth = DIS_AV_CMS),
    colour = river_col,
    lineend = "round"
  ) +
  
  scale_linewidth(
    range = c(0.3, 1.1),
    trans = "sqrt",
    guide = "none"
  ) +
  
  # Mangrove distribution
  geom_sf(
    data = coastal_mangrove,
    fill = mangrove_col,
    colour = mangrove_edge,
    linewidth = 0.12
  ) +
  
  # Sampling locations
  geom_sf(
    data = samples_sf,
    shape = 16,
    size = 3.2,
    colour = sample_col
  ) +
  
  # Sampling-site labels
  geom_text_repel(
    data = samples_labels,
    aes(
      x = X,
      y = Y,
      label = site
    ),
    size = 3.5,
    hjust = 0,
    bg.color = "white",
    bg.r = 0.15,
    nudge_x = 0.12,
    direction = "y",
    box.padding = 0.3,
    min.segment.length = 0,
    segment.linewidth = 0.3,
    segment.colour = "grey30",
    max.overlaps = Inf,
    seed = 42
  ) +
  
  # Black-and-white scale bar
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.20,
    unit_category = "metric",
    style = "bar",
    bar_cols = c("black", "white"),
    text_cex = 0.75,
    line_width = 0.4,
    height = grid::unit(0.12, "cm"),
    pad_x = grid::unit(0.25, "cm"),
    pad_y = grid::unit(0.25, "cm")
  ) +
  
  # Compass rose / north arrow
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = grid::unit(1.0, "cm"),
    width = grid::unit(1.0, "cm"),
    pad_x = grid::unit(0.30, "cm"),
    pad_y = grid::unit(0.30, "cm")
  ) +
  
  # Geographic extent
  coord_sf(
    xlim = coast_bbox[c("xmin", "xmax")],
    ylim = coast_bbox[c("ymin", "ymax")],
    expand = FALSE
  ) +
  
  scale_x_continuous(
    breaks = seq(39, 42, by = 0.5)
  ) +
  
  scale_y_continuous(
    breaks = seq(-5, -1, by = 0.5)
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_bw() +
  
  theme(
    panel.background = element_rect(
      fill = sea_col,
      colour = NA
    ),
    panel.border = element_rect(
      colour = "grey30",
      fill = NA,
      linewidth = 0.5
    ),
    panel.grid = element_blank(),
    axis.ticks = element_line(
      colour = "grey30"
    ),
    axis.title = element_text(
      size = 10
    ),
    axis.text = element_text(
      size = 9
    ),
    plot.margin = margin(5, 8, 5, 5)
  )


# ============================================================
# 10. KENYA LOCATION INSET
# ============================================================

inset_map <- ggplot() +
  
  geom_sf(
    data = kenya,
    fill = "grey95",
    colour = "grey30",
    linewidth = 0.3
  ) +
  
  geom_sf(
    data = kenya_mangrove,
    fill = mangrove_col,
    colour = mangrove_col,
    linewidth = 0.2
  ) +
  
  # Rectangle showing the mapped coastal region
  geom_sf(
    data = coast_bbox_geo,
    fill = NA,
    colour = "red",
    linewidth = 0.6
  ) +
  
  coord_sf(
    expand = FALSE
  ) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = "grey40",
      linewidth = 0.4
    ),
    plot.margin = margin(3, 3, 3, 3)
  )


# ============================================================
# 11. CUSTOM LEGEND: BOTTOM RIGHT
#     Entries: Ocean, Land, Main rivers, Sampling point
# ============================================================

legend_map <- ggplot() +
  
  # White legend background
  annotate(
    "rect",
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    fill = "white",
    colour = "grey40",
    linewidth = 0.4
  ) +
  
  # Ocean symbol
  annotate(
    "rect",
    xmin = 0.07,
    xmax = 0.20,
    ymin = 0.77,
    ymax = 0.88,
    fill = sea_col,
    colour = "grey50",
    linewidth = 0.3
  ) +
  
  annotate(
    "text",
    x = 0.26,
    y = 0.825,
    label = "Ocean",
    hjust = 0,
    size = 3.1
  ) +
  
  # Land symbol
  annotate(
    "rect",
    xmin = 0.07,
    xmax = 0.20,
    ymin = 0.57,
    ymax = 0.68,
    fill = land_col,
    colour = "grey50",
    linewidth = 0.3
  ) +
  
  annotate(
    "text",
    x = 0.26,
    y = 0.625,
    label = "Land",
    hjust = 0,
    size = 3.1
  ) +
  
  # Main rivers symbol
  annotate(
    "segment",
    x = 0.07,
    xend = 0.20,
    y = 0.425,
    yend = 0.425,
    colour = river_col,
    linewidth = 0.9,
    lineend = "round"
  ) +
  
  annotate(
    "text",
    x = 0.26,
    y = 0.425,
    label = "Main rivers",
    hjust = 0,
    size = 3.1
  ) +
  
  # Sampling point symbol
  annotate(
    "point",
    x = 0.135,
    y = 0.225,
    shape = 21,
    size = 3.2,
    fill = "white",
    colour = sample_col,
    stroke = 1
  ) +
  
  annotate(
    "text",
    x = 0.26,
    y = 0.225,
    label = "Sampling point",
    hjust = 0,
    size = 3.1
  ) +
  
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1),
    expand = FALSE,
    clip = "off"
  ) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.margin = margin(0, 0, 0, 0)
  )


# ============================================================
# 12. COMBINE MAP, INSET AND LEGEND
# ============================================================

final_map <- main_map +
  
  # Kenya inset: top left
  inset_element(
    inset_map,
    left = 0.015,
    bottom = 0.60,
    right = 0.34,
    top = 0.985
  ) +
  
  # Custom legend: bottom right
  inset_element(
    legend_map,
    left = 0.72,
    bottom = 0.035,
    right = 0.985,
    top = 0.285
  )


# Display the map
print(final_map)


# ============================================================
# 13. SAVE FINAL MAP
# ============================================================

output_file <- "output/kenya_mangrove_map_3.png"

if (file.exists(output_file)) {
  file.remove(output_file)
}

ggsave(
  filename = output_file,
  plot = final_map,
  width = 7.5,
  height = 9,
  dpi = 600,
  bg = "white"
)

ggsave(
  "output/kenya_mangrove_map.pdf",
  plot = final_map,
  width = 7.5,
  height = 9,
  bg = "white"
)

message("Map saved successfully.")
