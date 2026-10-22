# ============================================================
# ENVIRONMENTAL DNA SAMPLING SITES
# Coastal Kenya | Mangroves, Rivers and Elevation
# ============================================================


# ============================================================
# 1. LOAD PACKAGES AND GENERAL SETTINGS
# ============================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages(
    "pacman",
    repos = "https://cloud.r-project.org"
  )
}

pacman::p_load(
  sf,
  terra,
  ggplot2,
  dplyr,
  ggrepel,
  ggspatial,
  patchwork,
  geodata
)

library(grid)


# ------------------------------------------------------------
# GENERAL SETTINGS
# ------------------------------------------------------------

sf::sf_use_s2(FALSE)

options(
  timeout = 1200
)


# ============================================================
# 2. DIRECTORIES AND INPUT FILES
# ============================================================

data_dir <- "data_2"

output_dir <- "output"

elevation_dir <- file.path(
  data_dir,
  "elevation"
)

dir.create(
  data_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  elevation_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# INPUT DATA
# ------------------------------------------------------------

mangrove_file <- file.path(
  data_dir,
  "kenya_mangroves_2025.gpkg"
)

sample_file <- file.path(
  data_dir,
  "cords.csv"
)

rivers_file <- file.path(
  data_dir,
  "kenya_coast_rivers_v2.gpkg"
)

zip_file <- file.path(
  data_dir,
  "HydroRIVERS_v10_af_shp.zip"
)

local_kenya_shp <- file.path(
  data_dir,
  "gadm41_KEN_0.shp"
)


# ------------------------------------------------------------
# CHECK REQUIRED INPUT FILES
# ------------------------------------------------------------

if (!file.exists(mangrove_file)) {
  stop(
    "Mangrove file not found: ",
    mangrove_file
  )
}

if (!file.exists(sample_file)) {
  stop(
    "Sampling coordinate file not found: ",
    sample_file
  )
}


# ============================================================
# 3. DEFINE MAP EXTENT AND CARTOGRAPHIC SETTINGS
# ============================================================

map_bbox <- c(
  xmin = 39.10,
  xmax = 41.60,
  ymin = -4.80,
  ymax = -1.60
)


# ------------------------------------------------------------
# SPATIAL EXTENTS
# ------------------------------------------------------------

map_extent <- terra::ext(
  map_bbox[["xmin"]],
  map_bbox[["xmax"]],
  map_bbox[["ymin"]],
  map_bbox[["ymax"]]
)

map_bbox_sf <- sf::st_bbox(
  c(
    xmin = map_bbox[["xmin"]],
    ymin = map_bbox[["ymin"]],
    xmax = map_bbox[["xmax"]],
    ymax = map_bbox[["ymax"]]
  ),
  crs = sf::st_crs(4326)
)


# ------------------------------------------------------------
# RIVER DISPLAY THRESHOLD
# ------------------------------------------------------------

# Rivers displayed on the map:
# Mean discharge >= 5 m3/s.
#
# The cached river network retains segments >= 1 m3/s.
# Adjust min_discharge to change the displayed river network
# without downloading or rebuilding the cached dataset.

min_discharge <- 5


# ------------------------------------------------------------
# MAP COLOURS
# ------------------------------------------------------------

sea_col <- "#DCEBF5"

land_col <- "grey96"

mangrove_col <- "#007A33"

mangrove_edge <- "#003D1A"

river_col <- "#1F78B4"

sample_col <- "#D7191C"


# ------------------------------------------------------------
# ELEVATION COLOUR PALETTE
# ------------------------------------------------------------

elevation_colours <- c(
  "#F2F2D0",
  "#DCE8B0",
  "#B6D49B",
  "#E8D58A",
  "#D5A46C",
  "#A87850",
  "#B9B9B5"
)


# ============================================================
# 4. LOAD LAND BOUNDARIES
# ============================================================

message(
  "Loading GADM boundaries..."
)


# ------------------------------------------------------------
# FUNCTION TO LOAD COUNTRY BOUNDARIES
# ------------------------------------------------------------

get_gadm <- function(iso) {
  
  geodata::gadm(
    country = iso,
    level = 0,
    path = data_dir
  ) |>
    sf::st_as_sf() |>
    sf::st_transform(4326) |>
    sf::st_make_valid() |>
    dplyr::select(geometry) |>
    dplyr::mutate(
      iso = iso
    )
  
}


# ------------------------------------------------------------
# LOAD KENYA BOUNDARY
# ------------------------------------------------------------

kenya <- if (file.exists(local_kenya_shp)) {
  
  sf::st_read(
    local_kenya_shp,
    quiet = TRUE
  ) |>
    sf::st_transform(4326) |>
    sf::st_make_valid() |>
    sf::st_geometry() |>
    sf::st_sf() |>
    dplyr::mutate(
      iso = "KEN"
    )
  
} else {
  
  get_gadm("KEN")
  
}


# ------------------------------------------------------------
# LOAD NEIGHBOURING COUNTRIES
# ------------------------------------------------------------

land_countries <- dplyr::bind_rows(
  kenya,
  get_gadm("TZA"),
  get_gadm("SOM")
)


# ------------------------------------------------------------
# DEFINE LAND MASK EXTENT
# ------------------------------------------------------------

land_window_bbox <- sf::st_bbox(
  c(
    xmin = 38.3,
    xmax = 42.2,
    ymin = -5.4,
    ymax = -1.0
  ),
  crs = sf::st_crs(4326)
)


# ------------------------------------------------------------
# CLIP LAND TO THE COASTAL REGION
# ------------------------------------------------------------

land_window <- suppressWarnings(
  sf::st_crop(
    land_countries,
    land_window_bbox
  )
)

if (nrow(land_window) == 0) {
  stop(
    "No GADM land polygons intersect the study area."
  )
}


# ------------------------------------------------------------
# LAND WITHIN THE MAIN MAP EXTENT
# ------------------------------------------------------------

land_study <- suppressWarnings(
  sf::st_crop(
    land_countries,
    map_bbox_sf
  )
)

message(
  "Land boundaries prepared."
)


# ============================================================
# 5. LOAD AND PROCESS MANGROVE DATA
# ============================================================

message(
  "Loading mangrove polygons..."
)


# ------------------------------------------------------------
# READ AND VALIDATE MANGROVE DATA
# ------------------------------------------------------------

mangroves_all <- sf::st_read(
  mangrove_file,
  quiet = TRUE
)

if (is.na(sf::st_crs(mangroves_all))) {
  stop(
    "The mangrove dataset has no defined CRS. ",
    "Assign the correct CRS before continuing."
  )
}

mangroves_all <- mangroves_all |>
  sf::st_transform(4326) |>
  sf::st_make_valid()


# ------------------------------------------------------------
# CROP MANGROVES TO THE STUDY REGION
# ------------------------------------------------------------

mangroves <- suppressWarnings(
  sf::st_crop(
    mangroves_all,
    map_bbox_sf
  )
)

message(
  "Mangrove features retained: ",
  nrow(mangroves)
)


# ============================================================
# 6. DOWNLOAD AND PREPARE HYDRORIVERS
# ============================================================

message(
  "Preparing HydroRIVERS data..."
)


# ------------------------------------------------------------
# LOCATE EXISTING HYDRORIVERS SHAPEFILE
# ------------------------------------------------------------

find_rivers_shp <- function() {
  
  matches <- list.files(
    data_dir,
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


# ------------------------------------------------------------
# LOAD CACHED COASTAL RIVERS OR PREPARE NEW DATA
# ------------------------------------------------------------

if (file.exists(rivers_file)) {
  
  message(
    "Cached coastal rivers found: ",
    rivers_file
  )
  
  rivers <- sf::st_read(
    rivers_file,
    quiet = TRUE
  )
  
  if (is.na(sf::st_crs(rivers))) {
    stop(
      "The cached river file has no CRS: ",
      rivers_file
    )
  }
  
  if (!"DIS_AV_CMS" %in% names(rivers)) {
    stop(
      "Cached river file lacks DIS_AV_CMS. ",
      "Delete the cache and rerun the script."
    )
  }
  
  rivers <- sf::st_transform(
    rivers,
    4326
  )
  
} else {
  
  # ----------------------------------------------------------
  # FIND OR DOWNLOAD HYDRORIVERS
  # ----------------------------------------------------------
  
  shp_path <- find_rivers_shp()
  
  if (is.na(shp_path)) {
    
    if (!file.exists(zip_file)) {
      
      message(
        "Downloading HydroRIVERS Africa data..."
      )
      
      download.file(
        url = paste0(
          "https://data.hydrosheds.org/file/",
          "HydroRIVERS/HydroRIVERS_v10_af_shp.zip"
        ),
        destfile = zip_file,
        mode = "wb",
        quiet = FALSE
      )
      
    } else {
      
      message(
        "HydroRIVERS archive found. ",
        "Skipping download."
      )
      
    }
    
    message(
      "Extracting HydroRIVERS data..."
    )
    
    unzip(
      zip_file,
      exdir = data_dir
    )
    
    shp_path <- find_rivers_shp()
    
  } else {
    
    message(
      "HydroRIVERS shapefile found. ",
      "Skipping download."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # CHECK SHAPEFILE AVAILABILITY
  # ----------------------------------------------------------
  
  if (is.na(shp_path)) {
    stop(
      "HydroRIVERS shapefile could not be located in ",
      data_dir,
      "."
    )
  }
  
  
  # ----------------------------------------------------------
  # READ RIVERS WITHIN A PRELIMINARY BOUNDING BOX
  # ----------------------------------------------------------
  
  message(
    "Reading HydroRIVERS: ",
    shp_path
  )
  
  rivers <- sf::st_read(
    shp_path,
    wkt_filter = sf::st_as_text(
      sf::st_as_sfc(
        sf::st_bbox(
          c(
            xmin = 38.5,
            xmax = 42.0,
            ymin = -5.2,
            ymax = -1.2
          ),
          crs = sf::st_crs(4326)
        )
      )
    ),
    quiet = TRUE
  )
  
  if (nrow(rivers) == 0) {
    stop(
      "No HydroRIVERS features returned ",
      "for the preliminary bounding box."
    )
  }
  
  if (!"DIS_AV_CMS" %in% names(rivers)) {
    stop(
      "DIS_AV_CMS is absent from HydroRIVERS."
    )
  }
  
  
  # ----------------------------------------------------------
  # FILTER AND TRANSFORM THE RIVER NETWORK
  # ----------------------------------------------------------
  
  message(
    "Filtering river network..."
  )
  
  rivers <- rivers |>
    sf::st_transform(4326) |>
    dplyr::filter(
      !is.na(DIS_AV_CMS),
      DIS_AV_CMS >= 1
    ) |>
    dplyr::select(
      DIS_AV_CMS
    )
  
  message(
    "River segments with discharge >= 1 m3/s: ",
    nrow(rivers)
  )
  
  
  # ----------------------------------------------------------
  # CROP RIVERS TO THE STUDY EXTENT
  # ----------------------------------------------------------
  
  message(
    "Clipping rivers to study-area bounding box..."
  )
  
  rivers <- suppressWarnings(
    sf::st_crop(
      rivers,
      map_bbox_sf
    )
  )
  
  rivers <- rivers[
    !sf::st_is_empty(rivers),
    ,
    drop = FALSE
  ]
  
  message(
    "River segments within study area: ",
    nrow(rivers)
  )
  
  if (nrow(rivers) == 0) {
    stop(
      "No river segments remain within ",
      "the study-area bounding box."
    )
  }
  
  
  # ----------------------------------------------------------
  # PREPARE THE COASTAL LAND MASK
  # ----------------------------------------------------------
  
  message(
    "Preparing coastal land mask..."
  )
  
  land_mask <- sf::st_buffer(
    sf::st_geometry(land_window),
    dist = 0.005
  )
  
  land_mask <- sf::st_union(
    land_mask
  )
  
  
  # ----------------------------------------------------------
  # CLIP RIVERS TO COASTAL LAND
  # ----------------------------------------------------------
  
  message(
    "Clipping rivers to coastal land..."
  )
  
  rivers <- suppressWarnings(
    sf::st_intersection(
      rivers,
      land_mask
    )
  )
  
  rivers <- suppressWarnings(
    sf::st_collection_extract(
      rivers,
      "LINESTRING"
    )
  )
  
  rivers <- rivers[
    !sf::st_is_empty(rivers),
    ,
    drop = FALSE
  ]
  
  message(
    "River segments after clipping: ",
    nrow(rivers)
  )
  
  if (nrow(rivers) == 0) {
    stop(
      "No river segments remain after ",
      "clipping to coastal land."
    )
  }
  
  
  # ----------------------------------------------------------
  # SAVE PREPARED RIVER NETWORK
  # ----------------------------------------------------------
  
  sf::st_write(
    rivers,
    rivers_file,
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message(
    "Prepared coastal river network saved: ",
    rivers_file
  )
  
}


# ============================================================
# 7. SELECT MAJOR RIVERS FOR MAPPING
# ============================================================

major_rivers <- rivers |>
  dplyr::filter(
    !is.na(DIS_AV_CMS),
    DIS_AV_CMS >= min_discharge
  )

message(
  "Rivers meeting discharge threshold of ",
  min_discharge,
  " m3/s: ",
  nrow(major_rivers)
)


# ------------------------------------------------------------
# CROP AND VALIDATE RIVER GEOMETRIES
# ------------------------------------------------------------

rivers_map <- suppressWarnings(
  sf::st_crop(
    major_rivers,
    map_bbox_sf
  )
)

rivers_map <- rivers_map[
  as.character(
    sf::st_geometry_type(rivers_map)
  ) %in% c(
    "LINESTRING",
    "MULTILINESTRING"
  ),
  ,
  drop = FALSE
]

message(
  "River segments available for plotting: ",
  nrow(rivers_map)
)

if (nrow(rivers_map) == 0) {
  warning(
    "No river segments are available within ",
    "the map extent. Consider lowering min_discharge."
  )
}


# ============================================================
# 8. LOAD SAMPLING COORDINATES
# ============================================================

message(
  "Loading sampling coordinates..."
)


# ------------------------------------------------------------
# READ COORDINATE TABLE
# ------------------------------------------------------------

coordinates <- read.csv(
  sample_file,
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# VALIDATE REQUIRED COLUMNS
# ------------------------------------------------------------

required_fields <- c(
  "site",
  "latitude",
  "longitude"
)

missing_fields <- setdiff(
  required_fields,
  names(coordinates)
)

if (length(missing_fields) > 0) {
  stop(
    "Sampling file is missing required fields: ",
    paste(
      missing_fields,
      collapse = ", "
    )
  )
}


# ------------------------------------------------------------
# CLEAN SAMPLING COORDINATES
# ------------------------------------------------------------

coordinates <- coordinates |>
  dplyr::mutate(
    site = as.character(site),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) |>
  dplyr::filter(
    !is.na(site),
    !is.na(latitude),
    !is.na(longitude)
  )


# ------------------------------------------------------------
# RETAIN SITES WITHIN THE MAP EXTENT
# ------------------------------------------------------------

inside_extent <- (
  coordinates$longitude >= map_bbox[["xmin"]] &
    coordinates$longitude <= map_bbox[["xmax"]] &
    coordinates$latitude >= map_bbox[["ymin"]] &
    coordinates$latitude <= map_bbox[["ymax"]]
)

coordinates <- coordinates[
  inside_extent,
  ,
  drop = FALSE
]


# ------------------------------------------------------------
# CONVERT COORDINATES TO SPATIAL FEATURES
# ------------------------------------------------------------

samples_sf <- sf::st_as_sf(
  coordinates,
  coords = c(
    "longitude",
    "latitude"
  ),
  crs = 4326,
  remove = FALSE
)

message(
  "Sampling locations retained: ",
  nrow(samples_sf)
)


# ============================================================
# 9. DOWNLOAD AND PREPARE ELEVATION DATA
# ============================================================

message(
  "Preparing elevation rasters..."
)


# ------------------------------------------------------------
# DEFINE COUNTRIES AND RASTER FILES
# ------------------------------------------------------------

elevation_countries <- c(
  "KEN",
  "TZA",
  "SOM"
)

dem_files <- file.path(
  elevation_dir,
  paste0(
    "elevation_",
    elevation_countries,
    ".tif"
  )
)


# ------------------------------------------------------------
# DOWNLOAD MISSING ELEVATION RASTERS
# ------------------------------------------------------------

for (i in seq_along(elevation_countries)) {
  
  if (!file.exists(dem_files[i])) {
    
    message(
      "Downloading elevation data for ",
      elevation_countries[i],
      "..."
    )
    
    downloaded_dem <- geodata::elevation_30s(
      country = elevation_countries[i],
      path = elevation_dir
    )
    
    terra::writeRaster(
      terra::rast(downloaded_dem),
      dem_files[i],
      overwrite = TRUE
    )
    
  } else {
    
    message(
      "Using cached elevation raster: ",
      dem_files[i]
    )
    
  }
  
}


# ------------------------------------------------------------
# LOAD AND MERGE COUNTRY RASTERS
# ------------------------------------------------------------

dem_list <- lapply(
  dem_files,
  terra::rast
)

dem <- terra::merge(
  terra::sprc(dem_list)
)


# ------------------------------------------------------------
# CROP ELEVATION TO THE MAP EXTENT
# ------------------------------------------------------------

dem_crop <- terra::crop(
  dem,
  map_extent,
  snap = "out"
)


# ------------------------------------------------------------
# MASK ELEVATION TO THE LAND BOUNDARIES
# ------------------------------------------------------------

land_vect <- terra::vect(
  sf::st_as_sf(
    sf::st_union(
      sf::st_geometry(land_window)
    )
  )
)

dem_crop <- terra::mask(
  dem_crop,
  land_vect,
  touches = TRUE
)


# ------------------------------------------------------------
# CONVERT RASTER TO A PLOTTING DATA FRAME
# ------------------------------------------------------------

dem_df <- as.data.frame(
  dem_crop,
  xy = TRUE,
  na.rm = TRUE
)

if (nrow(dem_df) == 0) {
  stop(
    "No valid elevation cells remain after ",
    "cropping and masking."
  )
}

names(dem_df)[3] <- "elevation"

dem_df$elevation <- pmax(
  dem_df$elevation,
  0
)

message(
  "Elevation cells available: ",
  nrow(dem_df)
)


# ============================================================
# 10. PREPARE KENYA LOCATION INSET
# ============================================================

message("Preparing Kenya inset...")

inset_xlim <- c(33.5, 42.5)
inset_ylim <- c(-5.5, 5.5)

inset_bbox <- sf::st_bbox(
  c(
    xmin = inset_xlim[1],
    xmax = inset_xlim[2],
    ymin = inset_ylim[1],
    ymax = inset_ylim[2]
  ),
  crs = sf::st_crs(4326)
)


# ------------------------------------------------------------
# LOAD AND SIMPLIFY NEIGHBOURING COUNTRIES
# ------------------------------------------------------------

inset_neighbours <- dplyr::bind_rows(
  lapply(
    c("TZA", "UGA", "SSD", "ETH", "SOM"),
    get_gadm
  )
) |>
  sf::st_transform(3857) |>
  sf::st_simplify(
    dTolerance = 2000,
    preserveTopology = TRUE
  ) |>
  sf::st_transform(4326)


# ------------------------------------------------------------
# CROP NEIGHBOURING COUNTRIES TO THE INSET EXTENT
# ------------------------------------------------------------

inset_neighbours <- suppressWarnings(
  sf::st_crop(
    inset_neighbours,
    inset_bbox
  )
)


# ------------------------------------------------------------
# SIMPLIFY KENYA BOUNDARY
# ------------------------------------------------------------

kenya_inset <- kenya |>
  sf::st_transform(3857) |>
  sf::st_simplify(
    dTolerance = 500,
    preserveTopology = TRUE
  ) |>
  sf::st_transform(4326)


# ------------------------------------------------------------
# DEFINE STUDY-AREA RECTANGLE
# ------------------------------------------------------------

study_rectangle <- sf::st_sf(
  geometry = sf::st_as_sfc(map_bbox_sf),
  crs = 4326
)

# ------------------------------------------------------------
# BUILD KENYA INSET MAP
# ------------------------------------------------------------

inset_map <- ggplot() +
  
  geom_sf(
    data = inset_neighbours,
    fill = "grey93",
    colour = "grey55",
    linewidth = 0.25
  ) +
  
  geom_sf(
    data = kenya_inset,
    fill = "grey86",
    colour = "grey25",
    linewidth = 0.35
  ) +
  
  geom_sf(
    data = mangroves_all,
    fill = mangrove_col,
    colour = mangrove_col,
    linewidth = 0.1
  ) +
  
  geom_sf(
    data = study_rectangle,
    fill = NA,
    colour = sample_col,
    linewidth = 0.7
  ) +
  
  coord_sf(
    xlim = inset_xlim,
    ylim = inset_ylim,
    expand = FALSE
  ) +
  
  theme_void() +
  
  theme(
    panel.background = element_rect(
      fill = sea_col,
      colour = "grey35",
      linewidth = 0.35
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.margin = margin(
      2,
      2,
      2,
      2
    )
  )


# ============================================================
# 11. BUILD THE MAIN COASTAL MAP
# ============================================================

message(
  "Building the main map..."
)


# ------------------------------------------------------------
# ELEVATION BACKGROUND
# ------------------------------------------------------------

main_map <- ggplot() +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = sea_col
  ) +
  
  geom_raster(
    data = dem_df,
    aes(
      x = x,
      y = y,
      fill = elevation
    ),
    interpolate = TRUE
  ) +
  
  scale_fill_gradientn(
    colours = elevation_colours,
    name = "Elevation (m)",
    na.value = sea_col,
    guide = guide_colorbar(
      barheight = unit(
        4.0,
        "cm"
      ),
      barwidth = unit(
        0.35,
        "cm"
      ),
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "grey40",
      ticks.colour = "grey30"
    )
  )


# ------------------------------------------------------------
# LAND BOUNDARIES AND COASTLINE
# ------------------------------------------------------------

main_map <- main_map +
  
  geom_sf(
    data = land_study,
    fill = NA,
    colour = "grey40",
    linewidth = 0.15,
    inherit.aes = FALSE
  )


# ------------------------------------------------------------
# RIVER NETWORK
# ------------------------------------------------------------

if (nrow(rivers_map) > 0) {
  
  main_map <- main_map +
    
    geom_sf(
      data = rivers_map,
      aes(
        linewidth = DIS_AV_CMS
      ),
      colour = river_col,
      alpha = 0.95,
      lineend = "round",
      inherit.aes = FALSE
    ) +
    
    scale_linewidth(
      range = c(
        0.3,
        1.2
      ),
      trans = "sqrt",
      guide = "none"
    )
  
}


# ------------------------------------------------------------
# MANGROVE DISTRIBUTION
# ------------------------------------------------------------

if (nrow(mangroves) > 0) {
  
  main_map <- main_map +
    
    geom_sf(
      data = mangroves,
      fill = mangrove_col,
      colour = mangrove_edge,
      linewidth = 0.12,
      alpha = 0.95,
      inherit.aes = FALSE
    )
  
}


# ------------------------------------------------------------
# SAMPLING LOCATIONS
# ------------------------------------------------------------

if (nrow(samples_sf) > 0) {
  
  main_map <- main_map +
    
    geom_sf(
      data = samples_sf,
      shape = 16,
      size = 2.5,
      colour = sample_col,
      inherit.aes = FALSE
    )
  
}


# ------------------------------------------------------------
# SAMPLING SITE LABELS
# ------------------------------------------------------------

if (nrow(coordinates) > 0) {
  
  main_map <- main_map +
    
    ggrepel::geom_label_repel(
      data = coordinates,
      aes(
        x = longitude,
        y = latitude,
        label = site
      ),
      size = 3.0,
      colour = "black",
      fill = "white",
      alpha = 0.9,
      label.size = 0.15,
      label.padding = unit(
        0.12,
        "lines"
      ),
      box.padding = 0.35,
      point.padding = 0.25,
      min.segment.length = 0,
      segment.colour = "grey35",
      segment.linewidth = 0.25,
      max.overlaps = Inf,
      seed = 123
    )
  
}


# ------------------------------------------------------------
# NORTH ARROW AND SCALE BAR
# ------------------------------------------------------------

main_map <- main_map +
  
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(
      0.9,
      "cm"
    ),
    width = unit(
      0.9,
      "cm"
    )
  ) +
  
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.20,
    unit_category = "metric",
    style = "bar",
    bar_cols = c(
      "black",
      "white"
    ),
    text_cex = 0.7,
    line_width = 0.5
  )


# ------------------------------------------------------------
# MAP EXTENT, AXES AND THEME
# ------------------------------------------------------------

main_map <- main_map +
  
  coord_sf(
    crs = sf::st_crs(4326),
    xlim = c(
      map_bbox[["xmin"]],
      map_bbox[["xmax"]]
    ),
    ylim = c(
      map_bbox[["ymin"]],
      map_bbox[["ymax"]]
    ),
    expand = FALSE
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_bw(
    base_size = 10
  ) +
  
  theme(
    panel.background = element_rect(
      fill = sea_col,
      colour = NA
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      colour = "grey35",
      fill = NA,
      linewidth = 0.45
    ),
    axis.title = element_text(
      size = 9
    ),
    axis.text = element_text(
      size = 8,
      colour = "grey20"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 9,
      face = "bold"
    ),
    legend.text = element_text(
      size = 8
    ),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    ),
    legend.box.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.margin = margin(
      5,
      5,
      5,
      5
    )
  )


# ============================================================
# 12. CREATE THE CUSTOM MAP LEGEND
# ============================================================

legend_map <- ggplot() +
  
  # Ocean
  annotate(
    "rect",
    xmin = 0.08,
    xmax = 0.22,
    ymin = 0.78,
    ymax = 0.90,
    fill = sea_col,
    colour = "grey50",
    linewidth = 0.25
  ) +
  
  annotate(
    "text",
    x = 0.29,
    y = 0.84,
    label = "Ocean",
    hjust = 0,
    size = 3.0
  ) +
  
  # Land
  annotate(
    "rect",
    xmin = 0.08,
    xmax = 0.22,
    ymin = 0.60,
    ymax = 0.72,
    fill = land_col,
    colour = "grey50",
    linewidth = 0.25
  ) +
  
  annotate(
    "text",
    x = 0.29,
    y = 0.66,
    label = "Land",
    hjust = 0,
    size = 3.0
  ) +
  
  # Major rivers
  annotate(
    "segment",
    x = 0.08,
    xend = 0.22,
    y = 0.48,
    yend = 0.48,
    colour = river_col,
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = 0.29,
    y = 0.48,
    label = "Major rivers",
    hjust = 0,
    size = 3.0
  ) +
  
  # Mangroves
  annotate(
    "rect",
    xmin = 0.08,
    xmax = 0.22,
    ymin = 0.30,
    ymax = 0.42,
    fill = mangrove_col,
    colour = mangrove_edge,
    linewidth = 0.25
  ) +
  
  annotate(
    "text",
    x = 0.29,
    y = 0.36,
    label = "Mangroves",
    hjust = 0,
    size = 3.0
  ) +
  
  # Sampling sites
  annotate(
    "point",
    x = 0.15,
    y = 0.18,
    shape = 16,
    size = 2.8,
    colour = sample_col
  ) +
  
  annotate(
    "text",
    x = 0.29,
    y = 0.18,
    label = "Sampling site",
    hjust = 0,
    size = 3.0
  ) +
  
  coord_cartesian(
    xlim = c(
      0,
      1
    ),
    ylim = c(
      0.08,
      1
    ),
    expand = FALSE,
    clip = "off"
  ) +
  
  theme_void() +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = "grey55",
      linewidth = 0.3
    ),
    plot.margin = margin(
      3,
      3,
      3,
      3
    )
  )


# ============================================================
# 13. ASSEMBLE THE FINAL MAP
# ============================================================

final_map <- main_map +
  
  patchwork::inset_element(
    inset_map,
    left = 0.015,
    bottom = 0.60,
    right = 0.34,
    top = 0.985,
    align_to = "panel"
  ) +
  
  patchwork::inset_element(
    legend_map,
    left = 0.72,
    bottom = 0.15,
    right = 0.985,
    top = 0.40,
    align_to = "panel"
  )