
# ============================================================
# COASTAL KENYA MANGROVE MAP WITH KENYA INSET
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(patchwork)


# ------------------------------------------------------------
# 1. Define coastal study extent
# ------------------------------------------------------------

coast_bbox <- c(
  xmin = 39.0,
  xmax = 41.5,
  ymin = -4.8,
  ymax = -1.5
)


# ------------------------------------------------------------
# 2. Create Kenya coastline
# ------------------------------------------------------------

kenya_coastline <- ne_coastline(
  scale = "medium",
  returnclass = "sf"
)

kenya_coastline <- st_transform(
  kenya_coastline,
  4326
)


# ------------------------------------------------------------
# 3. Crop mangroves to coastal study region
# ------------------------------------------------------------

coastal_mangrove <- st_crop(
  kenya_mangrove,
  st_bbox(coast_bbox)
)


# ------------------------------------------------------------
# 4. Crop coastline to coastal study region
# ------------------------------------------------------------

coastal_coastline <- st_crop(
  kenya_coastline,
  st_bbox(coast_bbox)
)


# ------------------------------------------------------------
# 5. Prepare sampling labels
# ------------------------------------------------------------

samples_labels <- samples_sf |>
  st_coordinates() |>
  as.data.frame() |>
  bind_cols(
    samples_sf |>
      st_drop_geometry() |>
      select(site)
  )


# ============================================================
# MAIN COASTAL MAP
# ============================================================

main_map <- ggplot() +
  
  # Coastal land
  geom_sf(
    data = kenya,
    fill = "grey95",
    colour = NA
  ) +
  
  # Coastline
  geom_sf(
    data = coastal_coastline,
    colour = "grey30",
    linewidth = 0.5
  ) +
  
  # Mangrove extent
  geom_sf(
    data = coastal_mangrove,
    fill = "darkgreen",
    colour = NA,
    alpha = 0.85
  ) +
  
  # Sampling sites
  geom_sf(
    data = samples_sf,
    shape = 21,
    size = 3.5,
    fill = "white",
    colour = "red",
    stroke = 1
  ) +
  
  # Site labels
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
    
    axis.title = element_text(
      size = 10
    ),
    
    axis.text = element_text(
      size = 9
    ),
    
    plot.margin = margin(
      5, 5, 5, 5
    )
  )


# ============================================================
# KENYA INSET MAP
# ============================================================

# Create rectangle showing coastal study region
study_box <- st_as_sfc(
  st_bbox(
    coast_bbox,
    crs = st_crs(4326)
  )
)


inset_map <- ggplot() +
  
  # Kenya
  geom_sf(
    data = kenya,
    fill = "grey95",
    colour = "grey30",
    linewidth = 0.4
  ) +
  
  # Coastal mangrove distribution
  geom_sf(
    data = kenya_mangrove,
    fill = "darkgreen",
    colour = NA,
    alpha = 1
  ) +
  
  # Coastal study area
  geom_sf(
    data = study_box,
    fill = NA,
    colour = "red",
    linewidth = 0.1
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
    plot.margin = margin(
      2, 2, 2, 2
    )
  )


# ============================================================
# COMBINE MAIN MAP + INSET
# ============================================================

final_map <- main_map +
  inset_element(
    inset_map,
    left = 0.68,
    bottom = 0.62,
    right = 0.98,
    top = 0.98
  )


# ------------------------------------------------------------
# Display final map
# ------------------------------------------------------------

print(final_map)


# ------------------------------------------------------------
# Save high-resolution PNG
# ------------------------------------------------------------

ggsave(
  "output/coastal_kenya_mangrove_sampling_map.png",
  final_map,
  width = 9,
  height = 7,
  units = "in",
  dpi = 600
)


# ------------------------------------------------------------
# Save PDF
# ------------------------------------------------------------

ggsave(
  "output/coastal_kenya_mangrove_sampling_map.pdf",
  final_map,
  width = 9,
  height = 7,
  units = "in"
)


# ============================================================
# END
# ============================================================


### What the resulting figure will contain

**Main map**
  
  * Coastal Kenya only
* One coastline line
* GMW mangrove extent in green
* Six sampling sites
* Non-overlapping site labels
* No county boundaries
* No unnecessary inland Kenya detail

**Inset**
  
  * Full Kenya outline
* Kenya's mangrove distribution
* A red rectangle showing exactly where the main coastal map is located

One small adjustment may be needed after you see the result: **the inset position** (`left = 0.68`, `bottom = 0.62`, etc.) can be moved depending on where the mangrove coastline and labels fall. The important part is that the inset rectangle will clearly identify the coastal region shown in the main panel.
