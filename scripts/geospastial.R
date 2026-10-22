 install.packages(c("sf","ggplot2","rnaturalearth","rnaturalearthdata","ggspatial","cowplot"))
library(sf)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)   # north arrow + scale bar
library(cowplot)     # for inset if needed
 
 library(rnaturalearth)
 library(rnaturalearthdata)
 library(ggspatial)
 library(sf)
 library(ggplot2)


 # Your site table (assuming you have Site, Lat, Long)
 sites <- read.csv("data/sites.csv.csv", stringsAsFactors=FALSE)
 
 sites_sf <- st_as_sf(sites,
                      coords = c("Long","Lat"),
                      crs = 4326,
                      remove = FALSE)
 
 
 # Get basemap layers
 kenya <- ne_countries(scale = "medium", country = "Kenya", returnclass = "sf")
 lakes <- ne_download(scale = 10, type = "lakes", category = "physical", returnclass = "sf")
 rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
 
 # Filter lakes/rivers to your extent
 lakes <- st_crop(lakes, st_bbox(kenya))
 rivers <- st_crop(rivers, st_bbox(kenya))
 
 ggplot() +
   geom_sf(data = kenya, fill = "grey95", color = "grey100") +
   geom_sf(data = lakes, fill = "#a6cee3", color = NA, alpha = 0.7) +
   geom_sf(data = rivers, color = "#1f78b4", size = 0.4, alpha = 0.6) +
   geom_sf(data = mangrove, fill = "#66c2a5", color = NA, alpha = 0.6) +
   geom_sf(data = sites_sf, aes(color = Site), size = 3) +
   geom_text(data = sites, aes(x = Long, y = Lat, label = Site),
             nudge_y = 0.03, size = 3) +
   annotation_scale(location = "bl", width_hint = 0.25) +
   annotation_north_arrow(location = "bl", which_north = "true",
                          pad_x = unit(0.1, "in"), pad_y = unit(0.4, "in"),
                          style = north_arrow_fancy_orienteering) +
   coord_sf(xlim = c(38.5, 41.0), ylim = c(-5.5, -2.0)) +
   theme_minimal(base_size = 14) +
   labs(title = "Study Sites & Mangrove Cover – Kenya Coast",
        x = "Longitude", y = "Latitude",
        color = "Site")
 
 
 # Kenya boundary
 kenya <- ne_countries(country="Kenya", scale="medium", returnclass="sf")
 
 # Mangrove layer
 mangrove <- st_read("data/Clipped_Mangroves_2020 Modified/Clipped_Mangroves_2020.shp")
 
 
 ggplot() +
   geom_sf(data = kenya, fill="grey95", color="grey60") +
   geom_sf(data = mangrove, fill = "#66c2a5", color = NA, alpha = 0.6) +   # mangroves in greenish
   geom_sf(data = sites_sf, aes(color = Site), size = 3) +
   geom_text(data = sites, aes(x = Long, y = Lat, label = Site),
             nudge_y = 0.03, size = 3) +
   annotation_scale(location = "bl", width_hint = 0.25) +
   annotation_north_arrow(location = "bl", which_north = "true",
                          pad_x = unit(0.1, "in"), pad_y = unit(0.4, "in"),
                          style = north_arrow_fancy_orienteering) +
   coord_sf(xlim = c(38.5, 41.0), ylim = c(-5.5, -2.0)) +   # adjust to Kenya coast region
   theme_minimal(base_size = 14) +
   labs(title = "Study Sites & Mangrove Cover – Kenya Coast",
        x = "Longitude", y = "Latitude",
        color = "Site")
 
 

# Optionally crop to extent around your sites for performance
bbox <- st_bbox(st_buffer(st_union(sites_sf), dist = 3)) # buffer in degrees
map_poly <- st_crop(world, bbox)

# --- Plot
p <- ggplot() +
  geom_sf(data = map_poly, fill = "grey95", color = "grey60") +
  geom_sf(data = sites_sf, aes(color = Site, shape = Site), size = 3) +
  geom_text(data = sites, aes(x = Long, y = Lat, label = Site),
            nudge_y = 0.05, size = 3) +
  annotation_scale(location = "bl", width_hint = 0.25) +   # scale bar
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_minimal(base_size = 14) +
  labs(x = "Longitude", y = "Latitude", color = "Site")

print(p)

library(osmdata)
library(sf)

# Define bounding box for Kenya coast
bbox <- c(38.5, -5.5, 41.0, -2.0)

# Query OSM water polygons
water <- opq(bbox = bbox) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

water_sf <- water$osm_polygons

geom_sf(data = lakes, fill = "#a6cee3", color = NA, alpha = 0.6) +
  geom_sf(data = water_sf, fill = "#74add1", color = NA, alpha = 0.4)


# Plot with your data
ggplot() +
  geom_sf(data = water_sf, fill = "#a6cee3", color = NA, alpha = 0.6) +
  geom_sf(data = kenya, fill = "grey95", color = "grey60") +
  geom_sf(data = mangrove, fill = "#66c2a5", color = NA, alpha = 0.6) +
  geom_sf(data = sites_sf, aes(color = Site), size = 3) +
  geom_text(data = sites, aes(x = Long, y = Lat, label = Site),
            nudge_y = 0.03, size = 3) +
  coord_sf(xlim = c(38.5, 41.0), ylim = c(-5.5, -2.0)) +
  theme_minimal(base_size = 14)

ggplot() +
  geom_sf(data = lakes, fill = "#a6cee3", color = NA, alpha = 0.6) +
  geom_sf(data = water_sf, fill = "#74add1", color = NA, alpha = 0.4) +
  geom_sf(data = kenya, fill = "grey95", color = "grey60") +
  geom_sf(data = mangrove, fill = "#66c2a5", color = NA, alpha = 0.6) +
  geom_sf(data = sites_sf, aes(color = Site), size = 3) +
  geom_text(data = sites, aes(x = Long, y = Lat, label = Site),
            nudge_y = 0.03, size = 3) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.4, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(38.5, 41.0), ylim = c(-5.5, -2.0)) +
  theme_minimal(base_size = 14) +
  labs(title = "Study Sites, Mangroves, and Water Bodies – Kenya Coast",
       x = "Longitude", y = "Latitude",
       color = "Site")


# Load libraries
library(ggplot2)
library(dplyr)
library(forcats)  # for fct_reorder

# Load libraries
library(ggplot2)
library(dplyr)

# Read your RSCU data
rscu_data <- read.table(text = "
AA Codon Count RSCU
Glu GAG 218 0.45
Asn AAT 660 1.56
His CAC 115 0.52
Ser TCC 192 0.9
Ala GCA 285 1.07
Thr ACC 171 0.74
Pro CCT 307 1.66
Phe TTT 676 1.39
Ala GCC 182 0.68
Thr ACA 289 1.26
Pro CCG 96 0.52
Phe TTC 297 0.61
Ala GCG 130 0.49
Thr ACG 90 0.39
Pro CCA 206 1.11
Leu TTG 369 1.19
Ala GCT 470 1.76
Thr ACT 371 1.61
Pro CCC 131 0.71
Leu TTA 646 2.09
Val GTA 386 1.55
Ile ATC 285 0.54
Leu CTA 246 0.8
Ter TAG 12 0.75
Val GTC 109 0.44
Ile ATA 486 0.93
Leu CTT 375 1.21
Ter TGA 10 0.62
Val GTG 139 0.56
Ile ATT 805 1.53
Leu CTC 105 0.34
Ter TAA 26 1.62
Val GTT 361 1.45
Met ATG 417 1.0
Leu CTG 114 0.37
", header=TRUE)

# Order codons within each amino acid by RSCU (optional)
rscu_data <- rscu_data %>%
  group_by(AA) %>%
  arrange(desc(RSCU)) %>%
  ungroup() %>%
  mutate(Codon = factor(Codon, levels=unique(Codon)))

# Stacked bar plot
ggplot(rscu_data, aes(x=AA, y=RSCU, fill=Codon)) +
  geom_bar(stat="identity") +
  theme_classic(base_size = 14) +
  ylab("RSCU") +
  xlab("Amino Acid") +
  ggtitle("Stacked RSCU by Amino Acid") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_brewer(palette="Paired")

library(circlize)

# Set factor levels so codons are grouped by amino acid
rscu_data$Codon <- factor(rscu_data$Codon, levels = rscu_data$Codon)

library(circlize)

circos.clear()
circos.par(start.degree = 90, gap.degree = 2)

circos.initialize(factors = rscu_data$AA, x = rscu_data$pos)

circos.par(cell.padding = c(0, 0, 0, 0))

circos.track(rscu_data$pos, rscu_data$RSCU,
             panel.fun = function(x, y) {
               sector.index <- get.cell.meta.data("sector.index")
               codons_in_sector <- rscu_data$pos[rscu_data$AA == sector.index]
               values_in_sector <- rscu_data$RSCU[rscu_data$AA == sector.index]
               circos.barplot(values_in_sector, codons_in_sector,
                              col = rainbow(length(codons_in_sector)))
             },
             ylim = c(0, max(rscu_data$RSCU)*1.1))


# Initialize sectors (codons)
circos.initialize(factors = rscu_data$AA, x = rscu_data$Codon)

# Create bars for RSCU
circos.trackPlotRegion(factors = rscu_data$AA, y = rscu_data$RSCU,
                       panel.fun = function(x, y) {
                         sector.index <- get.cell.meta.data("sector.index")
                         codons_in_sector <- rscu_data$Codon[rscu_data$AA == sector.index]
                         rscu_values <- rscu_data$RSCU[rscu_data$AA == sector.index]
                         circos.barplot(rscu_values, codons_in_sector, col=rainbow(length(codons_in_sector)))
                       },
                       ylim = c(0, max(rscu_data$RSCU) * 1.1))

# Add axis labels
circos.trackText(rscu_data$AA, rep(max(rscu_data$RSCU)*1.1, nrow(rscu_data)),
                 labels = rscu_data$AA, facing = "clockwise", niceFacing = TRUE)

