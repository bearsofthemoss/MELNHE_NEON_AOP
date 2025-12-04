## 4 x 1 pattern.

### top left:  WREF boundary
### top right, 4 plot RGB with outline plot and 30x30
### bottom left: select plot with CHM showing tree crowns
### bottom right: select plot with shade / non-shade and pixel ID (i.e. in / out)


library(ggplot2)
library(sf)
library(units)
library(here)
library(tidyr)
library(remotes)
#install_github("cran/ggsn")
#library(ggsn)
library(ggrepel)
library(dplyr)
library(terra)
library(tidyterra)
library(rhdf5)

library(ggrepel)
library(maps)
library(cowplot)  # for combining plots


### Bartlett shapefile

bart <- sf::read_sf("D:/Users/bears/Downloads/S_USA.Experimental_Area_Boundaries/S_USA.Experimental_Area_Boundaries.shp") 
bart <- bart[bart$NAME=="Bartlett Experimental Forest",]
ba <- st_union(bart)

ba <- st_transform(ba, crs=4326)


## stakes
stands <- st_read(file.path("D:/Users/bears/Downloads/Intensive_Bartlett_GIS/Bartlett_intensive_sites.shp"))


subp <- st_read(file.path("D:/Users/bears/Downloads/Intensive_Bartlett_GIS/Bartlett_intensive_sites_subplots.shp"))
subp <- subp[subp$Site=="C9",]
subp <- st_transform(subp, crs=4326)


# Transform and assign treatments
stands <- st_transform(stands, crs = 4326)
stands[stands$Plot == "1", "Treatment"] <- "Control"
stands[stands$Plot == "2", "Treatment"] <- "P"
stands[stands$Plot == "3", "Treatment"] <- "N+P"
stands[stands$Plot == "4", "Treatment"] <- "N"
stands$Treatment <- factor(stands$Treatment, 
                           levels = c("Control", "N", "P", "N+P"))

# Create stand centroids and assign ages
stand_centroids <- stands %>%
  group_by(Site) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid()

# Assign ages to sites
age_assignments <- list(
  "Young" = c("C1", "C2", "C3"),
  "Mid-aged" = c("C4", "C5", "C6"),
  "Mature" = c("C7", "C8", "C9")
)

for (age in names(age_assignments)) {
  stand_centroids[stand_centroids$Site %in% age_assignments[[age]], "Age"] <- age
}

stand_centroids$Age <- factor(stand_centroids$Age, 
                              levels = c("Young", "Mid-aged", "Mature"))

# -----------------------------------------------------------------------------
# Panel 1: Inset Map (North America context)
# -----------------------------------------------------------------------------

library(rnaturalearth)
library(rnaturalearthdata)

study_site_lon <- -71.5  # Replace with actual coordinates
study_site_lat <- 43.8

north_america <- ne_countries(scale = "medium", continent = "North America", 
                              returnclass = "sf")
states_provinces <- ne_states(country = c("United States of America", "Canada"), 
                              returnclass = "sf")

inset_map <- ggplot() +
  geom_sf(data = north_america, fill = "lightgray", color = "white", size = 0.3) +
  geom_sf(data = states_provinces, fill = NA, color = "white", size = 0.2) +
  geom_point(aes(x = study_site_lon, y = study_site_lat), 
             color = "red", size = 5, shape = 8) +
  coord_sf(xlim = c(-86, -60), ylim = c(35, 50), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.border = element_rect(fill = NA, color = "black", size = 1)
  )
inset_map
# -----------------------------------------------------------------------------
# Panel 2: Site Overview Map
# -----------------------------------------------------------------------------

g1 <- ggplot() + 
  geom_sf(data = ba, fill = "lightgreen") +
  geom_sf(data = stand_centroids, aes(fill = Age, shape = Age), size = 4) +
  scale_fill_manual(values = c("Young" = "#E6AB02", 
                               "Mid-aged" = "#666666", 
                               "Mature" = "#D95F02")) +
  scale_shape_manual(values = c("Young" = 21, "Mid-aged" = 22, "Mature" = 24)) +
  geom_text_repel(data = stand_centroids, 
                  aes(x = st_coordinates(stand_centroids)[, 1], 
                      y = st_coordinates(stand_centroids)[, 2],
                      label = Site)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(
    fill = guide_legend(
      title = "Forest age",
      title.position = "top",
      title.hjust = 0.5,
      ncol = 1
    ),
    shape = guide_legend(
      title = "Forest age",
      title.position = "top", 
      title.hjust = 0.5,
      ncol = 1
    )
  ) +
  labs(x = "", y = "")

# -----------------------------------------------------------------------------
# Panel 3: CHM with Treatment Plots
# -----------------------------------------------------------------------------

# Load and process CHM data
lidar_path <- here::here("data_folder", "DP3.30015.001", "neon-aop-products",
                         "2019", "FullSite", "D01", "2019_BART_5", 
                         "L3", "DiscreteLidar", "CanopyHeightModelGtif")

chm.C2a <- rast(file.path(lidar_path, "NEON_D01_BART_DP3_318000_4881000_CHM.tif"))
chm.C2b <- rast(file.path(lidar_path, "NEON_D01_BART_DP3_318000_4880000_CHM.tif"))
chm.C2 <- terra::merge(chm.C2a, chm.C2b)

C2 <- st_transform(stands[stands$Site == "C2", ], st_crs(chm.C2))
C2_buffer <- st_buffer(C2, dist = 70)
chm_c2 <- terra::crop(chm.C2, C2_buffer)

chm_c2_df <- as.data.frame(chm_c2, xy = TRUE)
names(chm_c2_df) <- c("x", "y", "height")

g2 <- ggplot() +
  geom_raster(data = chm_c2_df, aes(x = x, y = y, fill = height)) +
  scale_fill_gradientn(colors = rev(terrain.colors(100)), 
                       name = "Height (m)",
                       na.value = "transparent",
                       guide = guide_colorbar(
                         title = "Height (m)",
                         title.position = "top",
                         title.hjust = 0.5,
                         barwidth = 10,
                         barheight = 0.5,
                         order = 1
                       )) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = C2, aes(fill = Treatment, col = Treatment), 
          alpha = 0.4, linewidth = 2) +
  scale_fill_manual(values = c("Control" = "black", "N" = "blue", 
                               "P" = "red", "N+P" = "purple")) +
  scale_color_manual(values = c("Control" = "black", "N" = "blue", 
                                "P" = "red", "N+P" = "purple")) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  guides(
    fill = guide_legend(
      title = "Treatment",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      override.aes = list(alpha = 1),
      order = 2
    ),
    color = guide_legend(
      title = "Treatment",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      order = 2
    )
  ) +
  coord_sf(xlim = c(min(chm_c2_df$x), max(chm_c2_df$x)),
           ylim = c(min(chm_c2_df$y), max(chm_c2_df$y)),
           expand = FALSE) +
  labs(x = "", y = "")

# -----------------------------------------------------------------------------
# Panel 4: RGB with Tree Tops and Shade Analysis
# -----------------------------------------------------------------------------

rgb_path <- here::here("data_folder", "DP3.30010.001", "neon-aop-products",
                       "2019", "FullSite", "D01", "2019_BART_5", 
                       "L3", "Camera", "Mosaic")

chm.C7 <- terra::rast(file.path(lidar_path, "NEON_D01_BART_DP3_315000_4880000_CHM.tif"))
pic_C7 <- terra::rast(file.path(rgb_path, "2019_BART_5_315000_4880000_image.tif"))

single_plot <- stands[stands$Site == "C7" & stands$Plot == 2, ]
single_plot <- st_transform(single_plot, st_crs(pic_C7))
plot_buffer <- st_buffer(single_plot, dist = 5)

rgb_crop <- terra::crop(pic_C7, plot_buffer)
rgb_df <- as.data.frame(rgb_crop, xy = TRUE)
names(rgb_df) <- c("x", "y", "red", "green", "blue")
rgb_df$rgb <- rgb(rgb_df$red/255, rgb_df$green/255, rgb_df$blue/255)

# Tree detection
lin.C <- function(x) {x * 0.02}
m7c <- crop(chm.C7, single_plot, mask = TRUE)
m7c_raster <- raster::raster(m7c)
m7ctops <- ForestTools::vwf(CHM = m7c, winFun = lin.C, minHeight = 5)
m7ctops_sf <- st_as_sf(m7ctops)

# Shade analysis
dsm_path <- here::here("data_folder", "DP3.30024.001", "neon-aop-products",
                       "2019", "FullSite", "D01", "2019_BART_5", 
                       "L3", "DiscreteLidar", "DSMGtif")
h5_path <- here::here("data_folder", "DP3.30006.001", "neon-aop-products",
                      "2019", "FullSite", "D01", "2019_BART_5", 
                      "L3", "Spectrometer", "Reflectance")

dsm <- terra::rast(file.path(dsm_path, "NEON_D01_BART_DP3_315000_4880000_DSM.tif"))
dsm_slope <- terra::terrain(dsm, v = "slope")
dsm_aspect <- terra::terrain(dsm, v = "aspect")

i_h5 <- file.path(h5_path, "NEON_D01_BART_DP3_315000_4880000_reflectance.h5")
ii <- h5ls(file = i_h5)

# Extract solar angles
d_nam <- paste(ii[grep("Solar_Zenith", ii$name), ]$group, 
               ii[grep("Solar_Zenith", ii$name), ]$name, sep = "/")
zenith <- mean(sapply(d_nam, function(d) h5read(i_h5, d)))
h5closeAll()

d_nam <- paste(ii[grep("Solar_Azim", ii$name), ]$group, 
               ii[grep("Solar_Azim", ii$name), ]$name, sep = "/")
azimuth <- mean(sapply(d_nam, function(d) h5read(i_h5, d)))
h5closeAll()

dsm_shade <- terra::shade(dsm_slope, dsm_aspect, angle = zenith, direction = azimuth)
coords_matrix <- st_coordinates(m7ctops_sf)
tree_shade_values <- terra::extract(dsm_shade, coords_matrix)
m7ctops_sf$shade_intensity <- tree_shade_values$hillshade

g4 <- ggplot() +
  geom_raster(data = rgb_df, aes(x = x, y = y), fill = rgb_df$rgb) +
  geom_sf(data = m7ctops_sf, aes(col = shade_intensity), size = 3) +
  scale_color_viridis_c(name = "Shade intensity", option = "plasma") +
  geom_sf(data = single_plot, col = "black", fill = NA, linewidth = 3) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  guides(
    color = guide_colorbar(
      title = "Shade intensity",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 10,
      barheight = 0.5
    )
  ) +
  coord_sf(xlim = c(min(rgb_df$x), max(rgb_df$x)),
           ylim = c(min(rgb_df$y), max(rgb_df$y)),
           expand = FALSE) +
  labs(x = "", y = "")

# =============================================================================
# LAYOUT OPTIONS
# =============================================================================

# -----------------------------------------------------------------------------
# OPTION 1: Using patchwork with equal widths
# -----------------------------------------------------------------------------
library(patchwork)

option1 <- inset_map + g1 + g2 + g4 + 
  plot_layout(ncol = 4, widths = c(1, 1, 1, 1)) +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')+
  theme(plot.margin = margin(1, 1, 1, 1))

option1

ggsave("figure_1.png", option1, 
       width = 16, height = 4.5, dpi = 300, bg = "white")
