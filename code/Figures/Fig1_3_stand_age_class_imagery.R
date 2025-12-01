 


stands <- st_transform(stands, crs=4326)
stands[stands$Plot=="1","Treatment"] <- "Control"
stands[stands$Plot=="2","Treatment"] <- "P"
stands[stands$Plot=="3","Treatment"] <- "N+P"
stands[stands$Plot=="4","Treatment"] <- "N"
stands$Treatment <- factor(stands$Treatment, levels=c("Control","N","P","N+P"))

stand_centroids <- stands %>%
  group_by(Site) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid()


stand_centroids[stand_centroids$Site=="C1" ,"Age"] <- "Young"
stand_centroids[stand_centroids$Site=="C2" ,"Age"] <- "Young"
stand_centroids[stand_centroids$Site=="C3" ,"Age"] <- "Young"
stand_centroids[stand_centroids$Site=="C4" ,"Age"] <- "Mid-aged"
stand_centroids[stand_centroids$Site=="C5" ,"Age"] <- "Mid-aged"
stand_centroids[stand_centroids$Site=="C6" ,"Age"] <- "Mid-aged"
stand_centroids[stand_centroids$Site=="C7" ,"Age"] <- "Mature"
stand_centroids[stand_centroids$Site=="C8" ,"Age"] <- "Mature"
stand_centroids[stand_centroids$Site=="C9" ,"Age"] <- "Mature"


stand_centroids$Age <- factor(stand_centroids$Age, 
                              levels = c("Young", "Mid-aged", "Mature"))
#####

# 2nd figure

g1 <- ggplot() + 
  geom_sf(data = ba, fill = "lightgreen") +
  geom_sf(data = stand_centroids, aes(fill = Age, shape = Age), size = 4) +
  scale_fill_manual(values = c("Young" = "#E6AB02", "Mid-aged" = "#666666", "Mature" = "#D95F02")) +
  scale_shape_manual(values = c("Young" = 21, "Mid-aged" = 22, "Mature" = 24)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 16)
  ) +
  guides(
    fill = guide_legend(
      title = "Forest age",
      title.position = "top",
      title.hjust = 0.5,
      ncol = 1,
      byrow = TRUE
    ),
    shape = guide_legend(
      title = "Forest age",
      title.position = "top", 
      title.hjust = 0.5,
      ncol = 1,
      byrow = TRUE
    )
  ) +
  geom_text_repel(data = stand_centroids, 
                  aes(x = st_coordinates(stand_centroids)[, 1], 
                      y = st_coordinates(stand_centroids)[, 2],
                      label = Site)) +
  labs(x = "", y = "")
g1

# Create inset map showing location in world context
# You'll need to replace these coordinates with your actual study site location
study_site_lon <- -71.5  # Replace with your actual longitude
study_site_lat <- 43.8   # Replace with your actual latitude

# Get world map data
library(rnaturalearth)
library(rnaturalearthdata)

# Get North America countries
north_america <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")

# Get state/province boundaries
states_provinces <- ne_states(country = c("United States of America", "Canada"), returnclass = "sf")

inset_map <- ggplot() +
  geom_sf(data = north_america, fill = "lightgray", color = "white", size = 0.3) +
  geom_sf(data = states_provinces, fill = NA, color = "white", size = 0.2) +
  geom_point(aes(x = study_site_lon, y = study_site_lat), 
             color = "red", size = 2, shape = 8) +
  coord_sf(xlim = c(-86, -49), ylim = c(30, 50), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue"))
inset_map

########

# 3 should just be an example stand, with the chm, show 4 plots in a stand with CHM 

# Read in C2 CHM raster 

lidar_path <- here::here("data_folder","DP3.30015.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","CanopyHeightModelGtif")

chm.C2a<-rast(file.path(lidar_path,"NEON_D01_BART_DP3_318000_4881000_CHM.tif"))
chm.C2b<-rast(file.path(lidar_path,"NEON_D01_BART_DP3_318000_4880000_CHM.tif"))
chm.C2 <- terra::merge(chm.C2a,chm.C2b)

# Crop and mask the raster to C2
C2 <- st_transform(stands[stands$Site=="C2", ], st_crs(chm.C2))

C2_subp <- subp[subp$Site=="C2",]
C2_subp <- st_transform(C2_subp, st_crs(chm.C2))

C2_buffer <- st_buffer(C2, dist = 70)  # 100 meter buffer - adjust as needed

# Crop raster to buffered area
chm_c2 <- terra::crop(chm.C2, C2_buffer)


# Convert raster to data frame for ggplot
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
  geom_sf(data = C2, aes(fill = Treatment, col = Treatment), alpha = 0.4, linewidth = 2) +
  scale_fill_manual(values = c("Control" = "black", "N" = "blue", 
                               "P" = "red", "N+P" = "purple")) +
  scale_color_manual(values = c("Control" = "black", "N" = "blue", 
                                "P" = "red", "N+P" = "purple")) +
  theme_void() +
  labs(x = "", y = "") +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 16)
  ) +
  guides(
    fill = guide_legend(
      title = "Treatment",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(alpha = 1),
      order = 2
    ),
    color = guide_legend(
      title = "Treatment",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      order = 2
    )
  ) +
  coord_sf(xlim = c(min(chm_c2_df$x), max(chm_c2_df$x)),
           ylim = c(min(chm_c2_df$y), max(chm_c2_df$y)),
           expand = FALSE)
g2

# 4. Show one plot with tree tops, RGB 15 cm. 

# Get the RGB tile for C8
# east = 315000
# north = 4880000
# 
# byTileAOP("DP3.30010.001", site="BART", year="2019", check.size = F,buffer = 200,
#           easting=east, northing=north,
#           savepath="data_folder")

lidar_path <- here::here("data_folder","DP3.30015.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","CanopyHeightModelGtif")
rgb_path <- here::here("data_folder","DP3.30010.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","Camera","Mosaic")

chm.C7<-terra::rast(file.path(lidar_path,"NEON_D01_BART_DP3_315000_4880000_CHM.tif"))

pic_C7<-terra::rast(file.path(rgb_path,"2019_BART_5_315000_4880000_image.tif"))

single_plot <- stands[stands$Site=="C7" &
                        stands$Plot==2 ,]

single_plot <- st_transform(single_plot, st_crs( pic_C7))

plot_buffer <- st_buffer(single_plot, dist = 5)  # 5 meter buffer - adjust as needed

ext(pic_C7)
# Crop raster to buffered area
rgb_crop <- terra::crop(pic_C7, plot_buffer)


# Convert RGB raster to data frame for ggplot
rgb_df <- as.data.frame(rgb_crop, xy = TRUE)
# For RGB, you'll need separate columns for each band
names(rgb_df) <- c("x", "y", "red", "green", "blue")

# Create RGB values for plotting
rgb_df$rgb <- rgb(rgb_df$red/255, rgb_df$green/255, rgb_df$blue/255, maxColorValue = 1)


## Add in the crown outline and tree top
lin.C <- function(x){x * 0.02}
m7c <- crop(chm.C7, single_plot, mask=T)

m7c_raster <- raster::raster(m7c)

m7ctops <- ForestTools::vwf(CHM = m7c, winFun = lin.C, minHeight = 5)
m7ctops$Treatment<-"Control"

m7ctops_sf <- st_as_sf(m7ctops)

m7crowns <- ForestTools::mcws(treetops = m7ctops, CHM = m7c,format = "polygon", minHeight = 5)

m7crowns <- st_as_sf(m7crowns)



st_crs(single_plot) == st_crs(m7ctops_sf)


#C. selected pixels after shade masking and NDVI and shade mask



### Shade mask
### Shade mask
#  nam_d <- gsub("_reflectance.h5", "", nami) ## get coordinates of matching tile
dsm_path <- here::here("data_folder","DP3.30024.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","DSMGtif")
h5_path <- here::here("data_folder","DP3.30006.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","Spectrometer","Reflectance")

dsm <- terra::rast(file.path(dsm_path, "NEON_D01_BART_DP3_315000_4880000_DSM.tif"))
dsm_slope <- terra::terrain(dsm,v="slope")
dsm_aspect <- terra::terrain(dsm,v="aspect")



i_h5 <- file.path(h5_path,"NEON_D01_BART_DP3_315000_4880000_reflectance.h5" )
ii <- h5ls(file = file.path(i_h5))

d_nam <- paste(ii[grep("Solar_Zenith",ii$name),]$group, ii[grep("Solar_Zenith",ii$name),]$name, sep="/")
zenith <- list()
for (dd in 1:length(d_nam)){
  zenith[[dd]] <- h5read(i_h5,d_nam[dd])
  h5closeAll()
}
zenith <- mean(unlist(zenith))

d_nam <- paste(ii[grep("Solar_Azim",ii$name),]$group, ii[grep("Solar_Azim",ii$name),]$name, sep="/")
azimuth <- list()
for (dd in 1:length(d_nam)){
  azimuth[[dd]] <- h5read(i_h5,d_nam[dd])
  h5closeAll()
}
azimuth <- mean(unlist(azimuth))


##############


# Your existing shade mask creation
dsm_shade <- terra::shade(dsm_slope, dsm_aspect, angle = zenith, direction = azimuth)
shade_mask <- dsm_shade >= 0.4


# Convert tree tops to terra-compatible format if needed
# If m7ctops is from ForestTools, convert to sf first, then to SpatVector
if (!inherits(m7ctops, "sf")) {
  m7ctops_sf <- st_as_sf(m7ctops)
}
m7ctops_vect <- vect(m7ctops_sf)

# Extract shade mask values at tree top locations
tree_shade_values <- terra::extract(dsm_shade, m7ctops_sf)


##########
# Crop hillshade to plot buffer
dsm_shade_crop <- terra::crop(dsm_shade, plot_buffer)

s_val <- 0.1

# Create shade mask with 0.1 threshold
shade_mask <- dsm_shade >= s_val

# Extract shade values at tree top locations using the 0.1 threshold
if (!inherits(m7ctops, "sf")) {
  m7ctops_sf <- st_as_sf(m7ctops)
}
m7ctops_vect <- vect(m7ctops_sf)

# Extract both the continuous shade values and the binary mask
tree_shade_values <- terra::extract(dsm_shade, m7ctops_vect)
tree_mask_values <- terra::extract(shade_mask, m7ctops_vect)

#####################################

# Keep as sf and use a different approach
m7ctops_sf <- st_as_sf(m7ctops)

# Convert coordinates to matrix for terra extract
coords_matrix <- st_coordinates(m7ctops_sf)

# Extract values using coordinate matrix
tree_shade_values <- terra::extract(dsm_shade, coords_matrix)
tree_mask_values <- terra::extract(shade_mask, coords_matrix)

# Add to tree tops data
m7ctops_sf$shade_intensity <- tree_shade_values$hillshade  # Continuous shade values



###################

remove_shaded_pixels <- m7ctops_sf[m7ctops_sf$kept=="TRUE",]
shaded_pixels <- m7ctops_sf[m7ctops_sf$kept=="FALSE",]

g4 <- ggplot() +
  geom_raster(data = rgb_df, aes(x = x, y = y), fill = rgb_df$rgb) +
  geom_sf(data = m7ctops_sf, 
          aes(col = shade_intensity),
          size = 3) +
  scale_color_viridis_c(name = "Shade intensity", option = "plasma") +
  geom_sf(data = single_plot, aes(), 
          col = "black", fill = NA, linewidth = 3, size = 2) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16),
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
           expand = FALSE)

g4

library(patchwork)

final_plot <- inset_map + g1 + g2 + g4 + 
  plot_layout(ncol = 4, widths = c(0.8, 1, 1, 1)) 

final_plot


library(cowplot)
final_plot <- plot_grid(
  inset_map, g1, g2, g4, 
  ncol = 4, 
  nrow = 1,
  rel_widths = c(1, 1, 1, 1),
  align = 'h',
  axis = 'tb'
)

final_plot
