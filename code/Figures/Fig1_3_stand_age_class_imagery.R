

## 2 x 2 pattern.

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

library(ggplot2)
library(sf)
library(ggrepel)
library(maps)
library(cowplot)  # for combining plots

# Get world map data
world_map <- map_data("world")

# Your original detailed map
g1 <- ggplot() + 
  geom_sf(data=ba, fill="lightgreen")+
  geom_sf(data = stand_centroids, aes(fill=Age, shape=Age), size = 4) +
  scale_fill_manual(values=c("Young"="#E6AB02", "Mid-aged"="#666666","Mature"="#D95F02")) +
  scale_shape_manual(values=c("Young"=21, "Mid-aged"=22, "Mature"=24)) +
  theme_minimal()+
  theme(panel.grid.major = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )+
  geom_text_repel(data = stand_centroids, aes(x = st_coordinates(stand_centroids)[,1], 
                                              y=  st_coordinates(stand_centroids)[,2],
                                              label = Site))+
  labs(fill="Age", shape="Age", x="", y="")+
  theme(plot.title = element_text(size = 16))

# Create inset map showing location in world context
# You'll need to replace these coordinates with your actual study site location
study_site_lon <- -71.5  # Replace with your actual longitude
study_site_lat <- 43.8   # Replace with your actual latitude

inset_map <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "white", size = 0.1) +
  geom_point(aes(x = study_site_lon, y = study_site_lat), 
             color = "red", size = 2, shape = 8) +  # star shape
  coord_fixed(1.3) +  # maintain aspect ratio
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.background = element_rect(fill = "white", color = "black"),
        panel.background = element_rect(fill = "lightblue"))  # ocean color

# Combine the main map with the inset
g1 <- ggdraw(g1) +
  draw_plot(inset_map, 
            x = 0.65, y = 0.02,    # position (bottom-left corner)
            width = 0.3, height = 0.3)  # size of inset



g1

########

# B should just be an example stand, with the chm, show 4 plots in a stand with CHM 

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
  # Add raster layer first (background)
  geom_raster(data = chm_c2_df, aes(x = x, y = y, fill = height)) +
  scale_fill_gradientn(colors = rev(terrain.colors(100)), 
                       name = "Height (m)",
                       na.value = "transparent") +
  
  # Add new fill scale for treatments
  ggnewscale::new_scale_fill() +
  
  # Add C2 plots with treatment colors
  geom_sf(data = C2, aes( fill = Treatment, col=Treatment), alpha = 0.4, linewidth = 2) +
  
  # Treatment color scale
  scale_fill_manual(values = c("Control" = "black", "N" = "blue", 
                               "P" = "red", "N+P" = "purple")) +
  scale_color_manual(values = c("Control" = "black", "N" = "blue", 
                               "P" = "red", "N+P" = "purple")) +
  
  theme_void() +  # Try theme_void() instead of theme_minimal()
  labs(fill = "Treatment",col="Treatment", x = "", y = "") +
  theme(
    plot.title = element_text(size = 16),
    legend.position = "right"
  ) +
  # geom_sf(data = m7ctops_sf, 
  #         aes(color = shade_intensity), 
  #         size = 2)+ 
  # Set coordinate limits explicitly
  coord_sf(xlim = c(min(chm_c2_df$x), max(chm_c2_df$x)),
           ylim = c(min(chm_c2_df$y), max(chm_c2_df$y)),
           expand = FALSE)

g2
# C. Show one plot with tree tops, RGB 15 cm. 

# Get the RGB tile for C8
# east = 315000
# north = 4880000
# 
# byTileAOP("DP3.30010.001", site="BART", year="2019", check.size = F,buffer = 200,
#           easting=east, northing=north,
#           savepath="data_folder")

lidar_path <- here::here("data_folder","DP3.30015.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","CanopyHeightModelGtif")
rgb_path <- here::here("data_folder","DP3.30010.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","Camera","Mosaic")

chm.C7<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_315000_4880000_CHM.tif"))

pic_C7<-rast(file.path(rgb_path,"2019_BART_5_315000_4880000_image.tif"))

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
dsm_path <- here::here("data_folder","Bart_DSM","DP3.30024.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","DSMGtif")
h5_path <- here::here("data_folder","Bart_tiles","DP3.30006.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","Spectrometer","Reflectance")

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
m7ctops_sf$kept <- tree_mask_values$hillshade  # Binary kept/removed
m7ctops_sf$status <- ifelse(m7ctops_sf$kept == 1, "Kept (≥0.1)", "Removed (<0.1)")

# Convert hillshade to dataframe for ggplot
shade_crop_df <- as.data.frame(dsm_shade_crop, xy = TRUE)
names(shade_crop_df) <- c("x", "y", "hillshade")

shade_crop_df$hillshade_cat <- cut(shade_crop_df$hillshade, 
                                   breaks = 4, 
                                   labels = c("Very Low", "Low", "Moderate", "High"))


g3 <- ggplot() +
  geom_raster(data = shade_crop_df, aes(x = x, y = y, fill = hillshade_cat)) +
  scale_fill_manual(values = c("Very Low" = "#000000", 
                               "Low" = "#555555", 
                               "Moderate" = "#AAAAAA", 
                               "High" = "#FFFFFF"),
                    name = "Shade value") +
  geom_sf(data = m7ctops_sf, 
          aes(color = status), 
          size = 2) +
  scale_color_manual(values = c("Kept (≥0.1)" = "green", 
                                "Removed (<0.1)" = "red"),
                     name = "Tree Status") +
  coord_sf(expand = FALSE) +
  geom_sf(data = single_plot, 
          fill = NA, color = "black", size = 1, linewidth=3) +
  theme_void() +
  theme(plot.title = element_text(size = 14)) 

g3


###################

remove_shaded_pixels <- m7ctops_sf[m7ctops_sf$kept=="TRUE",]
shaded_pixels <- m7ctops_sf[m7ctops_sf$kept=="FALSE",]

g4 <- ggplot() +
  # Add RGB raster as background
  geom_raster(data = rgb_df, aes(x = x, y = y), fill = rgb_df$rgb) +
  geom_sf(data = m7ctops_sf, 
          aes(col = shade_intensity  ),  # color by shade intensity
          size = 3) +
    # geom_sf(data = m7crowns, 
    #       fill = NA, 
    #       color = "red", 
    #       size = 0.5, 
    #       alpha = 0.8) +
  scale_color_viridis_c(name = "Tree shade intensity", option = "plasma") +
  # Add the single plot boundary
  geom_sf(data = single_plot, aes(), 
          col = "black", fill=NA, linewidth = 3, size = 2) +
  theme_void() +
    theme(
    plot.title = element_text(size = 16),
    legend.position = "right"
  ) +
  coord_sf(xlim = c(min(rgb_df$x), max(rgb_df$x)),
           ylim = c(min(rgb_df$y), max(rgb_df$y)),
           expand = FALSE)

g4


 library(cowplot)
 plot_grid(g1, g2, g4, ncol = 3, nrow = 1)
