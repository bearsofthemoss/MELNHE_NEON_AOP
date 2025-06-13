

## 2 x 2 pattern.

### top left:  WREF boundary
### top right, 4 plot RGB with outline plot and 30x30
### bottom left: select plot with CHM showing tree crowns
### bottom right: select plot with shade / non-shade and pixel ID (i.e. in / out)


library(tidycensus)
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

g1 <- ggplot() + 
  geom_sf(data=ba, fill="lightgreen")+
  geom_sf(data = stand_centroids, aes(fill=Age, shape=Age), size = 4) +
  scale_fill_manual(values=c("Young"="#E6AB02", "Mid-aged"="#666666","Mature"="#D95F02")) +
  scale_shape_manual(values=c("Young"=21, "Mid-aged"=22, "Mature"=24)) +  # or choose your preferred shapes
  theme_minimal()+
  theme(panel.grid.major = element_blank())+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )+
  geom_text_repel(data = stand_centroids, aes(x = st_coordinates(stand_centroids)[,1], 
                                              y=  st_coordinates(stand_centroids)[,2],
                                              label = Site))+
labs(fill="Age", shape="Age", x="", y="")+
  ggtitle("A. 9 forest stands")+

  theme(  plot.title = element_text(size = 16))

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
  ggtitle("B. Example clearcut with 4 treatment plots") +
  theme(
    plot.title = element_text(size = 16),
    legend.position = "right"
  ) +
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

chm.C7<-rast(file.path(lidar_path,"NEON_D01_BART_DP3_315000_4880000_CHM.tif"))

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
m7ctops <- ForestTools::vwf(CHM = m7c, winFun = lin.C, minHeight = 5)
m7ctops$Treatment<-"Control"

m7ctops_sf <- st_as_sf(m7ctops)

m7crowns <- ForestTools::mcws(treetops = m7ctops, CHM = m7c,format = "polygon", minHeight = 5)


g3 <- ggplot() +
  # Add RGB raster as background
  geom_raster(data = rgb_df, aes(x = x, y = y), fill = rgb_df$rgb) +
  geom_sf(data = m7ctops_sf, 
          aes(col = height),  # color by tree height
          size = 3) +
  geom_sf(data = m7crowns, 
          fill = NA, 
          color = "red", 
          size = 0.5, 
          alpha = 0.8) +
  scale_color_viridis_c(name = "Tree Height (m)", option = "plasma") +
# Add the single plot boundary
  geom_sf(data = single_plot, aes(), 
          col = "white", alpha = 0.3, size = 2) +
  theme_void() +
  ggtitle("C. Tree top pixels") +
  theme(
    plot.title = element_text(size = 16),
    legend.position = "right"
  ) +
  coord_sf(xlim = c(min(rgb_df$x), max(rgb_df$x)),
           ylim = c(min(rgb_df$y), max(rgb_df$y)),
           expand = FALSE)

g3

#D. selected pixels after shade masking and NDVI and shade mask



### Shade mask
#  nam_d <- gsub("_reflectance.h5", "", nami) ## get coordinates of matching tile
dsm_path <- here::here("data_folder","Bart_DSM","DP3.30024.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","DSMGtif")
h5_path <- here::here("data_folder","Bart_tiles","DP3.30006.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","Spectrometer","Reflectance")

dsm <- terra::rast(file.path(dsm_path, "NEON_D01_BART_DP3_315000_4880000_DSM.tif"))
i_h5 <- file.path(h5_path,"NEON_D01_BART_DP3_315000_4880000_reflectance.h5" )
 

dsm_slope <- terra::terrain(dsm,v="slope")
dsm_aspect <- terra::terrain(dsm,v="aspect")


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


# find a terra compatible method for this  
dsm_shade <- terra:: shade(dsm_slope, dsm_aspect, angle = zenith , direction = azimuth)
############################

# Find ideal threshold
shade_mask <- dsm_shade >= 0.3 


shade_mask_resampled <- resample(shade_mask, pic_C7, method = "near")

# Then apply the mask
shade_crop <- mask(pic_C7, shade_mask_resampled, maskvalue = 0)

shade_crop <- crop( shade_crop, plot_buffer)

# Convert RGB raster to data frame for ggplot
shade_df <- as.data.frame(shade_crop, xy = TRUE)
# For RGB, you'll need separate columns for each band
names(shade_df) <- c("x", "y", "red", "green", "blue")

# Create RGB values for plotting
shade_df$rgb <- rgb(shade_df$red/255, shade_df$green/255, shade_df$blue/255, maxColorValue = 1)


g4 <- ggplot() +
 # geom_raster(data = shade_crop, aes(x = x, y = y))+
 geom_raster(data = shade_df, aes(x = x, y = y), fill = shade_df$rgb) +
  geom_sf(data = m7ctops_sf,  # color by tree height
          size = 2) +
#  scale_fill_gradient(low = "black", high = "white", name = "Hillshade") +
  coord_sf(expand = FALSE) +
  geom_sf(data = single_plot, aes(), 
          col = "black", alpha = 0.3, size = 3) +
  theme_void() +
  theme(  plot.title = element_text(size = 14))+
  labs(title = "D. Shade mask visualization")

g4



library(patchwork)
(g1 + g2) / (g3 + g4)





############
library(ggplot2)
library(sf)
library(raster)
library(rasterVis)
library(here)
library(neonUtilities)
library(raster)

wd <- here::here()

# read in shapefile of plot locations
stands<-st_read(here::here("data_folder","private_melnhe_locations","Bartlett_intensive_sites_30x30.shp"))
tops <- st_read(here::here("data_folder","private_melnhe_locations","bart_ttops_2025_03_09.shp"))


sh <- read.csv(here::here("data_folder","melnhe_input_files","stand_heights.csv"))
res <- read.csv(here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))

res$staplo <- paste(res$Stand, res$Plot)

res$Treatment<-sapply(res$staplo,switch,
                      "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                      "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                      "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                      "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                      "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                      "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                      "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                      "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                      "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N")





#tinv <- read.csv(here::here("data_folder","ten_plus_DBH_2019.csv"))

# Set the CRS to WGS 1984, Zone 19N
stands <- st_transform(stands, 32619)

# add inm treatments
stdf<-as.data.frame(stands)
stdf$staplo <-paste(stdf$stand, stdf$plot)
stands$Treatment<-sapply(stdf[ ,"staplo"],switch,
                         "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                         "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                         "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                         "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                         "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                         "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                         "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                         "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                         "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N",
                         "HBM 1"="NP", "HBM 2"="N",  "HBM 3"="Control","HBM 4"="P",
                         "HBO 1"="P",  "HBO 2"="N",  "HBO 3"="NP",  "HBO 4"="Control", "HBO 7"="Control",
                         "JBM 1"="NP", "JBM 2"="N",  "JBM 3"="Control","JBM 4"="P",
                         "JBO 1"="NP", "JBO 2"="P",  "JBO 3"="N",   "JBO 4"="Control")
rm(stdf)


#### 6 figures. 
# first row RGB
#  Then the DSM
#  Then CHM with tree tops.  Bottom row is the spectra from each processing step. 

C3<-stands[stands$stand=="C3",]
# C5<-stands[stands$stand=="C5",]
# C6<-stands[stands$stand=="C6",]
# C7 <-stands[stands$stand=="C7",]

### get centroids
centroids <-  st_coordinates(st_centroid(C3))
east <- centroids[, 1]
north <-centroids[, 2]

# ## Download data from NEON if needed.
# # Lidar CHM
# byTileAOP(dpID="DP3.30015.001", site="BART",
#           year="2019", easting=east,
#           northing=north,
#           buffer=500, savepath="data_folder")
# 
# 
# ## this downloads 15 cm Rgb data for the whole site.
# byTileAOP("DP3.30010.001", site="BART", year="2019",buffer = 200,
#           easting=east, northing=north,
#           savepath="data_folder")




# Read in chm
lidar_path <- file.path(wd, "data_folder","DP3.30015.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","CanopyHeightModelGtif")

chm.C3<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_316000_4878000_CHM.tif"))
#chm.C5<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_314000_4878000_CHM.tif"))
#chm.C6<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_317000_4878000_CHM.tif"))
#chm.C7<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_315000_4880000_CHM.tif"))


## Source  DSM
getwd()

#source("code/misc_code_ay_cleanup/get_DSM_C3.R")

# Read in rgb IMAGE
pic_path <- file.path(wd, "data_folder","DP3.30010.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","Camera","Mosaic")

pic.C3<-stack(file.path(pic_path, "2019_BART_5_316000_4878000_image.tif"))
#pic.C5<-stack(file.path(pic_path, "2019_BART_5_314000_4878000_image.tif"))
#pic.C6<-stack(file.path(pic_path,"2019_BART_5_317000_4878000_image.tif"))
#pic.C7<-stack(file.path(pic_path,"2019_BART_5_315000_4880000_image.tif"))


pic <-  pic.C3
chm <-  chm.C3
stand <- C3



## Adjust the area of the bounding box
extend <- 30
yPlus <- extent(stand)[4] + extend
xPlus <- extent(stand)[2] + extend
yMinus <-extent(stand)[3] - extend
xMinus <-extent(stand)[1] - extend

# Example coordinates for four points
points_df <- data.frame(
  id = c(1, 2, 3, 4),
  x = c( xMinus, xMinus, xPlus, xPlus),
  y = c( yMinus, yPlus, yMinus, yPlus)
)


# Convert the data frame to an sf object
stand_view <- st_as_sf(points_df, coords = c("x", "y"))
# Assign WGS 84 CRS
st_crs(stand_view) <- st_crs(4326)

stand_box <- st_bbox(stand_view)

zoom.pic <- crop(pic, stand_box)

zoom.chm <- crop(chm, stand_box)



###########################################

par(mfrow=c(1,3))

plotRGB(zoom.pic,
        r = 1, g = 2, b = 3,
        scale = 150, stretch = "lin" , scales=F)

plot(stand, col = 'transparent',
     border = c("black","blue","red","purple"),
     lwd = 4, add = TRUE)


#################


# Now the DSM
plot(zoom.dsm, axes=F)
plot(stand, col='transparent',
     border = c("black","blue","red","purple"),
     lwd=4, add=TRUE)

## Now the CHM for the stand

plot(zoom.chm, axes=F)

plot(stand, col='transparent', 
     border = c("black","blue","red","purple"),
     lwd=4,add=T)

plot(trees, add=T)


############################



######### next set of figures is a zoom into a plot


# Step one, zoom in with RGB and 1 plot, show tree top points.

# step 2.  plot CHM and tree polys , show tree polygons and points
### CHM


# step 3.   Show DSM crop of the RGB image, \
# Create polygon crown map
C3n_crownsPoly <- mcws(treetops = m3ntops, CHM = tch3, format = "polygons", minHeight = 1.5, verbose = FALSE)




