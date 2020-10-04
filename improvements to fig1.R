library(ggplot2)
theme_set(theme_classic())
library(sf)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
#
library(ggmap)
library(gridExtra)
#

library(neonUtilities)
library(ForestTools)
library(raster)
library(rgdal)
library(rgeos)
###########


###### PART 1:   Map of NH
world <- ne_countries(scale = "medium", returnclass = "sf")
sites <- data.frame(longitude = c(-71.28731), latitude = c(44.06388 ))
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- cbind(states, st_coordinates(st_centroid(states)))

ggplot(data = world) +  geom_sf() +   geom_sf(data = states, fill = NA) + 
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-75, -67), ylim = c(40, 47), expand = FALSE)



######  PART2:  9 stands color coded plots
# read in shapefile
plots<-readOGR("R_input","Bartlett_intensive_sites_30x30")

# transform to UTM coordinates
crss <- make_EPSG()
UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))
stands <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))
# centroids are the 'plot centers'. code for Lidar tiles works with point data
centroids <- as.data.frame(getSpPPolygonsLabptSlots(stands))

# this grabs 15 cm Rgb data for the whole site.
#byFileAOP(dpID = "DP3.30010.001", site = "BART", year = "2017", check.size = T, savepath="R_input")

#3 read in the 20 rgb tifs for panel 2
t11<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_313000_4878000_image.tif")
t12<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_313000_4879000_image.tif")
t13<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_313000_4880000_image.tif")
t14<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_313000_4881000_image.tif")
#
t21<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_314000_4878000_image.tif")
t22<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_314000_4879000_image.tif")
t23<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_314000_4880000_image.tif")
t24<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_314000_4881000_image.tif")
#
t31<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_315000_4878000_image.tif")
t32<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_315000_4879000_image.tif")
t33<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_315000_4880000_image.tif")
t34<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_315000_4881000_image.tif")
#
t41<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_316000_4878000_image.tif")
t42<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_316000_4879000_image.tif")
t43<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_316000_4880000_image.tif")
t44<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_316000_4881000_image.tif")
#
t51<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_317000_4878000_image.tif")
t52<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_317000_4879000_image.tif")
t53<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_317000_4880000_image.tif")
t54<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_317000_4881000_image.tif")
#
t61<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_318000_4878000_image.tif")
t62<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_318000_4879000_image.tif")
t63<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_318000_4880000_image.tif")
t64<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_318000_4881000_image.tif")


t.bart<-merge(t11,t12,t13, t14,
          t21,t22,t23, t24,
          t31,t32,t33, t34,
          t41,t42,t43, t44,
          t51,t52,t53, t54,
          t61,t62,t63, t64)

land<-extent(stands)
land<-land+200 # add 100 m on each side
pic.bart<-crop(t.bart, land)

plotRGB(pic.bart)
plot(stands, add=T, col="pink",lwd=2)

######  Part 3 and 4
#stands<-readOGR("data_files\\plot_shp","Bartlett_intensive_sites_30x30")
library(sf)
stands<-readOGR("C:\\Users\\Dropcopter2\\Documents\\R\\hyperspectral R\\mel_NEON\\plot_shp","Bartlett_intensive_sites_30x30")

stands=spTransform(stands,CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs
+ellps=WGS84 +towgs84=0,0,0")) # To convert it to WGS8

stands$staplo<-paste(stands$stand, stands$plot)

C3<-stands[stands$staplo=="C3 1",]
center.C3 <- as.data.frame(getSpPPolygonsLabptSlots(C3))
center.C3

### stand C5
# download tile for RGB

byTileAOP("DP3.30010.001", site="BART", year="2017", 
          check.size = F,buffer = 100,
          easting=center.C3[,1], northing=center.C3[,2] , savepath="neon_downloads")


# loop through 
#  Read in tif


#byTileAOP("DP3.30015.001", site="BART", year="2017", 
#          check.size = F,buffer = 100,
#          easting=center.C3[,1], northing=center.C3[,2] , savepath="neon_downloads")

# read in rgb image
img_C3<-stack("neon_downloads\\DP3.30010.001\\2019\\FullSite\\D01\\2019_BART_5\\L3\\Camera\\Mosaic\\2019_BART_5_316000_4878000_image.tif")
# plot rgb image
plotRGB(img_C3,axes=TRUE, main="Stand C3")
# crop tile from AOP by plot extent
pC3<-crop(img_C3,C3)

# read in chm raster
chm_C3<-raster("C:\\Users\\Dropcopter2\\Documents\\GitHub\\MELNHE_NEON_AOP\\neon_downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_316000_4878000_CHM.tif")
plot(chm_C3)
lC3<-crop(chm_C3,C3)

# plotting
plotRGB(pC3,axes=TRUE, main="Bartlett Experimental Forest")
plot(C3, add=T, lwd=6)

par(mfrow=c(1,2))
plot(lC3)
plot(C3, add=T, lwd=6)

