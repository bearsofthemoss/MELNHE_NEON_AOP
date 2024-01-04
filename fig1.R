
# library(ggplot2)
# theme_set(theme_classic())
# library(sf)
# library(maps)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(maps)
# library(ggspatial)
# library(ggmap)
# library(gridExtra)
# library(ggpubr) # for showing 2 ggplots in 1 pane.
# 
# 

# library(ForestTools)
# library(raster)
# library(rgdal)
# library(rgeos)


############
library(ggplot2)
library(sf)
library(raster)
install.packages("rasterVis")
library(here)
library(neonUtilities)
library(raster)

wd <- here::here()

# read in shapefile of plot locations
stands<-st_read(file.path("data_folder","Bartlett_intensive_sites_30x30.shp"))

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
extend <- 70
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

par(mfrow=c(1,2))

plotRGB(zoom.pic,
        r = 1, g = 2, b = 3,
        scale = 800, stretch = "lin" , scales=F)

plot(stand, col = 'transparent',
     border = c("black","blue","red","purple"),
     lwd = 4, add = TRUE)

## Now the CHM for the stand

plot(zoom.chm, axes=F)

plot(stand, col='transparent', 
     border = c("black","blue","red","purple"),
     lwd=4,add=T)



#############################

#####
tch3<-crop(chm.C3, m3ntops)
pc3<-crop(pic.C3, m3ntops)
##
tch5<-crop(chm.C5, m5ntops)
pc5<-crop(pic.C5, m5ntops)
##
tch9<-crop(chm.C9, m9ntops)
pc9<-crop(pic.C9, m9ntops)


########################################################

# plot chm and rgb next to each other


# Create polygon crown map
C3n_crownsPoly <- mcws(treetops = m3ntops, CHM = tch3, format = "polygons", minHeight = 1.5, verbose = FALSE)
C5n_crownsPoly <- mcws(treetops = m5ntops, CHM = tch5, format = "polygons", minHeight = 1.5, verbose = FALSE)
C9n_crownsPoly <- mcws(treetops = m9ntops, CHM = tch9, format = "polygons", minHeight = 1.5, verbose = FALSE)


# Plot CHM
plot(tch3, xaxt='n', yaxt = 'n', main="Young stand C3")
plot(C3n_crownsPoly, border = "blue", lwd = 0.5, add = TRUE)
plot(C3_ttops, add=T, pch=17, cex=1)
plot(tch5, xaxt='n', yaxt = 'n', main="Mid-aged stand C5")
plot(C5n_crownsPoly, border = "blue", lwd = 0.5, add = TRUE)
plot(C5_ttops, add=T, pch=17, cex=1)
plot(tch9, xaxt='n', yaxt = 'n', main="Old stand C9")
plot(C9n_crownsPoly, border = "blue", lwd = 0.5, add = TRUE)
plot(C9_ttops, add=T, pch=17, cex=1)

#33


# PLot RGB
plotRGB(pc3, axes=F )
plot(C3n_crownsPoly, border = "red", lwd = 2, add = TRUE)
plot(C3_ttops, add=T, pch=17, cex=1.4, col="yellow")
plotRGB(pc5, axes=F )
plot(C5n_crownsPoly, border = "red", lwd = 2, add = TRUE)
plot(C5_ttops, add=T, pch=17, cex=1.4, col="yellow")
plotRGB(pc9, axes=F )
plot(C9n_crownsPoly, border = "red", lwd = 2, add = TRUE)
plot(C9_ttops, add=T, pch=17, cex=1.4, col="yellow")






# Compute average crown diameter
C9n_crownsPoly[["crownDiameter"]] <- sqrt(C9n_crownsPoly[["crownArea"]]/ pi) * 2
mean(C9n_crownsPoly$crownDiameter)





################
################


chm.C2a<-raster("data_folder/DP3.30015.001/neon-aop-products/2019/FullSite/D01/2019_BART_5/L3/DiscreteLidar/CanopyHeightModelGtif/NEON_D01_BART_DP3_318000_4881000_CHM.tif")
chm.C2b<-raster("data_folder/DP3.30015.001/neon-aop-products/2019/FullSite/D01/2019_BART_5/L3/DiscreteLidar/CanopyHeightModelGtif/NEON_D01_BART_DP3_318000_4880000_CHM.tif")
chm.C2 <- raster::merge(chm.C2a,chm.C2b)
chm.C4<-raster("data_folder/DP3.30015.001/neon-aop-products/2019/FullSite/D01/2019_BART_5/L3/DiscreteLidar/CanopyHeightModelGtif/NEON_D01_BART_DP3_318000_4880000_CHM.tif")


pic.C2a<-stack("data_folder/DP3.30010.001/neon-aop-products/2019/FullSite/D01/2019_BART_5/L3/Camera/Mosaic/2019_BART_5_318000_4881000_image.tif")
pic.C2b<-stack("data_folder/DP3.30010.001/neon-aop-products/2019/FullSite/D01/2019_BART_5/L3/Camera/Mosaic/2019_BART_5_318000_4880000_image.tif")
pic.C2 <- raster::merge(pic.C2a, pic.C2b)
class(pic.C2)

pic.C4<-stack("data_folder/DP3.30010.001/neon-aop-products/2019/FullSite/D01/2019_BART_5/L3/Camera/Mosaic/2019_BART_5_318000_4880000_image.tif")


plot(C2)
plot(chm.C2, add=T)
plot(C2, add=T)



#####
tch2<-crop(chm.C2, C2_ttops)
pc2<-crop(pic.C2, C2_ttops)


########################################################

# plot chm and rgb next to each other


# Create polygon crown map
C2_crownsPoly <- mcws(treetops = C2_ttops, CHM = tch2, format = "polygons", minHeight = 1.5, verbose = FALSE)


# Plot CHM
plot(tch2, xaxt='n', yaxt = 'n', main="Young stand C3")

plot(C2_crownsPoly, border = "blue", lwd = 0.5, add = TRUE)
  plot(C2_ttops, add=T, pch=17, cex=1)


  
# PLot RGB
par(mfrow=c(2,2))
  plotRGB(pc2, axes=F )
plot(C2, border = c("black","blue","red","purple"), lwd = 2, add = TRUE)

plot(tch2, axes=F )
plot(C2, border = c("black","blue","red","purple"), lwd = 2, add = TRUE)



plot(C2_ttops, add=T, pch=17, cex=1.4, col="yellow")




# Compute average crown diameter
C9n_crownsPoly[["crownDiameter"]] <- sqrt(C9n_crownsPoly[["crownArea"]]/ pi) * 2
mean(C9n_crownsPoly$crownDiameter)






