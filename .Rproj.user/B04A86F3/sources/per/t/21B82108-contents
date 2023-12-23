
library(ggplot2)
theme_set(theme_classic())
library(sf)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(ggspatial)
library(ggmap)
library(gridExtra)
library(ggpubr) # for showing 2 ggplots in 1 pane.


library(neonUtilities)
library(ForestTools)
library(raster)
library(rgdal)
library(rgeos)


#### 6 figures. 

# first row RGB


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






