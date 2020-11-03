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

par(mfrow=c(2,2))
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
plots<-readOGR("data_folder","Bartlett_intensive_sites_30x30")

# transform to UTM coordinates
crss <- make_EPSG()
UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))
stands <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))
C7<-stands[stands$stand =="C7",]
# centroids are the 'plot centers'. code for Lidar tiles works with point data
centroids <- as.data.frame(getSpPPolygonsLabptSlots(C7))
east<-centroids$V1
north<-centroids$V2

# this downloads 15 cm Rgb data for the whole site.
byTileAOP("DP3.30010.001", site="BART", year="2017", check.size = F,buffer = 200, 
          easting=east, northing=north, 
          savepath="data_folder")

pic.C7<-stack("data_folder\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_315000_4880000_image.tif")
C7<-stands[stands$stand=="C7",]
control<-C7[C7$plot=="3", ]
control3<-extent(control)+10
co.bart<-crop(pic.C7, control3)



#### plot chm
chm_C7<-raster("data_folder\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_315000_4880000_CHM.tif")
chm7<-crop(chm_C7,control3)
# do treE top detection
lin.C7 <- function(x){x * 0.02}
m7tops <- vwf(CHM = chm7, winFun = lin.C7, minHeight = 2)
C7crownsPoly <- mcws(treetops = m7tops, CHM = chm7, format = "polygons", minHeight = 1.5, verbose = FALSE)


#3 3 band tiff with shade mask
# read in mini_noshade with C7 no shade?

mini<-extent(control3)
mini_noshade <- crop(cube_no_shade,mini)

############################
par(mfrow=c(1,1))
#3333 Rgb image
plotRGB(co.bart)
dpi=300
#tiff("R_output/tree_fig",width=12*dpi, height=10*dpi, res=dpi)
dev.off()
# add treetops to chm
#tiff("R_output/lidar_fig",width=12*dpi, height=10*dpi, res=dpi)
plot(chm7, axes=F, box=F, legend=T)
#plot(C7crownsPoly, border = "white", lwd = 0.5, add = TRUE)
plot(m7tops, add=T, pch=21,bg="blue",col="white", cex=1.5)

plotRGB(mini_noshade, r = 56, g = 28, b = 14, stretch = 'lin')
#plot(C7crownsPoly, border = "white", lwd = 0.5, add = TRUE)
plot(m7tops, add=T, pch=21,bg="blue",  col="white",cex=1.5)
tiff("R_output/shade_fig",width=12*dpi, height=10*dpi, res=dpi)
