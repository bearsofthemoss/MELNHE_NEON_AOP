

library(sp)
library(rgdal)
library(stringr) # this is for data management
library(tidyr)
library(ggplot2)
library(raster)
library(neonUtilities)
library(rgdal)
library(rgeos)

# get fpar


plots <- readOGR("R_input\\fwdneoncoverageofourtrees","Bartlett_intensive_sites_30x30")

# tree tops
# Anna's tree tops:# trees <- readOGR("./R_input/tree_tops","bart_ttops_6_22_2020")
# Alex's tree tops
trees <- readOGR("R_input\\tree_tops","bart_ttops_6_22_2020")


# transform to UTM coordinates
crss <- make_EPSG()

UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))

plots_UTM <- spTransform(plots, CRS(SRS_string=paste0("EPSG:",UTM$code)))

# centroids are the 'plot centers'. This script works with point data.
centroids <- as.data.frame(getSpPPolygonsLabptSlots(plots_UTM))
centroids
# these are the easting and northings for the stand locations
east <- centroids[, 1]
east
north <-centroids[, 2]
north
## If you need to download the tiles

#byTileAOP(dpID="DP3.30006.001",site="BART",
#            year="2017", easting= east,
#            northing = north,
#            buffer=70, savepath = "./R_input/Bart_tiles",check.size = T)
# the code did not have the bart_tiles for savepath-  I renamed the folder, and updated the savepath for consistency Ay 9_22_2020
