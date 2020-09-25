### Anna K. Schweiger 9-22-2020
### Alex Young 9-22-2020

library(sp)
library(rgdal)
library(stringr) # this is for data management
library(tidyr)
library(ggplot2)
library(raster)
library(rhdf5)
library(neonUtilities)
library(rgdal)
library(rgeos)
source("./band2raster.R")

# brightness normalization function
bright_norm <- function(x){
  x_norm <- x/sqrt(sum(x^2))
  return(x_norm)}

# Anna's wd: # setwd("C:/Users/Anna/Bartlett/")
# Alex's wd:
setwd("C:\\Users\\aryoung\\Documents\\GitHub\\MELNHE_NEON_AOP")

## read plot shapefiles

## plots
#Anna's plot file# plots <- readOGR("./R_input/fwdneoncoverageofourtrees","Bartlett_intensive_sites_subplots")
# Alex's plots
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

# Download DSMs
#byTileAOP(dpID="DP3.30024.001",site="BART",
#          year="2017", easting= east,
#          northing = north,
#          buffer=50, savepath = "./R_input/Bart_DSM/",check.size = T)

# Mac
#ff <- list.files("R_input/Bart_tiles/",pattern = ".h5", recursive = T, full.names = T)
#dd <- list.files("R_input/Bart_DSM/",pattern = "DSM.tif", recursive = T, full.names = T)

#PC- Alex's work
ff <- list.files("R_input/Bart_tiles",pattern = ".h5", recursive = T, full.names = T)
dd <- list.files("R_input/Bart_DSM",pattern = "DSM.tif", recursive = T, full.names = T)


# AY saw 23 files in ff. and 23 files in dd. 9_22_2020
# AY used plot centers and got 10 files in each 9_23_2020
############################################################################################
#### Begin hyperspectral data management
# Extract image information
spectra_df <- list()

for (k in 1:length(ff)){
  f <- ff[1]
  
  x <- h5ls(f)[grep("Wavelength", h5ls(f)[,2]),]
  xx <- paste(x[1],x[2],sep="/")
  wvl <- h5read(f,xx)
  
  x <- h5ls(f)[grep("FWHM", h5ls(f)[,2]),]
  xx <- paste(x[1],x[2],sep="/")
  fwhm <- h5read(f,xx)
  
  x <- h5ls(f)[grep("Coordinate_System$", h5ls(f)[,2]),]
  xx <- paste(x[1],x[2],sep="/")
  (spInfo <- h5read(f,xx))
  
  x <- h5ls(f)[grep("Reflectance_Data", h5ls(f)[,2]),]
  xx <- paste(x[1],x[2],sep="/")
  reflInfo <- h5readAttributes(f,xx)
  
  reflInfo$Dimension_Labels  ### data to ignore
  (myNoDataValue <- as.numeric(reflInfo$Data_Ignore_Value))
  
  x <- h5ls(f)[grep("Map_Info", h5ls(f)[,2]),]  ### coordinate reference system
  xx <- paste(x[1],x[2],sep="/")
  mapInfo <- h5read(f,xx)
  (mapInfo <- unlist(strsplit(mapInfo, ",")))
  myCRS <- CRS(paste0("+init=epsg:",spInfo$`EPSG Code`))
  
  reso <- as.numeric(mapInfo[2]) ### resolution
  
  ### extent
  # left side x coordinate (xMin)
  xMin <- as.numeric(mapInfo[4]) 
  # top corner coordinate (yMax)
  yMax <- as.numeric(mapInfo[5])
  
  reflInfo$Dimension_Labels
  reflInfo$Dimensions
  
  xMax <- (xMin + reflInfo$Dimensions[2]*reso) ### xMax = left edge + (no of cols*x pixel)
  yMin <- (yMax -  reflInfo$Dimensions[1]*reso) ### yMin + top edge - (no of rows*y pixel)
  
  rasExt <- extent(xMin,xMax,yMin,yMax) ### define the extent (left, right, top, bottom)
  
  # Read all bands and create raster stack 
  a <- 1:length(wvl) ### list of bands to be stacked
  all_wvls <- list()
  for(i in 1:length(wvl)){
    all_wvls[[i]] <- a[i]
  }
  
    # this takes a minute
  cube_rast <- lapply(all_wvls,band2raster, path_h5 = f,NoDataValue = myNoDataValue,
                      xMin = xMin, yMax = yMax,
                      res=reso, crs = myCRS, ncols =  reflInfo$Dimensions[2],
                      nrows =  reflInfo$Dimensions[1])
  hsiStack <- stack(cube_rast)
  
  
  # make list of band names 
  bandNames <- paste("Band_", round(wvl,digits = 2),sep="")
  names(hsiStack) <- bandNames ### Raster does not safe band names
  
  # origina nami from Anna  (This just gave AY the value '2017')
  nami <- sapply(strsplit(f,"/"),"[",3)
  
  # alex try nami. The 10th slot has the tile name with coord 313000_4877000
  strsplit(f,"/")
  nami<-sapply(strsplit(f,"/"),"[",11)  
  nami
  
  ### plot
#3 AY can't see whats going on here?
  #plotRGB(hsiStack,r = 52, g = 28, b = 10, stretch = 'lin',colNA=1)
  #plot(plots_UTM, add=T, col="2")

## Ay can't see the overlay happenning? looks like the Rgb doesn't look right?  
  #plot(plots_UTM, col="pink")
  # text(coordinates(plots_UTM), labels=plots_UTM$Site, cex=0.8)
  
  # plot(plots_UTM[plots_UTM$Site=="C1",])
  
  # For Anna's wd:# writeRaster(hsiStack, paste0("./R_output/Bart_tiles_processed/", nami, "_.grd"), format="raster")

  ### Make NDVI raster layer
  # NEON uses bands close to 648.2, 858.6
  # ideal bands: https://data.neonscience.org/documents/10179/11204/NEON.DOC.002391vA/0b2a4472-95eb-42a7-a3cb-db6647de7ba9
  ndvi_bands <- c(54,96)
  wvl[ndvi_bands] ### check if these are the wavelengths
  
  # create raster list and stack the two bands
  ndvi_rast <- lapply(ndvi_bands,band2raster, path_h5 = f,NoDataValue = myNoDataValue,
                      xMin = xMin, yMax = yMax, res=reso, crs = myCRS, ncols =  reflInfo$Dimensions[2], nrows =  reflInfo$Dimensions[1])
  ndvi_stack <- stack(ndvi_rast)
  bandNDVINames <- paste("Band_",unlist(ndvi_bands),sep="")
  names(ndvi_stack) <- bandNDVINames
  
  # calculate NDVI
  NDVI <- function(x){(x[,2]-x[,1])/(x[,2]+x[,1])}
  ndvi_calc <- calc(ndvi_stack,NDVI)
  #For Anna's wd:# writeRaster(ndvi_calc, file= paste0("./R_output/Bart_tiles_processed/", nami,"_NDVI.tif"), format="GTiff")
  
  h5closeAll()
  ##################################################################################
  
  ##################################################################################
  ### Process images ###  Part 2
  ##################################################################################
  ### Remove water absorption bands
  # index for good bands
  good <- which((as.numeric(wvl)>400 & as.numeric(wvl)<1340|
                   as.numeric(wvl)>1445 & as.numeric(wvl)<1790|
                   as.numeric(wvl)>1955 & as.numeric(wvl)<2400)==T)
  
  ###------
  
  ### subset bands
  cube_wat <- raster::subset(hsiStack, good)
  
  ### NDVI mask
  ndvi_lim <- ndvi_calc >= 0.9 # set NDVI threshold, could be 0.6
  
  # mask bands, takes a minute
  cube_masked <- raster::mask(cube_wat, ndvi_lim, maskvalue = FALSE)
  
  # brightness normalization
  cube_norm <- raster::calc(cube_masked, fun=bright_norm)
  names(cube_norm) <- names(cube_masked)
  
  #plotRGB(cube_norm,r = 52, g = 28, b = 10, stretch = 'lin',colNA="red")
  
  ### Shade mask
  nam_d <- gsub("_reflectance.h5", "", nami) ## get coordinates of matching tile

    dsm <- raster(dd[grep(nam_d,dd)])

  
  dsm_slope <- terrain(dsm,opt="slope")
  dsm_aspect <- terrain(dsm,opt="aspect")
  

  i_h5 <- f[grep(nam_d,f)][1]
  ii <- h5ls(file = i_h5)
  
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
  
  dsm_shade <- hillShade(dsm_slope, dsm_aspect, angle = zenith, direction = azimuth)
  
  ############################
  # Find ideal threshold
  shade_mask <- dsm_shade >= 0.1 
  
  # Mask RGB for testing
  cube_no_shade <- raster::mask(hsiStack, shade_mask, maskvalue = 0)
  
  # Select subset to examine threshold
  # better: clip to extent of stand (centroid plus 200 m buffer)
  
  ## Alex #'d these out to reduce time
  #plotRGB(hsiStack, r = 56, g = 28, b = 14, stretch = 'lin')
  #plot(plots_UTM, add=T, col=5)
  #mini <- drawExtent()
  #mini_cube <- crop(hsiStack,mini)
  #mini_noshade <- crop(cube_no_shade,mini)
 
  # plot
  #plotRGB(mini_cube,r = 56, g = 28, b = 14, stretch = 'lin')
  #plotRGB(mini_noshade,r = 56, g = 28, b = 14, stretch = 'lin')
  #plot(plots_UTM, add=T, col="red")
  
  ###################
  # Apply to processed images
  cube_no_shade <- raster::mask(cube_norm, shade_mask, maskvalue = 0)
  
  extent(shade_mask)
  
  extent(cube_norm)
  #plotRGB(cube_no_shade, r = 56, g = 28, b = 14, stretch = 'lin', colNA="red")
  
  #plotRGB(mini_noshade, r = 56, g = 28, b = 14, stretch = 'lin', colNA="red")
  #plot(trees, add=T, col="yellow")
  
  ################################################################################################
  ################################################################################################
  # Extract data
  
  # Select trees within extent of tile
  inout <- gIntersects(as(extent(cube_no_shade), 'SpatialPolygons'), trees, byid = T)
  trees_in <- trees[as.vector(inout),]
  
# here you extract the hyperspectral data from the cube by the spatial points of the tree. Hopefully.
  spectra <- raster::extract(cube_no_shade,trees_in,df=T, sp=T)

  spectra_df[[k]] <- as.data.frame(spectra@data)
}

head(spectra_df)

### combine and save
spectra_all <- do.call(rbind, spectra_df)

head(spectra_all)
names(spectra_all)
write.csv(spectra_all, file="actual_tops_9_24.csv")







