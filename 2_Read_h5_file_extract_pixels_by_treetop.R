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

# Alex's wd:
# setwd("C:\\Users\\aryoung\\Documents\\GitHub\\MELNHE_NEON_AOP")
# Anna's wd:
#setwd("~//Documents/CABO/Bartlett_Alex/")

## read plot shapefiles

## plots
# Anna's plot file
#plots <- readOGR("./data_folder","Bartlett_intensive_sites_30x30")

# Alex's plots
plots <- readOGR("data_folder","Bartlett_intensive_sites_30x30")

# Alex's tree tops
 trees <- readOGR("data_folder","bart_ttops_2_19_2022_0.02")

 
 par(mfrow=c(1,1))
 
 plot(plots)
plot(trees) 



 
 # transform to UTM coordinate
crss <- make_EPSG()

UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))

# plots_UTM <- sp::spTransform(plots, CRS(SRS_string=paste0("EPSG:",UTM$code)))
plots_UTM <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))

# centroids are the 'plot centers'. This script works with point data.
centroids <- as.data.frame(getSpPPolygonsLabptSlots(plots_UTM))





# these are the easting and northings for the stand locations
east <- centroids[, 2]
north <-centroids[, 1]

centroids

byFileAOP(dpID="DP3.30006.001",site="BART",
            year="2017", savepath = "./data_folder/Bart_tiles",check.size = T)


# Download DSMs
byTileAOP(dpID="DP3.30024.001",site="BART",
          year="2017", easting= east,
          northing = north,
          buffer=200, savepath = "./data_folder/Bart_DSM/",check.size = T)


#PC- Alex's wd
 ff <- list.files("data_folder/Bart_tiles",pattern = ".h5", recursive = T, full.names = T)
 
 dd <- list.files("data_folder/Bart_DSM",pattern = "DSM.tif", recursive = T, full.names = T)
ff
 # Anna's
#ff <- list.files("//Volumes/Backup Plus/BARTcubes_Alex/",pattern = ".h5", recursive = T, full.names = T)
#dd <- list.files("//Volumes/Backup Plus/BARTdsm_Alex/",pattern = "DSM.tif", recursive = T, full.names = T)

############################################################################################
#### Begin hyperspectral data management
# Extract image information
spectra_df <- list()

###### get shademask for stands
#rC3<-316000_4878000
#rC5<-314000_4878000
#rC9<-317000_4879000
ff

# 
for (k in 1:length(ff)){
  (f <- ff[k])
  
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
  bandNames <- paste("Band_", round (wvl,digits = 2),sep="")
  names(hsiStack) <- bandNames ### Raster does not safe band names
  
  #########################################################
  
  #########################################################
  
  #########################################################
  ## index hyperspectral .h5 file 'f', call it nami. Use the 11th slot to match the .h5 tile to the chm .tif file 
  nami<-sapply(strsplit(f,"/"),"[",11)  
  nami
  
  ### plot
   plotRGB(hsiStack,r = 52, g = 28, b = 10, stretch = 'lin',colNA=1)
   plot(plots_UTM, add=T, col=2)
   text(coordinates(plots_UTM), labels=plots_UTM$stand, cex=0.8)

  
  # For Anna's wd: save if needed
  # writeRaster(hsiStack, paste0("./R_output/Bart_tiles_processed/", nami, "_.grd"), overwrite=T,
  #             format="raster")

  ## read again
  # hsiStack <- stack(paste0("./R_output/Bart_tiles_processed/", nami, "_.grd"))
  
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
  #plot(ndvi_calc)
  #plot(plots_UTM, col=2, add=T)
  
  #For Anna's wd: save if needed
  # writeRaster(ndvi_calc, file= paste0("./R_output/Bart_tiles_processed/", nami,"_NDVI.tif"), 
  #             format="GTiff", overwrite=T)
  
  # read again 
  # ndvi_calc <- raster(paste0("./R_output/Bart_tiles_processed/", nami,"_NDVI.tif"))
  h5closeAll()
  ##################################################################################
  
  ##################################################################################
 
  ##################################################################################

  ### Now that the tile is processed, we need to
  # Remove water absorption bands, index for good bands
  good <- which((as.numeric(wvl)>400 & as.numeric(wvl)<1340|
                   as.numeric(wvl)>1445 & as.numeric(wvl)<1790|
                   as.numeric(wvl)>1955 & as.numeric(wvl)<2400)==T)
  
  ### subset bands
  cube_wat <- raster::subset(hsiStack, good)
  
  ### NDVI mask
  ndvi_lim <- ndvi_calc >= 0.9 # set NDVI threshold, could be 0.6
  # plot(ndvi_lim)
  # plot(plots_UTM, add=T)
  
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
  for (nn in 1:length(d_nam)){
    zenith[[nn]] <- h5read(i_h5,d_nam[nn])
    h5closeAll()
  }
  zenith <- mean(unlist(zenith))
  
  d_nam <- paste(ii[grep("Solar_Azim",ii$name),]$group, ii[grep("Solar_Azim",ii$name),]$name, sep="/")
  azimuth <- list()
  for (nn in 1:length(d_nam)){
    azimuth[[nn]] <- h5read(i_h5,d_nam[nn])
    h5closeAll()
  }
  azimuth <- mean(unlist(azimuth))
  
  dsm_shade <- hillShade(dsm_slope, dsm_aspect, angle = zenith, direction = azimuth)
  
  ############################
  # Find ideal threshold
  shade_mask <- dsm_shade >= 0.1
  
  ###################
  # Apply to processed images
  cube_no_shade <- raster::mask(cube_norm, shade_mask, maskvalue = 0)
  
  
  ################################################################################################
  ################################################################################################
  # Extract data
  
  # Select trees within extent of tile
  inout <- gIntersects(as(extent(cube_no_shade), 'SpatialPolygons'), trees, byid = T)
  trees_in <- trees[as.vector(inout),]
 
 #################################################################################################

  # get the mini area of the control plot for each tile?
  mini <- extent(trees_in[trees_in$Treatment=="Control",])
  mini_cube <- crop(hsiStack,mini)
  mini_dsm <- crop(dsm_shade,mini)
 
  # Make plot
  par(mfrow=c(2,2))
  # large tile
  plotRGB(cube_no_shade, r = 56, g = 28, b = 14, stretch = 'lin', colNA="red")
  plot(plots_UTM, col="yellow", add=T)
  # control plot
  plotRGB(mini_cube,r = 56, g = 28, b = 14, stretch = 'lin')
  plot(plots_UTM, add=T, border=2, lwd=5)
  # plot shade mask
  plot(mini_dsm)
  plot(plots_UTM, add=T, border=2, lwd=5)
  plot(trees, add=T,pch=16, col=2)
    # plot control plot after shade mask
   mini_noshade <- crop(cube_no_shade,mini)
   plotRGB(mini_noshade, r = 56, g = 28, b = 14, stretch = 'lin')
   plot(plots_UTM, add=T, border=2, lwd=5)
   plot(trees, add=T, pch=16, col=2)
  
  # If you want to write the shade mask for figure 1
  #writeRaster(mini_noshade, "C7_no_shade", overwrite=T,format="raster")
  
   #################################################################################################
 #here you extract the hyperspectral data from the cube by the spatial points of the tree. Hopefully.
  if(length(trees_in) >0){
    spectra <- raster::extract(cube_no_shade,trees_in,df=T, sp=T)
    spectra_df[[k]] <- as.data.frame(spectra@data)
  }else{
    spectra_df[[4]] <- NULL
  }
}



### combine and save
spectra_all <- do.call(rbind, spectra_df)
head(spectra_all[ ,1:10])


dim(spectra_all)

## make a 'long' dada
ldada<-gather(spectra_all, "wvl","refl",6:350)
ldada$wvl<-as.numeric(gsub(".*_","",ldada$wvl))
ldada<-na.omit(ldada) # take out NA values- about half were NA 10_3 Ary
ldada$staplo<-paste(ldada$Stand, ldada$Treatment)


# look at number of obs per plot
table(ldada$Treatment, ldada$Stand)/345  
table(is.na(ldada$refl), ldada$Treatment) # but alot are NA




write.csv(spectra_all, file="R_input/actual_tops_2_20_greater_0.1.csv")

