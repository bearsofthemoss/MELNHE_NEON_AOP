### Anna K. Schweiger 9-22-2020
### Alex Young 9-22-2020

library(sf)
library(stringr) # this is for data management
library(tidyr)
library(ggplot2)
library(terra)
library(rhdf5)
library(neonUtilities)


source(here::here("code","band2raster.R"))

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("rhdf5")


# brightness normalization function
bright_norm <- function(x){
  x_norm <- x/sqrt(sum(x^2))
  return(x_norm)}

# read in shapefile of plot locations
stands<-st_read(here::here("data_folder","private_melnhe_locations","Bartlett_intensive_sites_30x30.shp"))

# Set the CRS to WGS 1984, Zone 19N

plots_UTM <- sf::st_transform(stands, crs = "EPSG:32619")


# Alex's tree tops
trees <- st_read(here::here("data_folder","private_melnhe_locations","bart_ttops_2025_05_21.shp"))
trees <- sf::st_transform(stands, crs = "EPSG:32619")

centroids <-  st_coordinates(st_centroid(stands))


# these are the easting and northings for the stand locations
east <- centroids[, 2]
north <-centroids[, 1]


# # Download the hypersepctral data
# byTileAOP(dpID="DP3.30006.001",site="BART", easting= east,
#           northing = north,
#           year="2019",buffer = 200, savepath = "data_folder/Bart_tiles/",check.size = T)


# Download DSMs
# byTileAOP(dpID="DP3.30024.001",site="BART",
#           year="2019", easting= east,
#           northing = north,
#           buffer=200, savepath = "./data_folder/Bart_DSM/",check.size = T)


# Folder working directories
ff <- list.files("data_folder/Bart_tiles/DP3.30006.001/neon-aop-products/2019/",pattern = ".h5", recursive = T, full.names = T)
dd <- list.files("data_folder/Bart_DSM/DP3.30024.001/neon-aop-products/2019/",pattern = "DSM.tif", recursive = T, full.names = T)



############################################################################################
#### Begin hyperspectral data management
# Extract image information
spectra_df <- list()


# Start for loop ####

# for (k in 1:3){

k <- 13 # For C7

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
  myCRS <- st_crs(paste0("EPSG:", spInfo$`EPSG Code`))
  
  reso <- as.numeric(mapInfo[2]) ### resolution
  
  ### extent
  # left side x coordinate (xMin)
  xMin <- as.numeric(mapInfo[4]) 
  # top corner coordinate (yMax)
  yMax <- as.numeric(mapInfo[5])
  
  reflInfo$Dimension_Labels
  reflInfo$Dimensions
  
  xMax <- (xMin + reflInfo$Dimensions[2]*reso)
  yMin <- (yMax - reflInfo$Dimensions[1]*reso)
  
  # Instead of extent() from raster, use ext() from terra
  rasExt <- ext(xMin, xMax, yMin, yMax)
  
  # Read all bands and create raster stack 
  a <- 1:length(wvl) ### list of bands to be stacked
  all_wvls <- list()
  for(i in 1:length(wvl)){
    all_wvls[[i]] <- a[i]
  }
  
  source(here::here("code","band2raster.R"))
  
  # this takes a minute
  cube_rast <- lapply(all_wvls,band2raster, path_h5 = f,NoDataValue = myNoDataValue,
                      xMin = xMin, yMax = yMax,
                      res=reso, crs = myCRS, ncols =  reflInfo$Dimensions[2],
                      nrows =  reflInfo$Dimensions[1])
  
  hsiStack <- stack(cube_rast)
  
  # make list of band names 
  bandNames <- paste("Band_", round (wvl,digits = 2),sep="")
  names(hsiStack) <- bandNames ### Raster does not safe band names
  
  crs(hsiStack)
  # Set the CRS to UTM Zone 19N (EPSG:32619)
  crs <- CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
  projection(hsiStack) <- crs
  
  #########################################################
  
  #########################################################
  
  #########################################################
  ## index hyperspectral .h5 file 'f', call it nami. Use the 11th slot to match the .h5 tile to the chm .tif file 
  nami<-sapply(strsplit(f,"/"),"[",12)  
  nami
  

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

  
  #For Anna's wd: save if needed
  # writeRaster(ndvi_calc, file= paste0("./R_output/Bart_tiles_processed/", nami,"_NDVI.tif"), 
  #             format="GTiff", overwrite=T)
  
  # read again 
  # ndvi_calc <- raster(paste0("./R_output/Bart_tiles_processed/", nami,"_NDVI.tif"))
  h5closeAll()

## 
  
  ### Now that the tile is processed, we need to
  # Remove water absorption bands, index for good bands
  good <- which((as.numeric(wvl)>400 & as.numeric(wvl)<1340|
                   as.numeric(wvl)>1445 & as.numeric(wvl)<1790|
                   as.numeric(wvl)>1955 & as.numeric(wvl)<2400)==T)
  
  ### subset bands
  cube_wat <- raster::subset(hsiStack, good)
  
  ### NDVI mask
  ndvi_lim <- ndvi_calc >= 0.7 # set NDVI threshold, could be 0.6
   plot(ndvi_lim)
   plot(plots_UTM, add=T)

  # mask bands, takes a minute
  cube_masked <- raster::mask(cube_wat, ndvi_lim, maskvalue = FALSE)

  # brightness normalization
  cube_norm <- raster::calc(cube_masked, fun=bright_norm)
  names(cube_norm) <- names(cube_masked)
  
  #plotRGB(cube_norm,r = 52, g = 28, b = 10, stretch = 'lin',colNA="red")
  
  ### Shade mask
  nam_d <- gsub("_reflectance.h5", "", nami) ## get coordinates of matching tile
  dsm <- rast(dd[grep(nam_d,dd)])

  dsm_slope <- terrain(dsm,v="slope")
  dsm_aspect <- terrain(dsm,v="aspect")
  
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
  
  dsm_shade <- shade(dsm_slope, dsm_aspect, angle = zenith, direction = azimuth)
  
    
  ############################
  # Find ideal threshold
  shade_mask <- dsm_shade >= 0.5
  
  hist(dsm_shade$layer)
  
  hist(shade_mask$layer)
  ###################
  
  
  # Apply to processed images
  cube_no_shade <- raster::mask(cube_norm, shade_mask, maskvalue = 0)

  
  ################################################################################################
  ################################################################################################
  # Extract data
  
  trees_proj <- sf::st_transform(trees, crs = crs(cube_norm))

  # Check CRS compatibility (convert both to strings for comparison)
  raster_crs <- as.character(crs(cube_norm))
  trees_crs <- as.character(st_crs(trees_proj)$wkt)

  # Extract spectral values for tree points
  if(length(trees_proj) >= 1) {
    spectra <- raster::extract(cube_no_shade, trees_proj, df = TRUE, sp = TRUE)
    
    # Convert to data frame and store
    spectra_df[[k]] <- as.data.frame(spectra@data)
    
    # Add file identifier to track which H5 file the data came from
    file_id <- gsub("_reflectance.h5", "", basename(f))
    spectra_df[[k]]$file_id <- file_id
    
    cat("  Successfully extracted spectra for", nrow(spectra_df[[k]]), "trees\n")
    
  } else {
    cat("  No tree points found in this tile\n")
    spectra_df[[k]] <- NULL
  }
  
  

cat("Completed iteration k =", k, "\n\n")
}

# ============================================================================
# COMBINE AND SAVE RESULTS
# ============================================================================

summary(as.data.frame(spectra_df))

cat("Combining results from all files...\n")

# Remove NULL entries
spectra_df <- spectra_df[!sapply(spectra_df, is.null)]

if (length(spectra_df) > 0) {
  # Combine all spectral data
  combined_spectra <- do.call(rbind, spectra_df)
  
  # Remove rows with all NA spectral values
  spectral_cols <- grep("^Band_", names(combined_spectra), value = TRUE)
  valid_rows <- rowSums(!is.na(combined_spectra[spectral_cols])) > 0
  combined_spectra <- combined_spectra[valid_rows, ]
  
  # Write to CSV
  output_file <- "tree_spectra_processed.csv"
  write.csv(combined_spectra, output_file, row.names = FALSE)
  
  cat("=== PROCESSING COMPLETE ===\n")
  cat("Results saved to:", output_file, "\n")
  cat("Total trees processed:", nrow(combined_spectra), "\n")
  cat("Number of spectral bands:", length(spectral_cols), "\n")
  cat("Wavelength range: ~", 
      min(as.numeric(gsub("Band_", "", spectral_cols))), "-", 
      max(as.numeric(gsub("Band_", "", spectral_cols))), "nm\n")
  cat("Files successfully processed:", length(spectra_df), "of", length(h5_files), "\n")
  
  # Show sample of the data
  cat("\nFirst few rows and columns:\n")
  print(combined_spectra[1:min(5, nrow(combined_spectra)), 1:min(8, ncol(combined_spectra))])
  
} else {
  cat("No valid spectral data was extracted from any files.\n")
}

  
   ###########
  

# 
 write.csv(spectra_all, file=here::here("data_folder","actual_tops_2025_05_23.csv"))
# 
# # Also write out the df of spectral data
# shade_mask_spec_df
# 
# head(all_spec_df)
# write.csv(all_spec_df, file="data_folder/all_spec_df.csv")
# 
# head(ndvi_mask_spec_df)
# write.csv(ndvi_mask_spec_df, file="data_folder/ndvi_mask_spec_df.csv")
# 
# head(norm_spec_df)
# write.csv(norm_spec_df, file="data_folder/norm_spec_df.csv")
# 
# head(shade_mask_spec_df)
# write.csv(shade_mask_spec_df, file="data_folder/shade_mask_spec_df.csv")

