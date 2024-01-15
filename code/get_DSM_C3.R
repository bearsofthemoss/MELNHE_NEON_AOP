
### DSM routine


### Shade mask
###  example use C3
 nami <- "NEON_D01_BART_DP3_316000_4878000_reflectance.h5" 
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

zoom.dsm <- crop(dsm_shade, stand_box ) 

# choose threshold and select NA space to show
# zoom.dsm <- zoom.dsm >=0.1

#plot(zoom.dsm)


