### Alex Young 9-8-2020

library(sp)
library(rgdal)
library(stringr) # this is for data management
library(tidyr)
library(ggplot2)
library(raster)
library(rhdf5)

#### Ideally I'd use the neonUtilities to pull the .h5 file directly for each stand.  The issue I have currently is limited processing power to open multiple .h5 files at once.
#library(neonUtilities)
# these are the easting and northings for the stand locations
easting<-c(  313000, 314000,   31800,31800,316000 ,318000,314000 ,317000, 315000 ,315000 ,316000,317000  )
northing<-c(4879000,4879000, 4881000,488000,4878000 ,4880000  ,4878000 ,4878000,4880000 ,4880000,4880000,4879000   )
#byTileAOP("DP3.30006.001", site="BART", year="2017", check.size = T,buffer = 200, easting=easting, northing=northing, savepath="C:\\Users\\aryoung\\Desktop\\mel_NEON\\Downloads")


## choose .h5 file to analyze stands 
##### I've downloaded them locally. the timestames for the downloaded files are in the file name i.e. 20170814, 154540
#C1   :"data_files\\raw_h5_files_by_stand\\NEON_D01_BART_DP1_20170814_154540_C1_reflectance.h5"
#C2+C4:"data_files\\raw_h5_files_by_stand\\NEON_D01_BART_DP1_20170814_151221_C24_reflectance.h5"
#C3   :"data_files\\raw_h5_files_by_stand\\NEON_D01_BART_DP1_20170814_143341_C3_reflectance.h5"
#C5   :"data_files\\raw_h5_files_by_stand\\NEON_D01_BART_DP1_20170814_153853_C5_reflectance.h5"
#C6try:"data_files\\raw_h5_files_by_stand\\NEON_D01_BART_DP1_20170814_150606_C6_reflectance.h5"
#C7+C8:"data_files\\raw_h5_files_by_stand\\NEON_D01_BART_DP1_20170814_141517_C78_reflectance.h5"
#C9   :"data_files\\raw_h5_files_by_stand\\NEON_D01_BART_DP1_20170814_145952_C9_reflectance.h5"


setwd("C:\\Users\\aryoung\\Desktop\\mel_NEON")
source("R_code_files\\band2raster.R")

# choose the right .h5 file for the right stand.
f<-"data_files\\raw_h5_files_by_stand\\NEON_D01_BART_DP1_20170814_145952_C9_reflectance.h5"

# make sure to choose stand correctly!
trees<-readOGR("write_shape_from_R","bart_ttops_6_22_2020")

# I currently enter the Stand (C1-C9) by hand and process each stand individually
stand.choose<-trees[trees$Stand=="C9",]



############################################################################################
#### Begin hyperspectral data management
# Extract image information
x <- h5ls(f)[grep("Wavelength", h5ls(f)[,2]),]
xx <- paste(x[1],x[2],sep="/")
wvl <- h5read(f,xx)

wvl


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
names(hsiStack) <- bandNames ### Raster does not safe bandnames

### Make NDVI raster layer
# NEON uses bands close to 648.2, 858.6
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
#3
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
# wvl[good]

### subset bands
cube_wat <- raster::subset(hsiStack, good)

### NDVI mask
ndvi_0.9 <- ndvi_calc >= 0.9 # set NDVI threshold, could be 0.6

# mask bands, takes a minute
cube_masked <- raster::mask(cube_wat, ndvi_0.9, maskvalue = FALSE)

# normalize for reflectance
bright_norm <- function(x){
  x_norm <- x/sqrt(sum(x^2))
  return(x_norm)}

cube_norm <- raster::calc(cube_masked, fun=bright_norm)


################################################################################################
################################################################################################


.rs.unloadPackage("tidyr")
# here you extract the hyperspectral data from the cube by the spatial points of the tree. Hopefully.
spectra.c<-as.data.frame(extract(cube_norm,stand.choose[stand.choose$Treatment=="Control",]))
spectra.n<-as.data.frame(extract(cube_norm,stand.choose[stand.choose$Treatment=="N",]))
spectra.p<-as.data.frame(extract(cube_norm,stand.choose[stand.choose$Treatment=="P",]))
spectra.np<-as.data.frame(extract(cube_norm,stand.choose[stand.choose$Treatment=="NP",]))


library(tidyr)
## format for graphing and future analysis.
numextract <- function(string){ str_extract(string, "\\-*\\d+\\.*\\d*")} 

dim(spectra.c)
c<-gather(spectra.c, "Bands","refl",1:345)
c$band.no<-as.numeric(numextract(c$Bands))
c$wavelength<-c$band.no*5+400
c$Treatment<-"Control"


n<-gather(spectra.n, "Bands","refl",1:345)
n$band.no<-as.numeric(numextract(n$Bands))
n$wavelength<-n$band.no*5+400
n$Treatment<-"N"

p<-gather(spectra.p, "Bands","refl",1:345)
p$band.no<-as.numeric(numextract(p$Bands))
p$wavelength<-p$band.no*5+400
p$Treatment<-"P"

np<-gather(spectra.np, "Bands","refl",1:345)
np$band.no<-as.numeric(numextract(np$Bands))
np$wavelength<-np$band.no*5+400
np$Treatment<-"NP"

head(np)

all<-rbind(c, n, p, np)
head(all)


summary(all$refl)
#############################################################################


## Make sure you label it right!
write.csv(all, file="ttop_C9.csv")




