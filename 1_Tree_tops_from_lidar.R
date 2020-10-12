
####  Alex Young  9-7-2020
library(neonUtilities)
library(ForestTools)
library(raster)
library(rgdal)
library(rgeos)
#library(dplyr)


# Set wd intentionally
setwd("C:\\Users\\aryoung\\Desktop\\mel_NEON")

# Set global option to NOT convert all character variables to factors
options(stringsAsFactors=F)


# read in shapefile
stands<-readOGR("data_files\\plot_shp","Bartlett_intensive_sites_30x30")
stands=spTransform(stands,CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs
+ellps=WGS84 +towgs84=0,0,0")) # To convert it to WGS8

stdf<-as.data.frame(stands)
stdf$staplo<-paste(stdf$stand, stdf$plot)
stdf
stands$Treatment<-sapply(stdf[ ,7],switch,
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

#################################   Each stand as a shapefile
C1<-stands[stands$stand=="C1",]
C2<-stands[stands$stand=="C2",]
C3<-stands[stands$stand=="C3",]
C4<-stands[stands$stand=="C4",]
C5<-stands[stands$stand=="C5",]
C6<-stands[stands$stand=="C6",]
C7<-stands[stands$stand=="C7",]
C8<-stands[stands$stand=="C8",]
C9<-stands[stands$stand=="C9",]

############################################################################################################

# Now to read in NEon data 
## these are the coordinates for the N and P addition plots in MELNHE. Used for downloading lidar from neonUtilities
easting<-c(  313000, 314000,   31800,31800,316000 ,318000,314000 ,317000, 315000 ,315000 ,316000,317000  )
northing<-c(4879000,4879000, 4881000,488000,4878000 ,4880000  ,4878000 ,4878000,4880000 ,4880000,4880000,4879000   )

# just C6
easting<-317000
northing<-4878000

# this downloads the data and saves it to your specified directory.
## # this will ask you if you want to download the files to your computer
byTileAOP("DP3.30015.001", site="BART", year="2017", check.size = T,buffer = 200, easting=easting, northing=northing, savepath="C:\\Users\\aryoung\\Desktop\\mel_NEON\\Downloads")

# once you download them, you'll need to set wd and tead each file.  Here they are.
# setwd()    # enter your wd.
chm.C1a<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_313000_4879000_CHM.tif")
chm.C1b<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_314000_4879000_CHM.tif")
chm.C1 <- raster::merge(chm.C1a,chm.C1b)
chm.C2a<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_318000_4881000_CHM.tif")
chm.C2b<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_318000_4880000_CHM.tif")
chm.C2 <- raster::merge(chm.C2a,chm.C2b)
chm.C3<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_316000_4878000_CHM.tif")
chm.C4<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_318000_4880000_CHM.tif")
chm.C5<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_314000_4878000_CHM.tif")
chm.C6<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_317000_4878000_CHM.tif")
chm.C7<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_315000_4880000_CHM.tif")
chm.C8a<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_315000_4880000_CHM.tif")
chm.C8b<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_316000_4880000_CHM.tif")
chm.C8 <- raster::merge(chm.C8a,chm.C8b)
chm.C9<-raster("Downloads\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_317000_4879000_CHM.tif")




## this makes a canopy height per plot
############################################
m1c<-crop(chm.C1, C1[C1$treat=="Control",])
m1n<-crop(chm.C1, C1[C1$treat=="N",])
m1p<-crop(chm.C1, C1[C1$treat=="P",])
m1np<-crop(chm.C1, C1[C1$treat=="NP",])
############################################
m2c<-crop(chm.C2, C2[C2$treat=="Control",])
m2n<-crop(chm.C2, C2[C2$treat=="N",])
m2p<-crop(chm.C2, C2[C2$treat=="P",])
m2np<-crop(chm.C2, C2[C2$treat=="NP",])
############################################
m3c<-crop(chm.C3, C3[C3$treat=="Control",])
m3n<-crop(chm.C3, C3[C3$treat=="N",])
m3p<-crop(chm.C3, C3[C3$treat=="P",])
m3np<-crop(chm.C3, C3[C3$treat=="NP",])
############################################
m4c<-crop(chm.C4, C4[C4$treat=="Control",])
m4n<-crop(chm.C4, C4[C4$treat=="N",])
m4p<-crop(chm.C4, C4[C4$treat=="P",])
m4np<-crop(chm.C4, C4[C4$treat=="NP",])
############################################
m5c<-crop(chm.C5, C5[C5$treat=="Control",])
m5n<-crop(chm.C5, C5[C5$treat=="N",])
m5p<-crop(chm.C5, C5[C5$treat=="P",])
m5np<-crop(chm.C5, C5[C5$treat=="NP",])
############################################
m6c<-crop(chm.C6, C6[C6$treat=="Control",])
m6n<-crop(chm.C6, C6[C6$treat=="N",])
m6p<-crop(chm.C6, C6[C6$treat=="P",])
m6np<-crop(chm.C6, C6[C6$treat=="NP",])
############################################
m7c<-crop(chm.C7, C7[C7$treat=="Control",])
m7n<-crop(chm.C7, C7[C7$treat=="N",])
m7p<-crop(chm.C7, C7[C7$treat=="P",])
m7np<-crop(chm.C7, C7[C7$treat=="NP",])
############################################
m8c<-crop(chm.C8, C8[C8$treat=="Control",])
m8n<-crop(chm.C8, C8[C8$treat=="N",])
m8p<-crop(chm.C8, C8[C8$treat=="P",])
m8np<-crop(chm.C8, C8[C8$treat=="NP",])
###########################################
m9c<-crop(chm.C9, C9[C9$treat=="Control",])
m9n<-crop(chm.C9, C9[C9$treat=="N",])
m9p<-crop(chm.C9, C9[C9$treat=="P",])
m9np<-crop(chm.C9, C9[C9$treat=="NP",])
#####################################

### define variable window function
#0.05, and 0.6 are defaults

### this is important!  I adjusted trees here, based on 

lin.C1 <- function(x){x * 0.002}
lin.C2 <- function(x){x * 0.002}
lin.C3 <- function(x){x * 0.002}
lin.C4 <- function(x){x * 0.02 + .5}
lin.C5 <- function(x){x * 0.02 + .5}
lin.C6 <- function(x){x * 0.02 + .5}
lin.C7 <- function(x){x * 0.02 + .5}
lin.C8 <- function(x){x * 0.02 + .5}
lin.C9 <- function(x){x * 0.02 + .5}

#############################################################################
# start automating tree tops
#############################################################################
plot(chm.C1)
plot(C1, add=T)

m1c <- mask(crop(chm.C1, extent(C1[C1$Treatment=="Control",])), C1[C1$Treatment=="Control",])
m1n <- mask(crop(chm.C1, extent(C1[C1$Treatment=="N",])),C1[C1$Treatment=="N",] )
m1p <- mask(crop(chm.C1, extent(C1[C1$Treatment=="P",])),C1[C1$Treatment=="P",] )
m1np <- mask(crop(chm.C1, extent(C1[C1$Treatment=="NP",])),C1[C1$Treatment=="NP",] )
m1ctops <- vwf(CHM = m1c, winFun = lin.C1, minHeight = 12)
m1ctops$Treatment<-"Control"
m1ntops <- vwf(CHM = m1n, winFun = lin.C1, minHeight = 3)
m1ntops$Treatment<-"N"
m1ptops <- vwf(CHM = m1p, winFun = lin.C1, minHeight = 3)
m1ptops$Treatment<-"P"
m1nptops<- vwf(CHM = m1np,winFun = lin.C1, minHeight = 3)
m1nptops$Treatment<-"NP"
##
m2c <- mask(crop(chm.C2, extent(C2[C2$Treatment=="Control",])), C2[C2$Treatment=="Control",])
m2n <- mask(crop(chm.C2, extent(C2[C2$Treatment=="N",])),C2[C2$Treatment=="N",] )
m2p <- mask(crop(chm.C2, extent(C2[C2$Treatment=="P",])),C2[C2$Treatment=="P",] )
m2np <- mask(crop(chm.C2, extent(C2[C2$Treatment=="NP",])),C2[C2$Treatment=="NP",] )
m2ctops <- vwf(CHM = m2c, winFun = lin.C2, minHeight = 3)
m2ctops$Treatment<-"Control"
m2ntops <- vwf(CHM = m2n, winFun = lin.C2, minHeight = 3)
m2ntops$Treatment<-"N"
m2n

m2ntops
m2ptops <- vwf(CHM = m2p, winFun = lin.C2, minHeight = 3)
m2ptops$Treatment<-"P"
m2nptops <- vwf(CHM = m2np, winFun = lin.C2, minHeight = 3)
m2nptops$Treatment<-"NP"
##
m3c <- mask(crop(chm.C3, extent(C3[C3$Treatment=="Control",])), C3[C3$Treatment=="Control",])
m3n <- mask(crop(chm.C3, extent(C3[C3$Treatment=="N",])),C3[C3$Treatment=="N",] )
m3p <- mask(crop(chm.C3, extent(C3[C3$Treatment=="P",])),C3[C3$Treatment=="P",] )
m3np <- mask(crop(chm.C3, extent(C3[C3$Treatment=="NP",])),C3[C3$Treatment=="NP",] )
m3ctops <- vwf(CHM = m3c, winFun = lin.C3, minHeight = 3)
m3ctops$Treatment<-"Control"
m3ntops <- vwf(CHM = m3n, winFun = lin.C3, minHeight = 3)
m3ntops$Treatment<-"N"
m3ptops <- vwf(CHM = m3p, winFun = lin.C3, minHeight = 3)
m3ptops$Treatment<-"P"
m3nptops <- vwf(CHM = m3np, winFun = lin.C3, minHeight = 3)
m3nptops$Treatment<-"NP"
##
m4c <- mask(crop(chm.C4, extent(C4[C4$Treatment=="Control",])), C4[C4$Treatment=="Control",])
m4n <- mask(crop(chm.C4, extent(C4[C4$Treatment=="N",])),C4[C4$Treatment=="N",] )
m4p <- mask(crop(chm.C4, extent(C4[C4$Treatment=="P",])),C4[C4$Treatment=="P",] )
m4np <- mask(crop(chm.C4, extent(C4[C4$Treatment=="NP",])),C4[C4$Treatment=="NP",] )
m4ctops <- vwf(CHM = m4c, winFun = lin.C4, minHeight = 3)
m4ctops$Treatment<-"Control"
m4ntops <- vwf(CHM = m4n, winFun = lin.C4, minHeight = 3)
m4ntops$Treatment<-"N"
m4ptops <- vwf(CHM = m4p, winFun = lin.C4, minHeight = 3)
m4ptops$Treatment<-"P"
m4nptops <- vwf(CHM = m4np, winFun = lin.C4, minHeight = 3)
m4nptops$Treatment<-"NP"
##
m5c <- mask(crop(chm.C5, extent(C5[C5$Treatment=="Control",])), C5[C5$Treatment=="Control",])
m5n <- mask(crop(chm.C5, extent(C5[C5$Treatment=="N",])),C5[C5$Treatment=="N",] )
m5p <- mask(crop(chm.C5, extent(C5[C5$Treatment=="P",])),C5[C5$Treatment=="P",] )
m5np <- mask(crop(chm.C5, extent(C5[C5$Treatment=="NP",])),C5[C5$Treatment=="NP",] )
m5ctops <- vwf(CHM = m5c, winFun = lin.C5, minHeight = 3)
m5ctops$Treatment<-"Control"
m5ntops <- vwf(CHM = m5n, winFun = lin.C5, minHeight = 3)
m5ntops$Treatment<-"N"
m5ptops <- vwf(CHM = m5p, winFun = lin.C5, minHeight = 3)
m5ptops$Treatment<-"P"
m5nptops <- vwf(CHM = m5np, winFun = lin.C5, minHeight = 3)
m5nptops$Treatment<-"NP"
##
m6c <- mask(crop(chm.C6, extent(C6[C6$Treatment=="Control",])), C6[C6$Treatment=="Control",])
m6n <- mask(crop(chm.C6, extent(C6[C6$Treatment=="N",])),C6[C6$Treatment=="N",] )
m6p <- mask(crop(chm.C6, extent(C6[C6$Treatment=="P",])),C6[C6$Treatment=="P",] )
m6np <- mask(crop(chm.C6, extent(C6[C6$Treatment=="NP",])),C6[C6$Treatment=="NP",] )
m6ctops <- vwf(CHM = m6c, winFun = lin.C6, minHeight = 3)
m6ctops$Treatment<-"Control"
m6ntops <- vwf(CHM = m6n, winFun = lin.C6, minHeight = 3)
m6ntops$Treatment<-"N"
m6ptops <- vwf(CHM = m6p, winFun = lin.C6, minHeight = 3)
m6ptops$Treatment<-"P"
m6nptops <- vwf(CHM = m6np, winFun = lin.C6, minHeight = 3)
m6nptops$Treatment<-"NP"
##
m7c <- mask(crop(chm.C7, extent(C7[C7$Treatment=="Control",])), C7[C7$Treatment=="Control",])
m7n <- mask(crop(chm.C7, extent(C7[C7$Treatment=="N",])),C7[C7$Treatment=="N",] )
m7p <- mask(crop(chm.C7, extent(C7[C7$Treatment=="P",])),C7[C7$Treatment=="P",] )
m7np <- mask(crop(chm.C7, extent(C7[C7$Treatment=="NP",])),C7[C7$Treatment=="NP",] )
m7ctops <- vwf(CHM = m7c, winFun = lin.C7, minHeight = 3)
m7ctops$Treatment<-"Control"
m7ntops <- vwf(CHM = m7n, winFun = lin.C7, minHeight = 3)
m7ntops$Treatment<-"N"
m7ptops <- vwf(CHM = m7p, winFun = lin.C7, minHeight = 3)
m7ptops$Treatment<-"P"
m7nptops <- vwf(CHM = m7np, winFun = lin.C7, minHeight = 3)
m7nptops$Treatment<-"NP"
##
m8c <- mask(crop(chm.C8, extent(C8[C8$Treatment=="Control",])), C8[C8$Treatment=="Control",])
m8n <- mask(crop(chm.C8, extent(C8[C8$Treatment=="N",])),C8[C8$Treatment=="N",] )
m8p <- mask(crop(chm.C8, extent(C8[C8$Treatment=="P",])),C8[C8$Treatment=="P",] )
m8np <- mask(crop(chm.C8, extent(C8[C8$Treatment=="NP",])),C8[C8$Treatment=="NP",] )
m8ctops <- vwf(CHM = m8c, winFun = lin.C8, minHeight = 3)
m8ctops$Treatment<-"Control"
m8ntops <- vwf(CHM = m8n, winFun = lin.C8, minHeight = 3)
m8ntops$Treatment<-"N"
m8ptops <- vwf(CHM = m8p, winFun = lin.C8, minHeight = 3)
m8ptops$Treatment<-"P"
m8nptops <- vwf(CHM = m8np, winFun = lin.C8, minHeight = 3)
m8nptops$Treatment<-"NP"
##
m9c <- mask(crop(chm.C9, extent(C9[C9$Treatment=="Control",])), C9[C9$Treatment=="Control",])
m9n <- mask(crop(chm.C9, extent(C9[C9$Treatment=="N",])),C9[C9$Treatment=="N",] )
m9p <- mask(crop(chm.C9, extent(C9[C9$Treatment=="P",])),C9[C9$Treatment=="P",] )
m9np <- mask(crop(chm.C9, extent(C9[C9$Treatment=="NP",])),C9[C9$Treatment=="NP",] )
m9ctops <- vwf(CHM = m9c, winFun = lin.C9, minHeight = 3)
m9ctops$Treatment<-"Control"
m9ntops <- vwf(CHM = m9n, winFun = lin.C9, minHeight = 3)
m9ntops$Treatment<-"N"
m9ptops <- vwf(CHM = m9p, winFun = lin.C9, minHeight = 3)
m9ptops$Treatment<-"P"
m9nptops <- vwf(CHM = m9np, winFun = lin.C9, minHeight = 3)
m9nptops$Treatment<-"NP"



#######################################################################
## is there a way to combine chapefiles in R?
# maybe rbind will do the trick.
C1_ttops <- rbind(m1ctops, m1ntops, m1ptops, m1nptops)
C1_ttops$Stand<-"C1"
C2_ttops <- rbind(m2ctops, m2ntops, m2ptops, m2nptops)
C2_ttops$Stand<-"C2"
C3_ttops <- rbind(m3ctops, m3ntops, m3ptops, m3nptops)
C3_ttops$Stand<-"C3"
C4_ttops <- rbind(m4ctops, m4ntops, m4ptops, m4nptops)
C4_ttops$Stand<-"C4"
C5_ttops <- rbind(m5ctops, m5ntops, m5ptops, m5nptops)
C5_ttops$Stand<-"C5"
C6_ttops <- rbind(m6ctops, m6ntops, m6ptops, m6nptops)
C6_ttops$Stand<-"C6"
C7_ttops <- rbind(m7ctops, m7ntops, m7ptops, m7nptops)
C7_ttops$Stand<-"C7"
C8_ttops <- rbind(m8ctops, m8ntops, m8ptops, m8nptops)
C8_ttops$Stand<-"C8"
C9_ttops <- rbind(m9ctops, m9ntops, m9ptops, m9nptops)
C9_ttops$Stand<-"C9"

table(C9_ttops$Treatment)

## do another round of rbinding.
bart_ttops<-rbind(C1_ttops, C2_ttops, C3_ttops,
                  C4_ttops, C5_ttops, C6_ttops,
                  C7_ttops, C8_ttops, C9_ttops)

plot(bart_ttops)

plot(chm.C1, add=T)
plot(chm.C2, add=T)
plot(chm.C3, add=T)
plot(chm.C4, add=T)
plot(chm.C5, add=T)
plot(chm.C6, add=T)
plot(chm.C7, add=T)
plot(chm.C8, add=T)
plot(chm.C9, add=T)
plot(bart_ttops, add=T)

######################################################################################################

# write the shapefile
#dir.create("write_shape_from_R")
#writeOGR(obj=bart_ttops,dsn="write_shape_from_R"  ,layer="bart_ttops_6_22_2020", driver="ESRI Shapefile")
