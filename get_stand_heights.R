

####  Alex Young  12-28-2023
library(neonUtilities)
library(ForestTools)
library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(stringr) # this is for data management
library(tidyr)
# Set global option to NOT convert all character variables to factors
options(stringsAsFactors=F)


library(here)
wd <- here::here()

# This code relies heavily on the ForestTools package- helpful information here
# https://cran.r-project.org/web/packages/ForestTools/vignettes/treetopAnalysis.html

# read in shapefile of plot locations
stands<-st_read(file.path("data_folder","Bartlett_intensive_sites_30x30.shp"))

# Set the CRS to WGS 1984, Zone 19N
stands <- st_transform(stands, 32619)

# centroids are the 'plot centers'. code for Lidar tiles works with point data
centroids <-  st_coordinates(st_centroid(stands))


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

## Read in chm data,  start fig 1



# east <- centroids[, 1]
# north <-centroids[, 2]
#
# the lidar chm
# byTileAOP(dpID="DP3.30015.001", site="BART", 
#           year="2019", easting=east,
#           northing=north,
#           buffer=500, savepath="data_folder")




lidar_path <- file.path(wd, "data_folder","DP3.30015.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","CanopyHeightModelGtif")

chm.C1a<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_313000_4879000_CHM.tif"))
chm.C1b<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_314000_4879000_CHM.tif"))
chm.C1 <- raster::merge(chm.C1a,chm.C1b)
chm.C2a<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_318000_4881000_CHM.tif"))
chm.C2b<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_318000_4880000_CHM.tif"))
chm.C2 <- raster::merge(chm.C2a,chm.C2b)
chm.C3<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_316000_4878000_CHM.tif"))
chm.C4<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_318000_4880000_CHM.tif"))
chm.C5<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_314000_4878000_CHM.tif"))
chm.C6<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_317000_4878000_CHM.tif"))
chm.C7<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_315000_4880000_CHM.tif"))
chm.C8a<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_315000_4880000_CHM.tif"))
chm.C8b<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_316000_4880000_CHM.tif"))
chm.C8 <- raster::merge(chm.C8a,chm.C8b)
chm.C9<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_317000_4879000_CHM.tif"))

plot(stands[6], add=T)
plot(chm.C1)

plot(chm.C9)

rast <- merge(chm.C1, chm.C2, chm.C3,
              chm.C4, chm.C5, chm.C6,
              chm.C7, chm.C8, chm.C9)



st_crs(chm.C9)
st_crs(stands)

# loo# loo# look through the 9
stand_list <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9")


out <- list()

for(i in 1:9){
  choose_stand <- stand_list[i]

  
extracted_data <- extract(rast, stands[stands$stand== choose_stand, ], df=TRUE)

merged_data <- merge(stands[stands$stand== choose_stand, ], extracted_data, by.x = "plot", by.y = "ID")

mdf <- as.data.frame(merged_data[ ,1:8])

#ggplot(merged_data, aes(x=plot, y=layer))+geom_boxplot()

out <- rbind(out, mdf )

}



head(out)
dim(out)
table(out$stand, out$plot)

out$Treatment<- factor(out$Treatment, levels=c("Control","N","P","NP"))

ggplot(out, aes(x=stand, y=layer,group=unique_plo , fill=Treatment))+
         geom_boxplot()+
  scale_fill_manual(values=c("black","blue","red","purple"))

names(out)
stand_heights <- as.data.frame(out[ , 1:8])


write.csv(stand_heights, file="R_output/stand_heights.csv")
