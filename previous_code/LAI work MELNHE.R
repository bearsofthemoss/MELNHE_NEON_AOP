
# get LAI for 3 stands
library(sp)
library(rgdal)
library(stringr) # this is for data management
library(tidyr)
library(ggplot2)
library(raster)
library(neonUtilities)
library(rgdal)
library(rgeos)


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



 east <- centroids[, 1]
 north <-centroids[, 2]

# 
 byTileAOP(dpID="DP3.30012.001",site="BART",
            year="2017", easting= east,
            northing = north,
            buffer=70, savepath = "LAI",check.size = T)



#lf<-list.files(path="LAI\\DP3.30012.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Spectrometer\\LAI", recursive = T, full.names = T
               
lf<-list.files(path="LAI\\DP3.30012.001\\neon-aop-products\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Spectrometer\\LAI", recursive = T, full.names = T)


l8<-raster(lf[1])
l7<-raster(lf[1])
l9<-raster(lf[5])


par(mfrow=c(1,3))
plot(C7, main="Stand C7")
plot(l8, add=T)
plot(C7, add=T)

plot(C8, main="Stand C8")
plot(l8, add=T)
plot(C8, add=T)

plot(C9, main="Stand C9")
plot(l9, add=T)
plot(C9, add=T)



#3 
lai<-merge(l8, l9 )


plot(lai)
plot(old, add=T)

old$staplo<-paste(old$stand, old$plot)
old

old

head(old)
v1 <- raster::extract( lai, old, fun=mean, na.rm=TRUE)
nom <- sapply(old@polygons , slot, "ID")
nom
v1 <- data.frame(ID = nom, Value = v1)
v1$Stand<-rep(c("C7","C8","C9"), each=4)

old@data
head(v1)
ggplot(v1, aes(x=Stand, y=Value))+geom_point()+
  ggtitle("LAI in C7, C8, C9")+theme_classic()



