

## 2 x 2 pattern.

### top left:  WREF boundary
### top right, 4 plot RGB with outline plot and 30x30
### bottom left: select plot with CHM showing tree crowns
### bottom right: select plot with shade / non-shade and pixel ID (i.e. in / out)


library(tidycensus)
library(ggplot2)

library(sf)
library(units)
library(here)
library(tidyr)


### Bartlett shapefile

bart <- sf::read_sf("D:/Users/bears/Downloads/S_USA.Experimental_Area_Boundaries/S_USA.Experimental_Area_Boundaries.shp") 
bart <- bart[bart$NAME=="Bartlett Experimental Forest",]
ba <- st_union(bart)

ba <- st_transform(ba, crs=4326)


## stakes
stands <- st_read(file.path("D:/Users/bears/Downloads/Intensive_Bartlett_GIS/Bartlett_intensive_sites.shp"))
stands <- stands[stands$Site=="C9",]

subp <- st_read(file.path("D:/Users/bears/Downloads/Intensive_Bartlett_GIS/Bartlett_intensive_sites_subplots.shp"))
subp <- subp[subp$Site=="C9",]
subp <- st_transform(subp, crs=4326)
plot(subp)


stands <- st_transform(stands, crs=4326)
stands[stands$Plot=="1","Treatment"] <- "Control"
stands[stands$Plot=="2","Treatment"] <- "P"
stands[stands$Plot=="3","Treatment"] <- "N+P"
stands[stands$Plot=="4","Treatment"] <- "N"
stands$Treatment <- factor(stands$Treatment, levels=c("Control","N","P","N+P"))



lit_stands <- data.frame(long = c(-71.2881, -71.318,-71.266, -71.296,-71.273), 
                         lat  = c(44.0646, 44.044, 44.06, 44.057, 44.038),
                         Stand = c("","C1","C2","C7","C9"),
                         name = c("Ameriflux tower","Young stands","Young stands","Mature stands","Mature stands")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) 

library(remotes)
#install_github("cran/ggsn")
library(ggsn)
library(ggrepel)

g1 <- ggplot() + 
  geom_sf(data=ba, fill="lightgreen")+
  geom_sf(data = lit_stands, aes(fill=name, shape=name), size = 4) +
  
  theme_minimal()+
  theme(panel.grid.major = element_blank())+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )+
  labs(fill="", shape="", x="", y="")+
  #   scalebar(lit_stands, dist = 1, dist_unit = "km",
  #          transform = TRUE, model = "WGS84",
  #          location="topright",
  #          st.dist=.1,height=.1,
  #          anchor = c(x = -71.254, y = 44.022))+
  # north(lit_stands, scale=.4, symbol=3,
  #       location="topright",
  #       anchor = c(x = -71.25, y = 44.078)) +
  scale_fill_manual(values=c("Ameriflux tower"="orange", "Young stands"="black","Mature stands"="black"))+
  scale_shape_manual(values=c("Ameriflux tower"= 21, "Young stands" =23, "Mature stands"=24))+
  ggtitle("A. Bartlett Experimental Forest")+
  geom_sf_text(data=lit_stands, aes( label=Stand), nudge_x = 0.004, nudge_y = 0.0015)+
  theme(plot.title = element_text(size=22))

g1

g2 <- ggplot()+
  geom_sf(data=stands,aes( fill=Treatment), col="black")+
  geom_sf(data=subp, col="white", fill=NA)+
  scale_fill_manual(values=c("Control"="black", "N"="blue","P"="red","N+P"="purple"))+
  theme_minimal()+
  labs(fill="", shape="", x="", y="")+
  ggtitle("B. Example plot layout")+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )+theme(panel.grid.major = element_blank())+
  # scalebar(stands, dist = 25, dist_unit = "m",
  #          transform = TRUE, model = "WGS84",
  #          st.dist=.05,height=.04,
  #          location="topright",
  #          anchor = c(x = -71.2780, y = 44.0429))+
  theme(plot.title = element_text(size=22))


g2





















############
library(ggplot2)
library(sf)
library(raster)
library(rasterVis)
library(here)
library(neonUtilities)
library(raster)

wd <- here::here()

# read in shapefile of plot locations
stands<-st_read(here::here("data_folder","private_melnhe_locations","Bartlett_intensive_sites_30x30.shp"))
tops <- st_read(here::here("data_folder","private_melnhe_locations","bart_ttops_2025_03_09.shp"))


sh <- read.csv(here::here("data_folder","melnhe_input_files","stand_heights.csv"))
res <- read.csv(here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))

res$staplo <- paste(res$Stand, res$Plot)

res$Treatment<-sapply(res$staplo,switch,
                      "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                      "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                      "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                      "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                      "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                      "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                      "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                      "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                      "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N")





#tinv <- read.csv(here::here("data_folder","ten_plus_DBH_2019.csv"))

# Set the CRS to WGS 1984, Zone 19N
stands <- st_transform(stands, 32619)

# add inm treatments
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
rm(stdf)


#### 6 figures. 
# first row RGB
#  Then the DSM
#  Then CHM with tree tops.  Bottom row is the spectra from each processing step. 

C3<-stands[stands$stand=="C3",]
# C5<-stands[stands$stand=="C5",]
# C6<-stands[stands$stand=="C6",]
# C7 <-stands[stands$stand=="C7",]

### get centroids
centroids <-  st_coordinates(st_centroid(C3))
east <- centroids[, 1]
north <-centroids[, 2]

# ## Download data from NEON if needed.
# # Lidar CHM
# byTileAOP(dpID="DP3.30015.001", site="BART",
#           year="2019", easting=east,
#           northing=north,
#           buffer=500, savepath="data_folder")
# 
# 
# ## this downloads 15 cm Rgb data for the whole site.
# byTileAOP("DP3.30010.001", site="BART", year="2019",buffer = 200,
#           easting=east, northing=north,
#           savepath="data_folder")




# Read in chm
lidar_path <- file.path(wd, "data_folder","DP3.30015.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","CanopyHeightModelGtif")

chm.C3<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_316000_4878000_CHM.tif"))
#chm.C5<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_314000_4878000_CHM.tif"))
#chm.C6<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_317000_4878000_CHM.tif"))
#chm.C7<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_315000_4880000_CHM.tif"))


## Source  DSM
getwd()

#source("code/misc_code_ay_cleanup/get_DSM_C3.R")

# Read in rgb IMAGE
pic_path <- file.path(wd, "data_folder","DP3.30010.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","Camera","Mosaic")

pic.C3<-stack(file.path(pic_path, "2019_BART_5_316000_4878000_image.tif"))
#pic.C5<-stack(file.path(pic_path, "2019_BART_5_314000_4878000_image.tif"))
#pic.C6<-stack(file.path(pic_path,"2019_BART_5_317000_4878000_image.tif"))
#pic.C7<-stack(file.path(pic_path,"2019_BART_5_315000_4880000_image.tif"))


pic <-  pic.C3
chm <-  chm.C3
stand <- C3



## Adjust the area of the bounding box
extend <- 30
yPlus <- extent(stand)[4] + extend
xPlus <- extent(stand)[2] + extend
yMinus <-extent(stand)[3] - extend
xMinus <-extent(stand)[1] - extend

# Example coordinates for four points
points_df <- data.frame(
  id = c(1, 2, 3, 4),
  x = c( xMinus, xMinus, xPlus, xPlus),
  y = c( yMinus, yPlus, yMinus, yPlus)
)


# Convert the data frame to an sf object
stand_view <- st_as_sf(points_df, coords = c("x", "y"))
# Assign WGS 84 CRS
st_crs(stand_view) <- st_crs(4326)

stand_box <- st_bbox(stand_view)

zoom.pic <- crop(pic, stand_box)

zoom.chm <- crop(chm, stand_box)



###########################################

par(mfrow=c(1,3))

plotRGB(zoom.pic,
        r = 1, g = 2, b = 3,
        scale = 150, stretch = "lin" , scales=F)

plot(stand, col = 'transparent',
     border = c("black","blue","red","purple"),
     lwd = 4, add = TRUE)


#################


# Now the DSM
plot(zoom.dsm, axes=F)
plot(stand, col='transparent',
     border = c("black","blue","red","purple"),
     lwd=4, add=TRUE)

## Now the CHM for the stand

plot(zoom.chm, axes=F)

plot(stand, col='transparent', 
     border = c("black","blue","red","purple"),
     lwd=4,add=T)

plot(trees, add=T)


############################



######### next set of figures is a zoom into a plot


# Step one, zoom in with RGB and 1 plot, show tree top points.

# step 2.  plot CHM and tree polys , show tree polygons and points
### CHM


# step 3.   Show DSM crop of the RGB image, \
# Create polygon crown map
C3n_crownsPoly <- mcws(treetops = m3ntops, CHM = tch3, format = "polygons", minHeight = 1.5, verbose = FALSE)




