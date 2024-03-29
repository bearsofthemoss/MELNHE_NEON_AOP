library(ggplot2)
theme_set(theme_classic())
library(sf)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
#
library(ggmap)
library(gridExtra)
#

library(neonUtilities)
library(ForestTools)
library(raster)
library(rgdal)
library(rgeos)
###########

par(mfrow=c(2,2))
###### PART 1:   Map of NH
world <- ne_countries(scale = "medium", returnclass = "sf")
sites <- data.frame(longitude = c(-71.28731), latitude = c(44.06388 ))
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- cbind(states, st_coordinates(st_centroid(states)))

ggplot(data = world) +  geom_sf() +   geom_sf(data = states, fill = NA) + 
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-75, -67), ylim = c(40, 47), expand = FALSE)



######  PART2:  9 stands color coded plots
# read in shapefile
plots<-readOGR("data_folder","Bartlett_intensive_sites_30x30")

# transform to UTM coordinates
crss <- make_EPSG()
UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))
stands <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))
C7<-stands[stands$stand =="C7",]
# centroids are the 'plot centers'. code for Lidar tiles works with point data
centroids <- as.data.frame(getSpPPolygonsLabptSlots(C7))
east<-centroids$V1
north<-centroids$V2

# this downloads 15 cm Rgb data for the whole site.
byTileAOP("DP3.30010.001", site="BART", year="2017", check.size = F,buffer = 200, 
          easting=east, northing=north, 
          savepath="data_folder")

pic.C7<-stack("data_folder\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_315000_4880000_image.tif")
C7<-stands[stands$stand=="C7",]
control<-C7[C7$plot=="1", ]
control3<-extent(control)+10
co.bart<-crop(pic.C3, control3)



#### plot chm
chm_C7<-raster("data_folder\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_315000_4880000_CHM.tif")
chm7<-crop(chm_C7,control3)
# do treE top detection
lin.C7 <- function(x){x * 0.02}
m7tops <- vwf(CHM = chm7, winFun = lin.C7, minHeight = 2)
C7crownsPoly <- mcws(treetops = m7tops, CHM = chm7, format = "polygons", minHeight = 1.5, verbose = FALSE)


#3 3 band tiff with shade mask
# read in mini_noshade with C7 no shade?

mini<-extent(control3)
mini_noshade <- crop(cube_no_shade,mini)

############################
par(mfrow=c(1,3))
#3333 Rgb image
plotRGB(co.bart)

# add treetops to chm
plot(chm7, axes=F, box=F, legend=F)
plot(C7crownsPoly, border = "white", lwd = 0.5, add = TRUE)
plot(m7tops, add=T, pch=17,col="red", cex=1.5)

plotRGB(mini_noshade, r = 56, g = 28, b = 14, stretch = 'lin')
plot(C7crownsPoly, border = "white", lwd = 0.5, add = TRUE)
plot(m7tops, add=T, pch=17, col="red",cex=1.5)



#################################################################################################################################
## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada<-read.csv("actual_tops_10_04_greater_0.1.csv")
dada<-dada[,-1]

# stand ages
dada$Age[dada$Stand=="C1"]<-"~30 years old"
dada$Age[dada$Stand=="C2"]<-"~30 years old"
dada$Age[dada$Stand=="C3"]<-"~30 years old"
dada$Age[dada$Stand=="C4"]<-"~60 years old"
dada$Age[dada$Stand=="C5"]<-"~60 years old"
dada$Age[dada$Stand=="C6"]<-"~60 years old" 
dada$Age[dada$Stand=="C7"]<-"~100 years old"
dada$Age[dada$Stand=="C8"]<-"~100 years old"
dada$Age[dada$Stand=="C9"]<-"~100 years old"



names(dada)

# make a 'long' version of dada
ldada<-gather(dada, "wvl","refl",7:351)
ldada$wvl<-as.numeric(gsub(".*_","",ldada$wvl))
ldada<-na.omit(ldada) # take out NA values- about half were NA 10_3 Ary
ldada$staplo<-paste(ldada$Stand, ldada$Treatment)


# look at number of obs per plot
table(ldada$Treatment, ldada$Stand)/345  
table(is.na(ldada$refl), ldada$Treatment) # but alot are NA

# min,max, and mean number of tree tops by plot.  6 is probably too low right?
min(table(ldada$staplo))/345
max(table(ldada$staplo))/345
mean(table(ldada$staplo))/345

dim(ldada)

# view individual trees
# Here Alex tried to visualize the spectra---


li<-subset(ldada, ldada$wvl<=1340)
sw<-subset(ldada, ldada$wvl>=1445 & ldada$wvl<=1790)
ir<-subset(ldada, ldada$wvl>=1995)
# check the values
li$group<-"1"
sw$group<-"2"
ir$group<-"3"
#
table(li$wvl)
table(sw$wvl)
table(ir$wvl)
# move forward without 'band' bands
ldada<-rbind(li,sw,ir)
ldada$tree<-paste(ldada$Stand, ldada$Treatment, ldada$treeID)
ldada$group.tree<-paste(ldada$tree, ldada$group)

# Just view 1 stand
C1_ldada<-ldada[ldada$staplo=="C3 Control",]

# Nice!
ggplot(C1_ldada, aes(x=wvl,col=Stand,group=group.tree, y=refl))+geom_line()


