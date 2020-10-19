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
plots<-readOGR("R_input","Bartlett_intensive_sites_30x30")

# transform to UTM coordinates
crss <- make_EPSG()
UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))
stands <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))
# centroids are the 'plot centers'. code for Lidar tiles works with point data
centroids <- as.data.frame(getSpPPolygonsLabptSlots(stands))

# this downloads 15 cm Rgb data for the whole site.
#byFileAOP(dpID = "DP3.30010.001", site = "BART", year = "2017", check.size = T, savepath="R_input")
pic.C3<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_316000_4878000_image.tif")
C3<-stands[stands$stand=="C3",]
control<-C3[C3$plot=="3", ]
control3<-extent(control)+10
co.bart<-crop(pic.C3, control3)


par(mfrow=c(1,1))
#3333 Rgb image
plotRGB(co.bart)

#### plot chm
chm_C3<-raster("R_input\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_316000_4878000_CHM.tif")
chm3<-crop(chm_C3,control3)
plot(chm3)
#3 do treE top detection
lin.C3 <- function(x){x * 0.002}
m3tops <- vwf(CHM = m3chm, winFun = lin.C3, minHeight = 3)
C3crownsPoly <- mcws(treetops = m3tops, CHM = m3chm, format = "polygons", minHeight = 1.5, verbose = FALSE)
# add treetop
plot(C3crownsPoly, border = "black", lwd = 0.5, add = TRUE)
plot(m3tops, add=T, pch=17, cex=1)

## shade mask crop

shade<-raster("C3C_no_shade2.grd")
shade<-crop(shade, co.bart)
plot(shade)
plotRGB(co.bart)
shade_mask <- is.na(shade)
plot(shade_mask)


f_no_shade1 <- raster::mask(co.bart, shade_mask, maskvalue = 0)

#3 how are they different extent?
extent(co.bart)
extent(shade_mask)

###################
# Apply to processed images
cube_no_shade <- raster::mask(cube_norm, shade_mask, maskvalue = 0)




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


