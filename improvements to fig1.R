library(ggplot2)
library(sf)
library(maps)
#
library(ggmap)
library(gridExtra)
#
library(broom)

library(neonUtilities)
library(ForestTools)
library(raster)
library(rgdal)
library(rgeos)
###########

## google account
register_google( )
google_account()
has_google_key()
google_key()  


#### load plots
plots<-readOGR("R_Output","Bartlett_intensive_sites_30x30")

# transform to UTM coordinates
crss <- make_EPSG()
UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))
stands <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))

# make lat long re projection
spgeo <- spTransform(stands, CRS("+proj=longlat +datum=WGS84"))
cen <- as.data.frame(getSpPPolygonsLabptSlots(spgeo))

spgeo$staplo<-paste(spgeo$stand, spgeo$plot)
spgeo
cen$staplo<-spgeo$staplo
cen

names(cen)[names(cen) == "V1"] <- "lon"
names(cen)[names(cen) == "V2"] <- "lat"

# compute the mean lat and lon
ll_means<-sapply(cen[1:2], mean)


Bart <- get_map(location = ll_means,  maptype = "terrain", source = "google", zoom =7)
mel <- get_map(location = ll_means,  maptype = "satellite", source = "google", zoom =13)
C4 <- get_map(location = sapply(cen[1:4,1:2], mean),  maptype = "satellite", source = "google", zoom =18)


m1<-ggmap(Bart)+geom_point(data=cen[1,],shape=21,col="gold", size=15, stroke=4)+
  geom_text(data =cen[1,] , aes(label = paste("Bartlett Experimental Forest")), angle = 0, hjust = .5, vjust=-3,size=7, color = "white")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())+ggtitle("a")
m1

m2<-ggmap(mel)+  geom_point(data =cen, aes(x=lon, y=lat),size=3, col="gold")+guides(size=F, col=F)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())+ggtitle("b")
m2  

C4C<-fortify(spgeo[1,])
C4C$Treatment<-"N"
C4N<-fortify(spgeo[2,])
C4N$Treatment<-"N+P"
C4P<-fortify(spgeo[3,])
C4P$Treatment<-"P"
C4NP<-fortify(spgeo[4,])
C4NP$Treatment<-"Control"

C4_plots<-rbind(C4C, C4N, C4P, C4NP)


C4_plots$Treatment<-factor(C4_plots$Treatment, levels=c("Control","N","P","N+P"))
m3<-ggmap(C4)+  
  geom_polygon(data = C4_plots, aes(long, lat, fill=Treatment,group = group,alpha=0.2), col="gold", size=2)+
  scale_fill_manual(values=c("black","red","blue","purple"))+theme(legend.position = "bottom")+guides(alpha=F)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.text=element_text(size=15))+ggtitle("c")
m3
library(ggpubr)

ggarrange(m1, m2, m3, nrow=1, common.legend = T, legend="right")


######  PART2:  9 stands color coded plots
# read in shapefile
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
control<-C7[C7$plot=="3", ]
control3<-extent(control)+10
co.bart<-crop(pic.C7, control3)



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
par(mfrow=c(1,1))
#3333 Rgb image
plotRGB(co.bart)
dpi=300
#tiff("R_output/tree_fig",width=12*dpi, height=10*dpi, res=dpi)
dev.off()
# add treetops to chm
#tiff("R_output/lidar_fig",width=12*dpi, height=10*dpi, res=dpi)
plot(chm7, axes=F, box=F, legend=T)
#plot(C7crownsPoly, border = "white", lwd = 0.5, add = TRUE)
plot(m7tops, add=T, pch=21,bg="blue",col="white", cex=1.5)
tiff("R_output/lidar_fig",width=12*dpi, height=10*dpi, res=dpi)
dev.off()

plotRGB(mini_noshade, r = 56, g = 28, b = 14, stretch = 'lin')
#plot(C7crownsPoly, border = "white", lwd = 0.5, add = TRUE)
plot(m7tops, add=T, pch=21,bg="blue",  col="white",cex=1.5)
tiff("R_output/shade_fig",width=12*dpi, height=10*dpi, res=dpi)
