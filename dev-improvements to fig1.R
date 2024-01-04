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
library(ggmap)
register_google("AIzaSyB7234pEGzoCTh6KH2zJash0oplcQ-GzeE" )

google_account()
has_google_key()
google_key()  

#devtools::install_github('oswaldosantos/ggsn')
library(ggsn)

#### load plots
plots<-readOGR("R_Output","Bartlett_intensive_sites_30x30")

# transform to UTM coordinates
crss <- make_EPSG()
UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))
stands <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))

# make lat long re projection
spgeo <- spTransform(stands, CRS("+proj=longlat +datum=WGS84"))
as<-fortify(spgeo)
cen <- as.data.frame(getSpPPolygonsLabptSlots(spgeo))
as
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


str(Bart)
library(ggsn)
m1<-ggmap(Bart)+geom_point(data=cen[1,],shape=21,col="gold", size=55, stroke=4)+
#  geom_text(data =cen[1,] , aes(label = paste("Bartlett Experimental Forest")), angle = 0, hjust = .5, vjust=-3,size=7, color = "white")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())+ggtitle("a")+ theme(plot.title = element_text(size = 40, face = "bold"))+
  scalebar( x.min=-74.8, x.max=-67.8, y.min=41.5, y.max=56.5 ,location = "bottomright", dist = 100, transform=TRUE, dist_unit="km")
           

  m1

cen

##############

#  SPGEO for all 9 stands
Cstand<-fortify(spgeo)
table(Cstand$id)


Cstand$Treatment<-rep(c("Control","N","P","N+P"), each=13)
Cstand
Cstand$Treatment<-factor(Cstand$Treatment, levels=c("Control","N","P","N+P"))
m2<-ggmap(mel)+  
  geom_polygon(data = Cstand, aes(long, lat, fill=Treatment,group = group),size=10)+
  geom_polygon(data = Cstand, aes(long, lat, fill=Treatment,group = group),col="white",size=0.5)+
  scale_fill_manual(values=c("black","blue","red","purple"))+theme(legend.position = "bottom")+guides(alpha=F)+
  xlim(-71.323, -71.265)+ylim(44.037, 44.06)+theme(legend.position = "bottom")+theme(plot.title = element_text(size = 40, face = "bold"))+  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.text=element_text(size=15))+ggtitle("b")+
  scalebar( x.min=-71.323, x.max=-71.265, y.min=44.037, y.max=44.06 ,location = "bottomright", dist = 1, transform=TRUE, dist_unit="km",
            st.dist = TRUE, st.bottom = FALSE, st.size = 20,st.color = "black", box.fill = c("black", "white"), box.color="white")

m2

##################
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
  scale_fill_manual(values=c("black","blue","red","purple"))+theme(legend.position = "bottom")+guides(alpha=F)+
  xlim(-71.32225, -71.3204)+ylim(44.0417, 44.0431)+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.text=element_text(size=15))+ggtitle("c")+theme(plot.title = element_text(size = 40, face = "bold"))+
  scalebar( x.min=-71.3, x.max=-67.8, y.min=41.5, y.max=56.5 ,location = "bottomright", dist = 100, transform=TRUE, dist_unit="km")

str(C4)
m3

library(ggpubr)



ggarrange(m1, m2, nrow=1, common.legend = T, legend="bottom")

ggarrange(m1, m2, m3, nrow=1, common.legend = T, legend="bottom")





######  Figure 2:  1 color coded plots
# read in shapefile
C7<-stands[stands$stand =="C7",]
# centroids are the 'plot centers'. code for Lidar tiles works with point data
centroids <- as.data.frame(getSpPPolygonsLabptSlots(C7))
east<-centroids$V1
north<-centroids$V2

# this downloads 15 cm Rgb data for one plot.
#byTileAOP("DP3.30010.001", site="BART", year="2017", check.size = F,buffer = 200, 
#          easting=east, northing=north, 
#          savepath="data_folder")

pic.C7<-stack("data_folder\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_315000_4880000_image.tif")
C7<-stands[stands$stand=="C7",]
control<-C7[C7$plot=="3", ]
control3<-extent(control)+10
co.bart<-crop(pic.C7, control3)


# this downlaods the lidar chm data
#byTileAOP("DP3.30015.001", site="BART", year="2017", check.size = F,buffer = 200, 
#          easting=east, northing=north, 
#          savepath="data_folder")


north
east
chm_C7<-raster("data_folder\\DP3.30015.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\DiscreteLidar\\CanopyHeightModelGtif\\NEON_D01_BART_DP3_315000_4880000_CHM.tif")
chm7<-crop(chm_C7,control3)
# do treE top detection
lin.C7 <- function(x){x * 0.02}
m7tops <- vwf(CHM = chm7, winFun = lin.C7, minHeight = 2)
C7crownsPoly <- mcws(treetops = m7tops, CHM = chm7, format = "polygons", minHeight = 1.5, verbose = FALSE)


#3 3 band tiff with shade mask
# read in mini_noshade with C7 no shade?


############################
dev.off()
par(mfrow=c(1,1))
#3333 Rgb image
plotRGB(co.bart)
plot(control, add=T, lwd=20)

# add treetops to chm
plot(chm7, axes=F, box=F, legend=T,legend.args=list(text='Tree height (m)',side=4, font=2, line=3, cex=2))

plot(C7crownsPoly, border = "white", lwd = 2, add = TRUE)
plot(m7tops, add=T, pch=24,cex=4, col="black", bg="yellow")
plot(control, add=T, lwd=20)


cube_no_shade <- stack("C7_no_shade.grd")
mini<-extent(control3)
mini_noshade <- crop(cube_no_shade,mini)

plotRGB(mini_noshade, r = 56, g = 30, b = 20, stretch = 'lin')
plot(C7crownsPoly, border = "white", lwd = 2, add = TRUE)
plot(m7tops, add=T, pch=24,cex=4, col="black", bg="yellow")
plot(control, add=T, lwd=20)

#################################################################################################################################
## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada<-read.csv("data_folder/actual_tops.csv")
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


