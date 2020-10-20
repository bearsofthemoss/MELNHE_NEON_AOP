

library(sp)
library(rgdal)
library(stringr) # this is for data management
library(tidyr)
library(ggplot2)
library(raster)
library(neonUtilities)
library(rgdal)
library(rgeos)

# get fpar

# Alex's plots
plots <- readOGR("R_input","Bartlett_intensive_sites_30x30")

# Alex's tree tops
trees <- readOGR("R_input","bart_ttops_10_4_2020")


# transform to UTM coordinates
crss <- make_EPSG()

UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))

# plots_UTM <- sp::spTransform(plots, CRS(SRS_string=paste0("EPSG:",UTM$code)))
plots_UTM <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))

# centroids are the 'plot centers'. This script works with point data.
centroids <- as.data.frame(getSpPPolygonsLabptSlots(plots_UTM))

# these are the easting and northings for the stand locations
east <- centroids[, 1]
north <-centroids[, 2]

#
C1<-stands[stands$stand=="C1",]
C2<-stands[stands$stand=="C2",]
C3<-stands[stands$stand=="C3",]
C4<-stands[stands$stand=="C4",]
C5<-stands[stands$stand=="C5",]
C6<-stands[stands$stand=="C6",]
C7<-stands[stands$stand=="C7",]
C8<-stands[stands$stand=="C8",]
C9<-stands[stands$stand=="C9",]

#byTileAOP(dpID="DP3.30006.001",site="BART",
#            year="2017", easting= east,
#            northing = north,
#            buffer=70, savepath = "./R_input/Bart_tiles",check.size = T)
# the code did not have the bart_tiles for savepath-  I renamed the folder, and updated the savepath for consistency Ay 9_22_2020

fpC78<-raster("R_input\\fPAR_data_portal\\NEON_D01_BART_DP1_20170814_141517_fPAR.tif")
fpC3<-raster("R_input\\fPAR_data_portal\\NEON_D01_BART_DP1_20170814_143341_fPAR.tif")
fpC9<-raster("R_input\\fPAR_data_portal\\NEON_D01_BART_DP1_20170814_145952_fPAR.tif")
fpC24<-raster("R_input\\fPAR_data_portal\\NEON_D01_BART_DP1_20170814_151221_fPAR.tif")
fpC15<-raster("R_input\\fPAR_data_portal\\NEON_D01_BART_DP1_20170814_154540_fPAR.tif")
fpC6_m<-raster("R_input\\fPAR_data_portal\\NEON_D01_BART_DP1_20170814_150606_fPAR.tif")
#33#
fpar.C1<-crop(fpC15, C1)
fpar.C2<-crop(fpC24, C2)
fpar.C3<-crop(fpC73, C3)
fpar.C4<-crop(fpC24, C4)
fpar.C5<-crop(fpC15, C5)
fpar.C6<-crop(fpC6_m, C6)
fpar.C7<-crop(fpC78, C7)
fpar.C8<-crop(fpC78, C8)
fpar.C9<-crop(fpC9, C9)


plot(fpar.C7)
plot(fpar.C8)
plot(fpar.C9)

#3 
fpar<-merge(fpC78, fpC3, fpC9, fpC15, fpC24, fpC6_m )

plot(fpar)
plot(plots_UTM, add=T)




use<- list(raster::extract(fpar,trees,df=T, sp=T))
fp <-as.data.frame( do.call(rbind, use))
names(fp)[7]<-"value"
head(fp)

ggplot(fp, aes(x=height, y=value, col=Treatment))+geom_point()+geom_smooth(se=F, method="lm")
ggplot(fp, aes(x=Stand, y=value, col=Treatment))+geom_boxplot()+scale_color_manual(values=c("black","blue","red","purple"))+
  ggtitle("fPAR from flight line")+theme_classic()


#N*P Anova
fp$Treatment<-factor(fp$Treatment, levels=c("Control","N","P","NP"))
fp$Ntrmt <- factor(  ifelse(fp$Treatment == "N" | fp$Treatment == "NP", "N", "NoN"))
fp$Ptrmt <- factor(  ifelse(fp$Treatment %in% c("P", "NP"), "P", "NoP"))
fp$staplo<-paste(fp$Stand, fp$Treatment)

anova(lmer(value ~Ntrmt*Ptrmt+(1|Stand/staplo), data=fp))
