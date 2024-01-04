
#Compare to actual top numbers
library(sf)

## Tree top analysis


# Alex's tree tops
stands<-st_read(file.path("data_folder","Bartlett_intensive_sites_30x30.shp"))

trees <- st_read(file.path("data_folder","bart_ttops.shp"))

lid_trees <- as.data.frame(table(trees$Stand, trees$Treatment))
lid_trees$Stand <- lid_trees$Var1
lid_trees$Treatment <- lid_trees$Var2
# Format lidar ttops
lid_trees$staplo<-paste(lid_trees$Stand, lid_trees$Treatment)

lid_trees$lid_density <- lid_trees$Freq  # / 900 * 10000



## # read in actual trees per 30 m to compare
tally<-read.csv("R_input\\Tally_of_10_plus_cm_stems.csv")

## Calculate the density

# by 30x30 m area.   900 m2
tally$density_hectare <-tally$actual  # / 900 * 10000


tally$lid_density <-lid_trees$lid_density[match(tally$staplo,lid_trees$staplo)]


names(tally)
# add in stand ages
tally$Age[tally$Stand=="C1"]<-"~30 years old"
tally$Age[tally$Stand=="C2"]<-"~30 years old"
tally$Age[tally$Stand=="C3"]<-"~30 years old"
tally$Age[tally$Stand=="C4"]<-"~60 years old"
tally$Age[tally$Stand=="C5"]<-"~60 years old"
tally$Age[tally$Stand=="C6"]<-"~60 years old" 
tally$Age[tally$Stand=="C7"]<-"~100 years old"
tally$Age[tally$Stand=="C8"]<-"~100 years old"
tally$Age[tally$Stand=="C9"]<-"~100 years old"

tally$Age<-factor(tally$Age, levels=c("~30 years old","~60 years old","~100 years old"))

library(ggrepel)
f.1<-ggplot(tally ,aes(x=density_hectare, y=lid_density, col=Age, label=Stand))+geom_point() +xlab("Number of 10+ cm trees")+geom_text_repel() + 
  ylab("Lidar tree tops")+
  theme_classic()+geom_abline()+theme(text=element_text(size=20))

f.1

names(tally)

library(tidyr)
tu <- gather(tally, "Type","density", c("density_hectare","lid_density"))

ggplot(tu, aes(x=staplo, y=density, fill=Type)) + geom_bar(stat="identity", position="dodge") + 
  theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.3)) +
  labs(x="Stand and treatment", y="Number of trees per 30x30 m area") +
  ggtitle("Comparison of actual tree tops and lidar tree tops") + 
  scale_fill_manual(values=c("darkgreen","darkblue")) + theme(legend.position="bottom")

spec<-as.data.frame(table(ldada$Treatment, ldada$Stand)/345  )



spec$staplo<-paste(spec$Var2, spec$Var1)

tally$spec<-spec$Freq[match(tally$staplo, spec$staplo)]

f.2<-ggplot(tally ,aes(x=lidar, y=spec, col=Age, label=Stand))+geom_point() +xlab("lidar tree tops")+geom_text_repel() + 
  ylab("Shade mask tree tops")+ylim(0,75)+xlim(0,75)+theme_classic()+geom_abline()+theme(text=element_text(size=20))
f.2
head(tally)

library(ggpubr)
ggarrange(f.1, f.2, common.legend = 2, nrow=1, legend="bottom")



####################################################################################

##############################################################################
par(mfrow=c(3,3))


####

lidar_path <- file.path(wd, "data_folder","DP3.30015.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","DiscreteLidar","CanopyHeightModelGtif")

chm.C2a<-raster("data_folder/DP3.30015.001/neon-aop-products/2019/FullSite/D01/2019_BART_5/L3/DiscreteLidar/CanopyHeightModelGtif/NEON_D01_BART_DP3_318000_4881000_CHM.tif")

chm.C1a<-raster(file.path(lidar_path,"NEON_D01_BART_DP3_313000_4879000_CHM.tif"))


pic_path <- file.path(wd, "data_folder","DP3.30010.001","neon-aop-products","2019","FullSite","D01","2019_BART_5","L3","Camera","Mosaic")

pic.C3<-stack(file.path(pic.path("2017_BART_3_316000_4878000_image.tif")))
pic.C5<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_314000_4878000_image.tif")
pic.C9<-stack("R_input\\DP3.30010.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Camera\\Mosaic\\2017_BART_3_317000_4879000_image.tif")

getwd()
pic.C2<- stack("data_folder\DP3.30010.001\neon-aop-products\2019\FullSite\D01\2019_BART_5\L3\Camera\Mosaic")



#####
tch3<-crop(chm.C3, m3ntops)
pc3<-crop(pic.C3, m3ntops)
##
tch5<-crop(chm.C5, m5ntops)
pc5<-crop(pic.C5, m5ntops)
##
tch9<-crop(chm.C9, m9ntops)
pc9<-crop(pic.C9, m9ntops)


########################################################

# plot chm and rgb next to each other


# Create polygon crown map
C3n_crownsPoly <- mcws(treetops = m3ntops, CHM = tch3, format = "polygons", minHeight = 1.5, verbose = FALSE)
C5n_crownsPoly <- mcws(treetops = m5ntops, CHM = tch5, format = "polygons", minHeight = 1.5, verbose = FALSE)
C9n_crownsPoly <- mcws(treetops = m9ntops, CHM = tch9, format = "polygons", minHeight = 1.5, verbose = FALSE)


# Plot CHM
plot(tch3, xaxt='n', yaxt = 'n', main="Young stand C3")
plot(C3n_crownsPoly, border = "blue", lwd = 0.5, add = TRUE)
plot(C3_ttops, add=T, pch=17, cex=1)
plot(tch5, xaxt='n', yaxt = 'n', main="Mid-aged stand C5")
plot(C5n_crownsPoly, border = "blue", lwd = 0.5, add = TRUE)
plot(C5_ttops, add=T, pch=17, cex=1)
plot(tch9, xaxt='n', yaxt = 'n', main="Old stand C9")
plot(C9n_crownsPoly, border = "blue", lwd = 0.5, add = TRUE)
plot(C9_ttops, add=T, pch=17, cex=1)

#33


# PLot RGB
plotRGB(pc3, axes=F )
plot(C3n_crownsPoly, border = "red", lwd = 2, add = TRUE)
plot(C3_ttops, add=T, pch=17, cex=1.4, col="yellow")
plotRGB(pc5, axes=F )
plot(C5n_crownsPoly, border = "red", lwd = 2, add = TRUE)
plot(C5_ttops, add=T, pch=17, cex=1.4, col="yellow")
plotRGB(pc9, axes=F )
plot(C9n_crownsPoly, border = "red", lwd = 2, add = TRUE)
plot(C9_ttops, add=T, pch=17, cex=1.4, col="yellow")






# Compute average crown diameter
C9n_crownsPoly[["crownDiameter"]] <- sqrt(C9n_crownsPoly[["crownArea"]]/ pi) * 2
mean(C9n_crownsPoly$crownDiameter)



##################
