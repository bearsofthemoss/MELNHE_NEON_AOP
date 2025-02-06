


# # Set the CRS to UTM Zone 19N (EPSG:32619)
# crs <- CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
# projection(hsiStack) <- crs


library(here)
library(ggplot2)
wd <- here::here()

# read in shapefile of plot locations
#stands<-st_read(file.path("data_folder","Bartlett_intensive_sites_30x30.shp"))

# define path
C3_path <- file.path("R_output","pixel_processing")


# read in 4 .csv files
all_plot <- read.csv(file.path(C3_path, "all_C3_spec.csv"))
all_plot$type <- "All pixels in the plot"
names(all_plot)

no_shade <- read.csv(file.path(C3_path, "no_shade_C3_spec.csv"))
no_shade$type <- "Shaded pixels removed"

tops <- read.csv(file.path(C3_path, "top_C3_spec.csv"))
tops$type <- "Only non-shaded tree top pixels"

dim(tops)
names(tops)

shtops <- read.csv(file.path(C3_path, "shady_top_C3_spec.csv"))
shtops$type <- "Tree tops including shaded tree tops"



library(dplyr)
library(tidyr)

all_plot <- all_plot %>%
  pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")


no_shade <- no_shade %>%
  pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")

tops <- tops %>%
  pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")

shtops <- shtops %>%
pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")


names(all_plot)
names(no_shade)
names(tops)
names(shtops)




# Assuming your column is named 'your_column'
sc <- rbind( all_plot[ ,c("type","wvl","refl")], 
             no_shade[ ,c("type","wvl","refl")], 
             tops[ ,c("type","wvl","refl")], 
             shtops[ ,c("type","wvl","refl")]) 


# Assuming your column is named 'your_column'
sc$wvl <- round(as.numeric(gsub("Band_", "", sc$wvl)))


# plo$wvl <- round(as.numeric(gsub("Band_", "", plo$wvl)))
# plo$Stand <- "C3"


head(sc)

"Shaded pixels removed"

tops <- read.csv(file.path(C3_path, "top_C3_spec.csv"))
tops$type <- "Only non-shaded tree top pixels"

dim(tops)
names(tops)

shtops <- read.csv(file.path(C3_path, "shady_top_C3_spec.csv"))
shtops$type <- "Tree tops including shaded tree tops"



num_entire_plot <- dim(sc[sc$type=="All pixels in the plot",])[1] / 426
num_after_shading <- round(dim(sc[sc$type=="Shaded pixels removed",])[1] / 426, digits=0)
num_tops <- dim(sc[sc$type=="Only non-shaded tree top pixels",])[1] / 345
num_shaded_tops <- dim(sc[sc$type=="Tree tops including shaded tree tops",])[1] / 345




g1 <- ggplot(sc[sc$type==c("All pixels in the plot"),], aes(x=wvl, y=refl, group=wvl )) + 
  geom_boxplot()+ xlim(380,2520)+
  ggtitle( paste0( num_entire_plot , " pixels, entire plot"))+
  xlab("")+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g2 <- ggplot(sc[sc$type==c("Shaded pixels removed"),], aes(x=wvl, y=refl, group=wvl )) + 
  geom_boxplot()+ xlim(380,2520)+
  ggtitle( paste0( num_after_shading , " pixels, shaded pixels removed"))+
  xlab("")+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g3 <- ggplot(sc[sc$type==c("Tree tops including shaded tree tops"),], aes(x=wvl, y=refl, group=wvl )) + 
  geom_boxplot()+ xlim(380,2520)+
  ggtitle( paste0( num_shaded_tops , " pixels shown, tree tops with shade"))+
  xlab("")+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g4 <- ggplot(sc[sc$type==c("Only non-shaded tree top pixels"),], aes(x=wvl, y=refl, group=wvl )) + 
  geom_boxplot()+ xlim(380,2520)+
  ggtitle( paste0( num_tops , " pixels shown, non-shaded tree top pixels, brightness normalized"))+
  xlab("")+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


library(ggpubr)
ggarrange( g1, g2, g3, g4, nrow=4)




