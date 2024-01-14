
# Set the CRS to UTM Zone 19N (EPSG:32619)
crs <- CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
projection(hsiStack) <- crs





ttops <- read.csv("C:\\Users\\bears\\Documents\\GitHub\\MELNHE_NEON_AOP\\data_folder\\actual_tops.csv")
ttops$type <- "used spectra"
all_spec <- read.csv("C:\\Users\\bears\\Documents\\GitHub\\MELNHE_NEON_AOP\\data_folder\\all_spec_df.csv")
all_spec$type <- "All spectra"
ndvi_mask_spec <- read.csv("C:\\Users\\bears\\Documents\\GitHub\\MELNHE_NEON_AOP\\data_folder\\ndvi_mask_spec_df.csv")
ndvi_mask_spec$type <- "NDVI mask"
norm_spec <- read.csv("C:\\Users\\bears\\Documents\\GitHub\\MELNHE_NEON_AOP\\data_folder\\norm_spec_df.csv")
norm_spec$type <- "Brightness normalize function"
shade_mask_spec <- read.csv("C:\\Users\\bears\\Documents\\GitHub\\MELNHE_NEON_AOP\\data_folder\\shade_mask_spec_df.csv")
shade_mask_spec$type <- "With Shade mask > 0.1"

plot_spec <- read.csv("C:\\Users\\bears\\Documents\\GitHub\\MELNHE_NEON_AOP\\data_folder\\all_C3_spec.csv")
plot_spec$type <- "Plot-level"

names(plot_spec)
names(shade_mask_spec)

library(dplyr)
library(tidyr)

ttops <- ttops %>%
  pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")


all <- all_spec %>%
  pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")

ndvi <- ndvi_mask_spec %>%
  pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")

norm <- norm_spec %>%
pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")

shade <- shade_mask_spec %>%
  pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")

plo  <- plot_spec %>%
  pivot_longer(cols = starts_with("Band_"), 
               names_to = "wvl", 
               values_to = "refl")




# Assuming your column is named 'your_column'
sc <- rbind(all, ndvi, norm, shade, ttops) 


# Assuming your column is named 'your_column'
sc$wvl <- round(as.numeric(gsub("Band_", "", sc$wvl)))


plo$wvl <- round(as.numeric(gsub("Band_", "", plo$wvl)))
plo$Stand <- "C3"

dim(sc[sc$type=="All spectra",])
dim(sc[sc$type=="NDVI mask",])
dim(sc[sc$type=="Brightness normalize function",])
dim(sc[sc$type=="With Shade mask < 0.1",])
dim(sc[sc$type=="used spectra",])

library(ggplot2)
dim(sc)

table(sc$Stand)
table(sc$treeID)
sc <- sc[sc$Stand=="C3",]

gp <- ggplot(plo, aes(x=wvl, y=refl, group=wvl #color=Treatment, group = Treatment
                      )) + 
  #geom_point(shape=".") +
  geom_boxplot()+
  ggtitle( paste0( dim(plo)[1] , "rows"))
#  scale_color_manual(values = c("Control" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))
gp



g1 <- ggplot(sc[sc$type==c("All spectra"),], aes(x=wvl, y=refl, color=Treatment, group = Treatment)) + 
  geom_point(shape=".") +ggtitle( paste0( length(unique(sc[sc$type=="All spectra", "treeID"]))," pixels displayed"))+
  scale_color_manual(values = c("Control" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))
g1

  g2 <- ggplot(sc[sc$type==c("NDVI mask"),], aes(x=wvl, y=refl, color=Treatment, group = treeID)) + 
  geom_point(shape=".") +ggtitle( paste0( length(unique(sc$treeID))," pixels displayed"))+
  scale_color_manual(values = c("Control" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))

g2

g3 <- ggplot(sc[sc$type==c("Brightness normalize function"),], aes(x=wvl, y=refl, color=Treatment, group = treeID)) + 
  geom_point(shape=".") +
  facet_wrap(~type, ncol=5, scales="free")+
  scale_color_manual(values = c("Control" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))


g4 <- ggplot(sc[sc$type==c("With Shade mask > 0.1"),], aes(x=wvl, y=refl, color=Treatment, group = treeID)) + 
   geom_point(shape=".") +
  facet_wrap(~type, ncol=5, scales="free")+
  scale_color_manual(values = c("Control" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))

library(ggpubr)
ggarrange(g1, g2, g3, g4, nrow=4)
4/




