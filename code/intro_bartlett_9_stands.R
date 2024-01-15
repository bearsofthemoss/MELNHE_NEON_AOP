



## Create figs intro

library(ggrepel)


# read in .csv files from R_output

sh <- read.csv(file.path("R_output","stand_heights.csv"))
sh[sh$stand=="C1" |sh$stand=="C2" |sh$stand=="C3" , "Age" ] <- "Young"
sh[sh$stand=="C4" |sh$stand=="C5" |sh$stand=="C6" , "Age" ] <- "Mid-aged"
sh[sh$stand=="C7" |sh$stand=="C8" |sh$stand=="C9" , "Age" ] <- "Old"

dbh <- read.csv("R_output/ten_plus_DBH_2019.csv")


# Only use Stands in Bartlett
bart <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9")
dbh <- dbh[dbh$Stand %in% bart, ]


# handle leading or trailing spaces
dbh$Treatment <- trimws(dbh$Treatment)

dbh$Treatment <- gsub("Con", "Control", dbh$Treatment)




dbh$cm <- dbh$DBH2019 * 2.54


dbh$ba_cm2 <- 3.14159  * (((dbh$cm/2)^2))  # cm2 per in2

#dbh <- dbh[!dbh$dead2019=="dead" , ]


dbh$ba_m2 <- dbh$ba_cm2 / 10000



diam <- aggregate(list(ba_m2 = dbh$ba_m2, cm = dbh$cm),  by=list(Stand = dbh$Stand, Treatment = dbh$Treatment), FUN="sum", na.rm=T)


# number of hectares in the 30x30 plot
diam$ha  <- 0.09

diam$ba_m2_ha <- diam$ba_m2 / diam$ha


# Figure 1, 

sh[sh$stand=="C1", "Last_year_harv"] <- "1990"
sh[sh$stand=="C2", "Last_year_harv"] <- "1988"
sh[sh$stand=="C3", "Last_year_harv"] <- "1985"
sh[sh$stand=="C4", "Last_year_harv"] <- "1979"
sh[sh$stand=="C5", "Last_year_harv"] <- "1976"
sh[sh$stand=="C6", "Last_year_harv"] <- "1975"
sh[sh$stand=="C7", "Last_year_harv"] <- "1890"
sh[sh$stand=="C8", "Last_year_harv"] <- "1883"
sh[sh$stand=="C9", "Last_year_harv"] <- "1890"


table(sh$Last_year_harv, sh$stand)

sh <- sh[sh$layer>5,]

heights <- aggregate(list(avg_height_m= sh$layer), 
                     by=list(Stand = sh$stand, 
                             Last_harv = sh$Last_year_harv,
                                       Age = sh$Age,
                            Treatment = sh$Treatment),
          FUN="mean", na.rm=T)


st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

SE_heights <- aggregate(list(se = sh$layer), by= list(
  Stand = sh$stand, 
  Last_harv = sh$Last_year_harv,
  Age = sh$Age,
  Treatment = sh$Treatment),
  FUN= st.err, na.rm=T)


# create staplo object
SE_heights$staplo <- paste(SE_heights$Stand, SE_heights$Treatment)
heights$staplo <- paste(heights$Stand, heights$Treatment)
diam$staplo <- paste(diam$Stand, diam$Treatment)


heights$years_since <- 2019 - as.numeric(heights$Last_harv)


# bring in BA from dbh df
heights$ba_m2_ha <- diam$ba_m2_ha[match(heights$staplo, diam$staplo)]

heights[heights$Treatment=="NP","Treatment"] <- "N+P"

heights$Treatment <- factor(heights$Treatment, levels=c("Control","N","P","N+P"))
heights$Age <-factor(heights$Age, levels=c("Old","Mid-aged","Young"))

heights$se <- SE_heights$se[match(heights$staplo, SE_heights$staplo)]


g1 <- ggplot(heights, aes(x= ba_m2_ha , y= avg_height_m,
                    color=Age,fill=Treatment,
                    label=Stand))+
  scale_fill_manual(values=c("black","blue","red","purple"))+
  scale_color_manual(values=c("#E6AB02","#666666","#D95F02"))+  
 # scale_shape_manual(values=c(22, 24, 23))+
  geom_text_repel()+
  geom_point(  shape=21, size=3,stroke=3 )+
  theme_bw()+theme(panel.grid.major=element_blank(), panel.grid.minor = element_blank())+
  labs(x="Basal area (square meters per hectare) ", y="Average canopy height (meters)")

g1

###################################################


#  Community ordination
names(dbh)

dbh$staplo <- paste(dbh$Stand, dbh$Plot)


dbh$Age <- heights$Age[match(dbh$Stand, heights$Stand)]

co<-aggregate(list(cm = dbh$cm), list(staplo =dbh$staplo,Stand=dbh$Stand,
                                      Treatment = dbh$Treatment, Age=dbh$Age, Species=dbh$Species),
              FUN="sum", na.rm=T,
              simplify=T)
names(co)

com<-spread(co[ , c("staplo","Stand","Treatment","Species","cm","Age")], "Species","cm")
com <- com[!com$Treatment=="Ca",]
dim(com)

com[is.na(com)]=0
com<-as.data.frame(com)


# Ordinate the tree comunity matrix.

# Using metaMDS() command in 'vegan'. 'sites' will store the plot.ID values
names(com)
use<-com[,-c(1:4)]
dim(use)
names(use)

library(vegan)
# Ordinate
m1 <- metaMDS(use, distance = "bray", k = 2, trymax = 200,
              autotransform =FALSE,  
              wascores = TRUE, expand = TRUE,
              trace = 1, plot = TRUE,)

#sites<-rownames(use)


points<-as.data.frame(m1$points)
points$Age<-com$Age
points$Stand<-com$Stand
points$Treatment <- com$Treatment

points$Treatment<- factor(points$Treatment, levels=c("Control","N","P","NP"))

library(ggforce)
g2 <- ggplot(points, aes(x=MDS1, y=MDS2))+
#  geom_mark_ellipse(aes( group=Stand, label = Stand)) +
  geom_mark_ellipse(aes( group=Age, fill = Age, label=Age, linewidth=3)) +
  geom_text(aes(label=Stand, col=Treatment), size=4)+
  labs(x="NMDS 1", y="NMDS 2")+
  scale_fill_manual(values=c("#E6AB02","#666666","#D95F02"))+ 
  scale_color_manual(values=c("black","blue","red","purple"))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(linewidth=F)+xlim(-1.5, 1.5)+ylim(-1.2, 1.2)

g2

###########



# Now compare the tree records in lidar vs the basal area.  Or count of trees?



#actual<- read.csv(file.path("data_folder","actual_tops.csv"))



#lid_trees <- as.data.frame(table(actual$Var1, actual$Var2))

trees <- st_read(file.path("data_folder","bart_ttops.shp"))
lid_trees<- as.data.frame(table(trees$Stand, trees$Treatment))


lid_trees$Stand <- lid_trees$Var1
lid_trees$Treatment <- lid_trees$Var2

# Format lidar ttops
lid_trees$staplo<-paste(lid_trees$Stand, lid_trees$Treatment)

# add age
lid_trees$Age <- com$Age[match(lid_trees$Stand, com$Stand)]


lid_trees$lid_density <- lid_trees$Freq / 0.09 


tally <-as.data.frame( table(dbh$Stand, dbh$Treatment))
tally <- tally[!tally$Var2=="Ca",]

## Calculate the density
tally$density_hectare <-tally$Freq / 0.09  # / 900 * 10000
tally$staplo <- paste(tally$Var1, tally$Var2)

tally$lid_density <-lid_trees$lid_density[match(tally$staplo,lid_trees$staplo)]
lid_trees$density_10cm <- tally$density_hectare[match(lid_trees$staplo, tally$staplo)]


# library(tidyr)
# tu <- gather(lid_trees, "Type","density", c("density_10cm","lid_density"))
# g3 <- ggplot(tu, aes(x=Stand, y=density, fill=Type)) + geom_bar(stat="identity", position="dodge") + 
#   theme_classic() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.3)) +
#   labs(x="Stand and treatment plot", y="Number of trees per hectare") +
#   ggtitle("Comparison of actual tree tops and lidar tree tops") + 
#   scale_fill_manual(values=c("darkgreen","darkblue")) + theme(legend.position="bottom")
# 
# 

g3<-ggplot(lid_trees ,aes(x=density_10cm, y=lid_density, col=Age, label=Stand))+
  geom_text(size=5) +
  xlab("Number of 10+ cm trees per hectare")+
  ylab("Number of tree top detections per hectare")+
  scale_color_manual(values=c("#E6AB02","#666666","#D95F02"))+ 
  theme_classic()+geom_abline()+
  xlim(0,1800)+ylim(0,1800)

g3


library(ggpubr)

ggarrange(g1, g2, g3, nrow=3)

