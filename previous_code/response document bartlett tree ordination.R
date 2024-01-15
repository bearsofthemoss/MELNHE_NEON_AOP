
# Background and data sources
  
library(vegan)
library(tidyverse)
library(ggplot2)
#library(ggforce)

#tree<-read.csv("data_folder/10+cm.csv")
tree<-read.csv("data_folder/ten_plus_DBH_2019.csv")
table(tree$Stand)
Bartlett <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9")
tree <- tree[tree$Stand %in% Bartlett, ]
tree$Plot.ID<-paste(tree$Stand, tree$Plot) 
dim(tree)
tree<-tree[tree$Plot!="5",] # no calcium



# 
tree[tree$Stand=="C1" | tree$Stand=="C2" | tree$Stand=="C3", "Age"]<- "Young"
tree[tree$Stand=="C4" | tree$Stand=="C5" | tree$Stand=="C6", "Age"]<- "Mid-aged"
tree[tree$Stand=="C7" | tree$Stand=="C8" | tree$Stand=="C9", "Age"]<- "Mature"

table(tree$Stand, tree$Age)

# Create community matrix 'com', and environmental 'envdata' as dataframes

# Each row will be a plot, each column will be the value of abundance of that tree species.
co<-aggregate(tree$DBH2019, list(Plot.ID =tree$Plot.ID,Stand=tree$Stand, Age=tree$Age, Species=tree$Species),
              FUN="sum", na.rm=T,
              simplify=T)
names(co)
com<-spread(co, "Species","x")

com[is.na(com)]=0
com<-as.data.frame(com)


# take out C5 outside
com <- com[!com$Plot.ID=="C5 outside",]


# Ordinate the tree comunity matrix.

# Using metaMDS() command in 'vegan'. 'sites' will store the plot.ID values
names(com)
use<-com[,c(-1,-2,-3)]
dim(use)
names(use)


# Ordinate
m1 <- metaMDS(use, distance = "bray", k = 2, trymax = 200,
              autotransform =FALSE,  
              wascores = TRUE, expand = TRUE,
              trace = 1, plot = TRUE,)

#sites<-rownames(use)


points<-as.data.frame(m1$points)
points$Age<-com$Age
points$Stand<-com$Stand

ggplot(points, aes(x=MDS1, y=MDS2, col=Age, group=Stand,label=Stand))+geom_point(size=4)+xlab("NMDS 1")+ylab("NMDS 2")+
  geom_mark_ellipse(aes( label = Stand, size=3), label.fontsize = 10) +scale_color_manual(values=c("green","yellow","forestgreen"))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text=element_text(size=20))+
  guides(size=F)



#################
# plot(m1, display = c("species"), choices = c(1,2), type ="t", shrink = FALSE, cex=1.4)
# 
# points(m1, display = c("sites","species"), choices = c(1,2), pch = c(0,1,2)[as.numeric(com$Age)], cex=1)
# 
# legend("topright", legend = paste(c("Mid","Old","Young")),
#        pch = c(0,1,2))
# 
# ordiellipse(m1, com$Stand, display = "sites", kind = "sd", label = T, cex=2)
# 
# title(main ="NMDS Ordination of MELNHE Plots in Bartlett")
