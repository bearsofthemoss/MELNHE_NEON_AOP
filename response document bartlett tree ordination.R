
# Background and data sources
  
library(vegan)
library(tidyr)
tree<-read.csv("R_input/10+cm.csv")
table(tree$Stand)
tree$Plot.ID<-paste(tree$Stand, tree$Plot) 
dim(tree)
tree<-tree[tree$Plot!="5",] # no calcium

# Create community matrix 'com', and environmental 'envdata' as dataframes

# Each row will be a plot, each column will be the value of abundance of that tree species.
co<-aggregate(tree$BA.m2, list(tree$Plot.ID,Stand=tree$Stand, Age=tree$Age, Species=tree$Species), FUN="sum", simplify=T)
names(co)
com<-spread(co, "Species","x")
com
com[is.na(com)]=0
com<-as.data.frame(com)
com


# Ordinate the tree comunity matrix.

# Using metaMDS() command in 'vegan'. 'sites' will store the plot.ID values

use<-com[,c(-1,-2,-3)]
dim(use)
names(use)
m1 <- metaMDS(use, distance = "bray", k = 2, trymax = 200,
              autotransform =FALSE,  
              wascores = TRUE, expand = TRUE,
              trace = 1, plot = TRUE,)

sites<-rownames(use)

dev.off()
par(mfrow=c(1,1))
# Visualize the NMDS ordination

com$Age<-factor(com$Age, levels=c("Young","Mid-aged","Old"))

col_vector <- factor(com$Age)
col_palette <- palette()[col_vector]
col_palette


m1$points

points<-as.data.frame(m1$points)
points$Age<-com$Age
points$Stand<-com$Stand
library(tidyverse)
library(ggplot2)
library(ggforce)

ggplot(points, aes(x=MDS1, y=MDS2, col=Age, group=Stand,label=Stand))+geom_point(size=4)+xlab("NMDS 1")+ylab("NMDS 2")+
  geom_mark_ellipse(aes( label = Stand, size=3), label.fontsize = 20) +scale_color_manual(values=c("green","yellow","forest green"))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(text=element_text(size=20))+
  guides(size=F)



## NMDS ordination in base R
plot(m1, display = c("sites", "species"), choices = c(1,2), type = "n", shrink = TRUE)
points(m1, display = "sites", pch = c(0,1,2)[as.factor(com$Age)], cex=2, col=com$Age)
legend("topright", legend = paste(c("Young","Mid-aged","Old")),pch = c(0,1,2), cex=2)
text(m1, labels=points$Stand, cex= 1,pos=4) ### label points
ordiellipse(m1 ,com$Stand, display = "sites", kind = "sd", label = T, cex=2, col=col_palette  )

title(main ="NMDS Ordination of MELNHE Plots in Bartlett Experimental Forest")



plot(m1, display = c("species"), choices = c(1,2), type ="t", shrink = FALSE, cex=1.4)

points(m1, display = c("sites","species"), choices = c(1,2), pch = c(0,1,2)[as.numeric(com$Age)], cex=1)

legend("topright", legend = paste(c("Mid","Old","Young")),
       pch = c(0,1,2))

ordiellipse(m1, com$Stand, display = "sites", kind = "sd", label = T, cex=2)

title(main ="NMDS Ordination of MELNHE Plots in Bartlett")




