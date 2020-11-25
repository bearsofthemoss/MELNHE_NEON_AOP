
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


par(mfrow=c(1,1))
# Visualize the NMDS ordination
plot(m1, display = c("sites", "species"), choices = c(1,2), type = "n", shrink = TRUE)

com$Age<-factor(com$Age, levels=c("Young","Mid-aged","Old"))
points(m1, display = "sites" , pch = c(0,1,2)[as.factor(com$Age)], cex=2)

legend("topright", legend = paste(c("Young","Mid-aged","Old")),pch = c(0,1,2), cex=2)

ordiellipse(m1 ,com$Stand, display = "sites", kind = "sd", label = T, cex=2)

title(main ="NMDS Ordination of MELNHE Plots in Bartlett")



plot(m1, display = c("species"), choices = c(1,2), type ="t", shrink = FALSE, cex=1.4)

points(m1, display = c("sites","species"), choices = c(1,2), pch = c(0,1,2)[as.numeric(com$Age)], cex=1)

legend("topright", legend = paste(c("Mid","Old","Young")),
       pch = c(0,1,2))

ordiellipse(m1, com$Stand, display = "sites", kind = "sd", label = T, cex=2)

title(main ="NMDS Ordination of MELNHE Plots in Bartlett")




