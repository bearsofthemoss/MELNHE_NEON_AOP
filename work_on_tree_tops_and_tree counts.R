
#3 get basal area for each plot

library(vegan)
tree<-read.csv("data_folder/10+cm.csv")
table(tree$Stand)
tree$staplo<-paste(tree$Stand, tree$Plot) 
dim(tree)
tree<-tree[tree$Plot!="5",] # no calcium

# Create community matrix 'com', and environmental 'envdata' as dataframes

# Each row will be a plot, each column will be the value of abundance of that tree species.
co<-aggregate(tree$BA.m2, list(tree$staplo,Stand=tree$Stand, Age=tree$Age, Species=tree$Species), FUN="sum", simplify=T)
names(co)
com<-spread(co, "Species","x")
com[is.na(com)]=0
com<-as.data.frame(com)
com

#33## make  bap
bap<-aggregate(tree$BA.m2, list(staplo=tree$staplo,Stand=tree$Stand, Age=tree$Age), FUN="sum", simplify=T)
bap


#3 here is where I'd like to 

dada$Age[dada$Stand=="C1"]<-"~30 years old"
dada$Age[dada$Stand=="C2"]<-"~30 years old"
dada$Age[dada$Stand=="C3"]<-"~30 years old"
dada$Age[dada$Stand=="C4"]<-"~60 years old"
dada$Age[dada$Stand=="C5"]<-"~60 years old"
dada$Age[dada$Stand=="C6"]<-"~60 years old" 
dada$Age[dada$Stand=="C7"]<-"~100 years old"
dada$Age[dada$Stand=="C8"]<-"~100 years old"
dada$Age[dada$Stand=="C9"]<-"~100 years old"


head(dada)
library(ggplot2)
ggplot(dada, aes(x=Treatment, y=winRadius, col=Stand))+geom_jitter(width=0.2)+facet_wrap(~Age)+
  ggtitle("Average Crown Width for Bartlett trees")
