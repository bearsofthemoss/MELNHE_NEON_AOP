library(ggplot2)
library(tidyr)
library(data.table)
## use foliar chem to look at species differences

dan<-read.csv("C:/Users/bears/Downloads/MELNHE_FoliarChemistry_2008to2016.csv", header=T)
head(dan)
ggplot(dan, aes(x=Year, y=N, col=Species))+geom_point()+facet_grid(Age ~ Stand)


Comparing CVs
#
CV <- function(x){
  (sd(x, na.rm=T)/mean(x, na.rm=T))*100
}

# take the mean across years, then do sd
annual<-aggregate(dan$N.P,by=list(Year=dan$Year),FUN="mean" , na.rm=T)
cv.year<-CV(annual$x)


species<-aggregate(dan$N.P,by=list(Species=dan$Species),FUN="mean" , na.rm=T)
cv.sp<-CV(species$x)


stand<-aggregate(dan$N.P,by=list(Stand=dan$Stand),FUN="mean" , na.rm=T)
cv.sta<-CV(stand$x)  

plot<-aggregate(dan$N.P,by=list(Plot=dan$Plot),FUN="mean" , na.rm=T)
cv.plot<-CV(plot$x)  

site<-aggregate(dan$N.P,by=list(Site=dan$Site),FUN="mean" , na.rm=T)

cv.site<- CV(site$x)
  


Treatment<-aggregate(dan[dan$pre_post=="post","N.P"] ,by=list(Treatment=dan[dan$pre_post=="post","Treatment"]),FUN="mean" , na.rm=T)
cv.trt<-CV(Treatment$x)

cv.trt
cbind(cv.year, cv.sp, cv.sta,cv.plot, cv.site, cv.trt)


## for loop it.   Do each analyte.

names(dan)


## make the function
cv.function <- function(y){
  annual<-aggregate(y,by=list(Year=dan$Year),FUN=CV)
  cv.year<-mean(annual$x, na.rm=T)
  
  species<-aggregate(y,by=list(Species=dan$Species),FUN=CV)
  cv.sp<-mean(species$x, na.rm=T)
  
  
  stand<-aggregate(y,by=list(Stand=dan$Stand),FUN=CV)
  cv.sta<-mean(stand$x, na.rm=T)  
  
  plot<-aggregate(y,by=list(Plot=dan$Plot),FUN=CV)
  cv.plot<-mean(plot$x, na.rm=T)  
  
  site<-aggregate(y,by=list(Site=dan$Site),FUN=CV)
  cv.site<- mean(site$x, na.rm=T)
  
  tree<-aggregate(y,by=list(Tree=dan$TagID),FUN=CV)
  cv.tree<- mean(tree$x, na.rm=T)

    mem<- as.data.frame(cbind(cv.year, cv.sp, cv.sta,cv.plot, cv.site,cv.tree ))
  return(mem)}

# this is for the coefficients
output.cv<-list()
for(i in c(16:31)){ 
  y = dan[,i]
  
  output.cv[[i-15]] <- cv.function(y)}
a.em<- as.data.frame(rbindlist(output.cv))
a.em

################   Mow for treatment
tan<-dan[dan$pre_post=="post",]

cv.t <- function(y){
  treat<-aggregate(y ,by=list(Treatment=tan$Treatment),FUN=CV)
  cv.treat<-mean(treat$x, na.rm=T)
  mem<- as.data.frame(cbind(cv.treat))
  return(mem)}

head(tan)
# this is for the coefficients
output.t<-list()
for(i in c(16:31)){ 
  y = tan[,i]
    output.t[[i-15]] <- cv.t(y)}
a.t<- as.data.frame(rbindlist(output.t))
a.t
##  now we can just cbind a.t to a.em
a.em$cv.treat<-a.t$cv.treat
a.em$variable<-rep(names(dan)[c(16:31)], each=1)
names(a.em)

head(a.em)

game<-gather(a.em, "type","CV",c(1:7))

game$name[game$type=="cv.year"]<-"Year"
game$name[game$type=="cv.sp"]<-"Species"
game$name[game$type=="cv.sta"]<-"Stand"
game$name[game$type=="cv.plot"]<-"Plot"
game$name[game$type=="cv.site"]<-"Site"
game$name[game$type=="cv.prepost"]<-"Pre-Post"
game$name[game$type=="cv.treat"]<-"Treatment"
game$name[game$type=="cv.tree"]<-"Tree"

game$name<-factor(game$name, levels=c("Treatment","Plot","Site","Stand","Pre-Post","Species","Year","Tree"))

ggplot(game, aes(x=name, y=CV, fill=name))+geom_bar(stat="identity")+facet_wrap(~variable, scales="free")+
  coord_flip()


write.csv(game, file="CV_first_CV.csv")
#write.csv(game, file="mean_first_CV.csv")

length(unique(dan$Stand))

m<-aggregate(game$CV, by=list(Name=game$name), FUN="mean", na.rm=T)

m

order(m$x)

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
