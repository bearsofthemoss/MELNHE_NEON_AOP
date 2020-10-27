### Ordinations ################
### Anna Schweiger Nov 6 2019- Alex Young 10_4_2020 ###
library(tidyverse)
library(MASS)
library(plotly)
library(vegan)
library(agricolae)



## dada contains the tree top reflectance.This was made in file 2. 
dada<-read.csv("R_input/actual_tops_10_26_greater_0.1.csv")
dada<-dada[,-1]   # when saving the .csv, the first column values are just X
names(dada)
# add in stand ages
dada$Age[dada$Stand=="C1"]<-"~30 years old"
dada$Age[dada$Stand=="C2"]<-"~30 years old"
dada$Age[dada$Stand=="C3"]<-"~30 years old"
dada$Age[dada$Stand=="C4"]<-"~60 years old"
dada$Age[dada$Stand=="C5"]<-"~60 years old"
dada$Age[dada$Stand=="C6"]<-"~60 years old" 
dada$Age[dada$Stand=="C7"]<-"~100 years old"
dada$Age[dada$Stand=="C8"]<-"~100 years old"
dada$Age[dada$Stand=="C9"]<-"~100 years old"


## chem contains the resin available N and P from 2017 measurements
chem <- read.csv("R_input/bart_resin_melnhe_10_30_2019_Young.csv")
chem$treat_stand<-paste(chem$Stand, chem$Trt)

head(chem)

library(tidyr)
# gather spectra for averaging
spectra_gather<-gather(dada, "wvl","refl",7:351)
head(spectra_gather)
table(spectra_gather$Stand)
# calculate plot-level average
dadam <-aggregate(list(refl=spectra_gather$refl), by=list(Stand=spectra_gather$Stand,Age=spectra_gather$Age, wvl=spectra_gather$wvl, Treatment=spectra_gather$Treatment, Plot=spectra_gather$layer), FUN="mean", na.rm=T)
dadam <- dadam[complete.cases(dadam),] ### pixels
# convert wavelengths to just have numeric values
dadam$wvl<-as.numeric(gsub(".*_","",dadam$wvl))
names(dadam)
head(dadam)


########################
###### LDA ##############
#  maximizes group differences
names(dadam)
dim(dadam)
library(tidyr)
head(dadam)
pre_lda<-spread(dadam, wvl,refl) ### means
head(pre_lda[1:10])
dim(pre_lda)
dat_lda<-pre_lda[,c(3,5:349)]
head(dat_lda[1:10,1:10])
res <- lda(as.factor(Treatment) ~., data = dat_lda, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- res$svd^2/sum(res$svd^2)*100) ### variability explained
out <-  as.data.frame(as.matrix(dat_lda[,-1]) %*% as.matrix(res$scaling))

## Add back in plot level information
out$Stand<-pre_lda$Stand
out$Age<-pre_lda$Age
out$Treatment<-pre_lda$Treatment
out$Treatment<-factor(out$Treatment, levels=c("Control","N","P","NP"))

out$staplo<-paste(out$Stand, out$Treatment)
out$total_N<-chem$total_N[match(out$staplo, chem$treat_stand )]
out$total_P<-chem$P[match(out$staplo, chem$treat_stand )]


##  tree-level
names(dada)
dada<-dada[complete.cases(dada),]
t.lda<-dada[,c(5,7:349)]
names(t.lda)


res <- lda(as.factor(Treatment) ~., data = t.lda, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- res$svd^2/sum(res$svd^2)*100) ### variability explained
out <-  as.data.frame(as.matrix(t.lda[,-1]) %*% as.matrix(res$scaling))

## Add back in plot level information
out$Stand<-dada$Stand
out$Age<-dada$Age
out$Treatment<-dada$Treatment
out$Treatment<-factor(out$Treatment, levels=c("Control","N","P","NP"))
table(out$Treatment)
out$staplo<-paste(out$Stand, out$Treatment)
out$total_N<-chem$total_N[match(out$staplo, chem$treat_stand )]
out$total_P<-chem$P[match(out$staplo, chem$treat_stand )]

#3
dev.off()
par(mfrow=c(1,2))
plot(out$LD1, out$LD2, type="n",bty="l",col="grey50", xlab="LD 1 (71%)",ylab="LD 2 (23%)")
title(main="a",   cex.main=1.5,adj = 0)
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=1)

#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_N,out,add=T, col="grey50", lwd=1.5, labcex=1.2)
legend("topleft", legend = unique(out$Treatment), pch=19,col=c("black","blue","red","purple")[out$Treatment] ,bty ="n", cex=1.3) 

#### P
plot(out$LD1, out$LD2, type="n",bty="l", col="grey50",xlab="LD 1 (71%)",ylab="LD 2 (23%)",  cex.lab=1.5)
title(main="b",   cex.main=  1.5,adj = 0)
points( main="a",out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=1)
#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points

ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_P,out,add=T, col="grey50", lwd=1.5, labcex=1.2)
legend("topleft", legend = unique(out$Treatment), pch=19,col=c("black","blue","red","purple")[out$Treatment] ,bty ="n", cex=1.3) 
legend("topright", legend = unique(out$Age), pch=c(16,17,15)[as.factor(unique(out$Age))] ,bty ="n", cex=1.3) 

######
#3 add stand age
## bap is from two to 10 and 10 plus
tree<-read.csv("data_folder/10+cm.csv")
tree$staplo<-paste(tree$Stand, tree$Plot) 
tree<-tree[tree$Plot!="5",] # no calcium

#33## make  bap
bap<-aggregate(tree$BA.m2, list(staplo=tree$staplo,Plot=tree$Plot , Stand=tree$Stand, Age=tree$Age), FUN="sum", simplify=T)
bap

#333#333#############
age_lda<-pre_lda[,c(2,5:349)]
head(age_lda[1:10,1:10])


res <- lda(as.factor(Age) ~., data = age_lda, CV=F) ### try resampling spectra to coarser resolution

(prop.lda <- res$svd^2/sum(res$svd^2)*100) ### variability explained


out <-  as.data.frame(as.matrix(age_lda[,-1]) %*% as.matrix(res$scaling))

## Add back in plot level information
out$Stand<-pre_lda$Stand
out$Plot<-pre_lda$Plot
out$staplo<-paste(out$Stand, out$Plot)
out$Treatment<-pre_lda$Treatment
out$Age<-pre_lda$Age
#
out$ba<-bap$x[match(out$staplo, bap$staplo )]

dev.off()
par(mfrow=c(1,1))

out$Treatment<-factor(out$Treatment, levels=c("Control","N","P","NP"))
plot(out$LD1, out$LD2, type="n",bty="l", main="Basal area", xlab="LD 1 (76%)",ylab="LD 2 (24%)")
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=2)
#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points

ordisurf(out[,c(1,2)]~ba,out,add=T, col="grey50", lwd=1.5, labcex=1.2)

legend("topright", legend = unique(out$Age), pch=c(16,17,15)[as.factor(unique(out$Age))] ,bty ="n", cex=1.3) 

legend("topleft", legend = unique(out$Treatment), pch=23,col=c("black","blue","red","purple")[as.factor(out$Treatment)] ,bty ="n", cex=1.3) 


##############################



#Here we could ask how much the tree species explained the spectral variation by plot

############ quick adonis test
## Add back in plot level information
dada$staplo<-paste(dada$Stand, dada$Treatment)
dada$total_N<-chem$total_N[match(dada$staplo, chem$treat_stand )]
dada$total_P<-chem$P[match(dada$staplo, chem$treat_stand )]
dada$bap<-bap$x[match(dada$staplo, bap$staplo)]


names(dada)
spec.matrix<-dada[,7:351]
adonis(spec.matrix ~ dada$total_N, data=dada, permutations = 100, method = "bray",strata = dada$Stand)

spec.pca <- prcomp(spec.matrix ,center = TRUE, scale = TRUE) ## means per treat_stand
# spec.pca <- prcomp(dada[,-c(1:5)],center = TRUE, scale = TRUE) ## pixels
plot(spec.pca,type="l")
summary(spec.pca)

plot(spec.pca)



head(pcdat)
pcdat$Trt<-factor(pcdat$Trt, levels=c("Control","N","P","NP"))

PC1<-spec.pca$x[,1]
PC2<-spec.pca$x[,2]
PC3<-spec.pca$x[,3]
PC4<-spec.pca$x[,4]
PC5<-spec.pca$x[,5]
PC6<-spec.pca$x[,6]

 pcdat<-data.frame(PC1,PC2,PC3,PC4, PC5, PC6)
dim(dada)
perm<-cbind(dada[,c(1:6,352:356) ],pcdat)

head(perm[1:10])

adonis(perm[,7:12] ~ perm$Treatment,method="euclidean", strata=perm$Stand, data=perm)
