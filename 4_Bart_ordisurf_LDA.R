### Ordinations ################
### Anna Schweiger Nov 6 2019- Alex Young 10_4_2020 ###
library(tidyverse)
library(MASS)
library(plotly)
library(vegan)
library(agricolae)



## dada contains the tree top reflectance.This was made in file 2. 
dada<-read.csv("actual_tops_10_03_shade_less_than_0.8.csv")
dada<-dada[,-1]   # when saving the .csv, the first column values are just X

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
chem <- read.csv("R_input\\bart_resin_melnhe_10_30_2019_Young.csv")
chem$Trt[chem$Trt=="Con"] <- "Control"
chem$treat_stand<-paste(chem$Stand, chem$Trt)


# gather spectra for averaging
spectra_gather<-gather(dada, "wvl","refl",7:351)
head(spectra_gather)
table(spectra_gather$Stand)
# calculate plot-level average
dadam <-aggregate(list(refl=spectra_gather$refl), by=list(Stand=spectra_gather$Stand,Age=spectra_gather$Age, wvl=spectra_gather$wvl, Treatment=spectra_gather$Treatment), FUN="mean", na.rm=T)
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

dat_lda<-pre_lda[,3:348]
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


par(mfrow=c(1,2))
plot(out$LD1, out$LD2, type="n",bty="l",col="grey50", xlab="LD 1 (67%)",ylab="LD 2 (25%)")
title(main="LDA ordination for nutrient treatment with soil N availability",   cex.main=1.5,adj = 0)
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=2)

#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_N,out,add=T, col="grey50", lwd=1.5, labcex=1.2)
legend("topleft", legend = unique(out$Treatment), pch=19,col=c("black","blue","red","purple")[as.factor(out$Treatment)] ,bty ="n", cex=1.3) 

#### P
plot(out$LD1, out$LD2, type="n",bty="l", col="grey50",xlab="LD 1 (67%)",ylab="LD 2 (25%)",  cex.lab=1.5)
title(main="b",   cex.main=2,adj = 0)
points( main="a",out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=2)
#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points

ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_P,out,add=T, col="grey50", lwd=1.5, labcex=1.2)
legend("topleft", legend = unique(out$Treatment), pch=19,col=c("black","blue","red","purple")[as.factor(out$Treatment)] ,bty ="n", cex=1.3) 

######
#3 add stand age
## bap is from two to 10 and 10 plus

#333 ALex needs to add this here. 
head(bap)
out$ba<-bap$BA[match(out$staplo, bap$stat)]


(prop.lda <- res$svd^2/sum(res$svd^2)*100) ### variability explained
prop.lda


75.61381 + 24.38619

dev.off()
par(mfrow=c(1,1))
plot(out$LD1, out$LD2, type="n",bty="l", main="Basal area", xlab="LD 1 (76%)",ylab="LD 2 (24%)")
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=2)
text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points

ordisurf(out[,c(1,2)]~ba,out,add=T, col="grey50", lwd=1.5, labcex=1.2)

legend("bottomleft", legend = unique(out$Age), pch=c(16,17,15)[as.factor(out$Age)] ,bty ="n", cex=1.3) 


# is this the place to 





