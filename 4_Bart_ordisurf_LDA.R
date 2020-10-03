### Ordinations ################
### Anna Schweiger Nov 6 2019- Alex Young 9_24_2020 ###
library(tidyverse)
library(MASS)
library(plotly)
library(vegan)
library(agricolae)



## dada contains the tree top reflectance. Add 'Stand Age' column
dada<-read.csv("actual_tops_10_02.csv")
dada<-dada[,-1]


dada$Age[dada$Stand=="C1"]<-"~30 years old"
dada$Age[dada$Stand=="C2"]<-"~30 years old"
dada$Age[dada$Stand=="C3"]<-"~30 years old"
dada$Age[dada$Stand=="C4"]<-"~60 years old"
dada$Age[dada$Stand=="C5"]<-"~60 years old"
dada$Age[dada$Stand=="C6"]<-"~60 years old" 
dada$Age[dada$Stand=="C7"]<-"~100 years old"
dada$Age[dada$Stand=="C8"]<-"~100 years old"
dada$Age[dada$Stand=="C9"]<-"~100 years old"

## AY stopped here 9_25 when he noticed only two plots in C1 are collected. 20 vs 66 treetops?
table(dada$Stand, dada$Treatment)


## chem contains the foliar N and P from 2017 measurements
chem <- read.csv("R_input\\bart_resin_melnhe_10_30_2019_Young.csv")
head(chem)
table(chem$Trt)
#chem$Trt[chem$Trt=="Con"] <- "Control"
chem$treat_stand<-paste(chem$Stand, chem$Trt)

## dadam contains the plot-averaged spectra
names(dada)
spectra_gather<-gather(dada, "wvl","refl",8:352)
head(spectra_gather)
table(spectra_gather$Stand)
# calculate plot-level average
dadam <-aggregate(list(refl=spectra_gather$refl), by=list(Stand=spectra_gather$Stand,Age=spectra_gather$Age, wvl=spectra_gather$wvl, Treatment=spectra_gather$Treatment), FUN="mean", na.rm=T)
dadam <- dadam[complete.cases(dadam),] ### pixels
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
pre_lda<-pre_lda[,3:348]


dat_lda<-pre_lda
head(dat_lda)

res <- lda(as.factor(Treatment) ~., data = dat_lda, CV=F) ### try resampling spectra to coarser resolution

(prop.lda <- res$svd^2/sum(res$svd^2)*100) ### variability explained


out <-  as.data.frame(as.matrix(dat_lda[,-1]) %*% as.matrix(res$scaling))
str(out)

out <- cbind(dadam[,c( )],out)

out$Treatment<-dadam$Treatment
out$Treatment<-factor(out$Treatment, levels=c("Con","N","P","NP"))
out$Stand<-dadam$Stand
out$staplo<-paste(dadam$Stand, dadam$Treatment)
out$total_N<-chem$total_N[match(out$staplo, chem$treat_stand )]
out$total_P<-chem$P[match(out$staplo, chem$treat_stand )]
out$Age<-dadam$Age

head(out)

dev.off()
#par(mfrow=c(1,2))
dev.off()
par(mfrow=c(1,1))
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




