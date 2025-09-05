### Ordinations ################
### Anna Schweiger Nov 6 2019- Alex Young 10_4_2020 ###
library(tidyverse)

library(MASS)
library(plotly)
library(vegan)
library(agricolae)
library(here)
library(caret)



## dada contains the tree top reflectance.This was made in file 2. 
dada<- read.csv(here::here("data_folder","actual_tops.csv"))
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
chem <-  read.csv(here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))
chem[chem$trmt=="Con", "trmt"] <- "Control"
chem$treat_stand<-paste(chem$Stand, chem$trmt)

head(chem)

library(tidyr)
# gather spectra for averaging
names(dada)
spectra_gather<-gather(dada, "wvl","refl",6:350)
table(spectra_gather$height)

names(spectra_gather)
spectra_gather$plot<-paste(spectra_gather$Stand, spectra_gather$Treatment)
head(spectra_gather)
table(spectra_gather$Stand)

# calculate plot-level average
names(spectra_gather)
dadam <-aggregate(list(refl=spectra_gather$refl), by=list(Stand=spectra_gather$Stand,Age=spectra_gather$Age, wvl=spectra_gather$wvl, Treatment=spectra_gather$Treatment, Plot=spectra_gather$plot), FUN="mean", na.rm=T)
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
pre_lda<-spread(spectra_gather, wvl,refl) ### means
names(pre_lda)
head(pre_lda[1:10])
dim(pre_lda)
names(pre_lda)

#######################
dat_lda<-pre_lda[,c(4,10:353)]

# proportion explained by treatment
lda_res <- lda(as.factor(Treatment) ~ . , data = dat_lda, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- lda_res$svd^2/sum(lda_res$svd^2)*100) ### variability explained
out <-  as.data.frame(as.matrix(dat_lda[,-1]) %*% as.matrix(lda_res$scaling))

# examine variation by stand
stand_lda<-pre_lda[,c(5,9:352)]
stand_lda_res <- lda(as.factor(Stand) ~ . , data = stand_lda, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- stand_lda_res$svd^2/sum(stand_lda_res$svd^2)*100) ### variability explained
#out <-  as.data.frame(as.matrix(dat_lda[,-1]) %*% as.matrix(lda_res$scaling))

age_lda<-pre_lda[,c(8,10:353)]
age_lda_res <- lda(as.factor(Age) ~ . , data = age_lda, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- age_lda_res$svd^2/sum(age_lda_res$svd^2)*100) ### variability explained



## Add back in plot level information
out$Stand<-pre_lda$Stand
out$Age<-pre_lda$Age
out$Treatment<-pre_lda$Treatment
out$Treatment<-factor(out$Treatment, levels=c("Control","N","P","NP"))

out$staplo<-paste(out$Stand, out$Treatment)
out$total_N<-chem$total_N[match(out$staplo, chem$treat_stand )]
out$total_P<-chem$P[match(out$staplo, chem$treat_stand )]



out
##  tree-level
names(dada)
dada<-dada[complete.cases(dada),]

t.lda<-dada[,c(4,6:350)]
names(t.lda)


lres <- lda(as.factor(Treatment) ~., data = t.lda, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- lres$svd^2/sum(lres$svd^2)*100) ### variability explained
out <-  as.data.frame(as.matrix(t.lda[,-1]) %*% as.matrix(lres$scaling))

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
#dev.off()
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



### 







