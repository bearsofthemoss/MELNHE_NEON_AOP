### Ordinations ################

# At the plot level
library(tidyverse)

library(MASS)
library(plotly)
library(vegan)
library(agricolae)
library(here)
library(caret)
library(dplyr)
library(ggforce)  # for stat_ellipse
library(metR)     # for geom_contour_fill or use stat_contour


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
dadam <-aggregate(list(refl=spectra_gather$refl), 
                  by=list(Stand=spectra_gather$Stand,
                          Age=spectra_gather$Age, 
                          wvl=spectra_gather$wvl, 
                          Treatment=spectra_gather$Treatment, 
                          Plot=spectra_gather$plot), 
                  FUN="mean", na.rm=T)

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



# for just Young stands

#######################
young_lda<-pre_lda[pre_lda$Age== "~30 years old",c(4,8:352)]

nzv <- nearZeroVar(young_lda[,-1], saveMetrics = TRUE)
problem_vars <- which(nzv$nzv == TRUE)

# Remove vars with 0 variance
if(length(problem_vars) > 0) {
  young_lda_cleaned <- young_lda[, -(problem_vars + 1)]
  cat("Removed", length(problem_vars), "near-zero variance variables\n")
}



# proportion explained by treatment
lda_res <- lda(as.factor(Treatment) ~ . , data = young_lda_cleaned, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- lda_res$svd^2/sum(lda_res$svd^2)*100) ### variability explained
young_out <-  as.data.frame(as.matrix(young_lda_cleaned[,-1]) %*% as.matrix(lda_res$scaling))





## Add back in plot level information
young_out$Stand<-pre_lda[pre_lda$Age=="~30 years old", "Stand"]
young_out$Age<-pre_lda[pre_lda$Age=="~30 years old", "Age"]
young_out$Treatment<-pre_lda[pre_lda$Age=="~30 years old", "Treatment"]
young_out$Treatment<-factor(young_out$Treatment, levels=c("Control","N","P","NP"))

young_out$staplo<-paste(young_out$Stand, young_out$Treatment)
young_out$total_N<-chem$NH4.hyphen.N[match(young_out$staplo, chem$treat_stand )]
young_out$total_P<-chem$PO4.hyphen.P[match(young_out$staplo, chem$treat_stand )]

out <- young_out

par(mfrow=c(1,2))
plot(out$LD1, out$LD2, type="n",bty="l",col="grey50", xlab="LD 1 (71%)",ylab="LD 2 (23%)")
title(main="Soil Nitrogen",   cex.main=1.5,adj = 0)
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=1)

#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_N,out,add=T, col="grey50", lwd=1.5, labcex=1.2)
legend("topleft", legend = unique(out$Treatment), pch=19,col=c("black","blue","red","purple")[out$Treatment] ,bty ="n", cex=1.3) 

###
plot(out$LD1, out$LD2, type="n",bty="l",col="grey50", xlab="LD 1 (71%)",ylab="LD 2 (23%)")
title(main="Soil Phosphorus",   cex.main=1.5,adj = 0)
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=1)

#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_P,out,add=T, col="grey50", lwd=1.5, labcex=1.2)



#Here we could ask how much the tree species explained the spectral variation by plot

############ quick adonis test
## Add back in plot level information
dada$staplo<-paste(dada$Stand, dada$Treatment)
dada$total_N<-chem$total_N[match(dada$staplo, chem$treat_stand )]
dada$total_P<-chem$PO4.hyphen.P[match(dada$staplo, chem$treat_stand )]
dada$bap<-bap$x[match(dada$staplo, bap$staplo)]


names(dada)
spec.matrix<-dada[,7:351]
adonis2(spec.matrix ~ total_N, data=dada, permutations = 100, method = "bray",strata = dada$Stand)

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
pcdat
dim(dada)
perm<-cbind(dada[,c(1:5,351:355) ],pcdat)

names(dada)
head(perm[1:10])
names(perm)
adonis(perm[,11:16] ~ perm$Treatment,method="euclidean", strata=perm$Stand, data=perm)


