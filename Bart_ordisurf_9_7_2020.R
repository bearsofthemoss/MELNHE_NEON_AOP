### Ordinations ################
### Anna Schweiger Nov 6 2019- Alex Young May 2020 ###
library(tidyverse)
library(MASS)
library(plotly)
library(vegan)
library(agricolae)




dada<-read.csv("C:\\Users\\Dropcopter2\\Documents\\actual_tops8.csv")

head(dada)



chem <- read.csv("C:\\Users\\Dropcopter2\\Documents\\R\\hyperspectral R\\bart_resin_melnhe_10_30_2019_Young.csv")
chem$Trt[chem$Trt=="Con"] <- "Control"
chem$treat_stand<-paste(chem$Stand, chem$Trt)


dadam <- dada[complete.cases(dada),] ### pixels

names(dadam)



########################
###### LDA ##############
#  maximizes group differences
names(dadam)
dat_lda <- dadam[,c(4,5:349)] ### means

res <- lda(as.factor(Treatment) ~., data = dat_lda, CV=F) ### try resampling spectra to coarser resolution

out <-  as.data.frame(as.matrix(dat_lda[,-1]) %*% as.matrix(res$scaling))
str(out)

out <- cbind(dadam[,c( )],out)

out$Treatment<-dadam$Treatment
out$Treatment<-factor(out$Treatment, levels=c("Control","N","P","NP"))
out$Stand<-dadam$Stand
out$staplo<-paste(dadam$Stand, dadam$Treatment)
out$total_N<-chem$total_N[match(out$staplo, chem$treat_stand )]
out$total_P<-chem$P[match(out$staplo, chem$treat_stand )]
out$Age<-dadam$Age


(prop.lda <- res$svd^2/sum(res$svd^2)*100) ### variability explained

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




