
##################
##### PCA ####
spec.pca <- prcomp(dat_lda[,-1],center = TRUE, scale = TRUE) ## means per treat_stand
# spec.pca <- prcomp(dada[,-c(1:5)],center = TRUE, scale = TRUE) ## pixels
plot(spec.pca,type="l")
summary(spec.pca)

plot(spec.pca)



head(pcdat)
pcdat$Trt<-factor(pcdat$Trt, levels=c("Control","N","P","NP"))
ggplot(pcdat, aes(x=PC1, y=PC2,size=5, col=Trt))+geom_point()+scale_color_manual(values=c("black","blue","red","purple"))+
  guides(size=F)

PC1<-spec.pca$x[,1]
PC2<-spec.pca$x[,2]
PC3<-spec.pca$x[,3]
PC4<-spec.pca$x[,4]
PC5<-spec.pca$x[,5]
PC6<-spec.pca$x[,6]

pcdat<-data.frame(Trt=dat_lda[,1],PC1,PC2,PC3,PC4, PC5, PC6)
# pcdat<-data.frame(dada[,1:5],PC1,PC2,PC3,PC4, PC5, PC6)
biplot(pcdat)
# chem$treat_stand <- paste(chem$Trt, chem$Stand, sep="_")
pcdada <- merge(chem[,-c(1:2)],pcdat, by="staplo", all.y = T)
with(pcdada,plot(PC1,PC2,col=as.factor(Trt), pch=16))
with(pcdada,plot(PC1,PC2,col=as.factor(Stand), pch=16)) ### grouped by stand



--------------------------------------------------------------------------------------------
