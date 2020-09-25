## Alex Young 10/21/2019

## Test all bands for treatment effects and for differences in age
## MELNHE stands in Bartlett, NH-  NEON AOP reflectance.
library(ggplot2)
library(lmerTest)
library(lme4)
library(tidyr)
library(data.table)



## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada<-read.csv("actual_tops_9_24.csv")
dada<-gather(dada, "wvl","refl",8:351)



head(dada)

dada<-na.omit(dada)

names(dada)
dada$staplo<-paste(dada$Stand, dada$Treatment)
min(table(dada$staplo))/345
max(table(dada$staplo))/345
mean(table(dada$staplo))/345


dim(dada)
# stand ages
dada$Age[dada$Stand=="C1"]<-"~30 years old"
dada$Age[dada$Stand=="C2"]<-"~30 years old"
dada$Age[dada$Stand=="C3"]<-"~30 years old"
dada$Age[dada$Stand=="C4"]<-"~60 years old"
dada$Age[dada$Stand=="C5"]<-"~60 years old"
dada$Age[dada$Stand=="C6"]<-"~60 years old" 
dada$Age[dada$Stand=="C7"]<-"~100 years old"
dada$Age[dada$Stand=="C8"]<-"~100 years old"
dada$Age[dada$Stand=="C9"]<-"~100 years old"

names(dada)
dada.mean <-aggregate(list(refl=dada$refl), by=list(Stand=dada$Stand,Age=dada$Age, wvl=dada$wvl, Treatment=dada$Treatment), FUN="mean", na.rm=T)
head(dada.mean)
da <-aggregate(list(refl=dada$refl), by=list(wvl=dada$wvl, Age=dada$Age,Stand=dada$Stand), FUN="mean", na.rm=T)
str(da)
da$wvl<-as.numeric(da$wvl)
head(da)

library(ggplot2)
f2<-ggplot(da, aes(x=wvl, y=refl))+geom_point( )+theme_classic()+theme(text=element_text(size=16))+
  xlab("")+ylab("Normalized reflectance")+ggtitle("Spectral signature of 9 stands")
f2


# this was from the most recent PLSDA model to make a figure in the paper.
abs<-read.csv("C:\\Users\\Dropcopter2\\Documents\\R\\hyperspectral R\\PLSDA_abs_loadings_Age and Treatment.csv")

f1<-ggplot(abs[abs$mean_abs_loading>0,], aes(x=wvl, y=mean_abs_loading, shape=Type))+geom_point()+theme_classic()+
  theme(text=element_text(size=16))+xlab("wavelength (nm)")+ylab("abs(loading)")+ggtitle("Importance for prediction")+
  scale_shape_manual(values=c(1,16))+
   theme(legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )+ guides(shape = guide_legend(override.aes = list(size=5)))
f1

library(ggpubr)
ggarrange(f2, f1, nrow=2)





#N*P Anova
dada$Treatment<-factor(dada$Treatment, levels=c("Control","N","P","NP"))
dada$Ntrmt <- factor(  ifelse(dada$Treatment == "N" | dada$Treatment == "NP", "N", "NoN"))
dada$Ptrmt <- factor(  ifelse(dada$Treatment %in% c("P", "NP"), "P", "NoP"))


### manipulate dataframe

names(dada)
library(tidyr)

spec.av<-aggregate(list(refl=dada$refl), by=list(Stand=dada$Stand, wvl=dada$wvl, Age=dada$Age,Treatment=dada$Treatment), FUN="mean", na.rm=T)

s<-spread(spec.av, "wvl","refl")
head(s)



names(s)



## choose the bands for the red edge
red<-subset(dada, dada$wvl>400 & dada$wvl<700)
ir<-subset(dada, dada$wvl>700 & dada$wvl<1100)
swir<-subset(dada, dada$wvl>1100 & dada$wvl<2500)


allw<-aggregate(list(refl=dada$refl), by=list(band=dada$wvl), FUN="mean", na.rm=T)
se <- function(x) sqrt(var(x)/length(x))
allw.se<-aggregate(list(se=dada$refl), by=list(band=dada$wvl), FUN="std")
allw$se<-allw.se$se

rd<-aggregate(list(refl=red$refl), by=list(Stand=red$Stand, band=red$wvl, Age=red$Age,Treatment=red$Treatment), FUN="mean", na.rm=T)
rd$staplo<-paste(rd$Stand, rd$Treatment)


library(ggplot2)
names(dada)


g1<-ggplot(dada, aes(x=wvl, y=refl, group=wvl, col=Age))+geom_point(size=.1)+
  theme_classic()+
  xlab("Wavelength")+ylab("Normalized reflectance")+ theme(text=element_text(size=22))+
  ggtitle("")+theme(legend.position="bottom")
g1


g2<-ggplot(rd, aes(x=band, y=refl, col=Treatment,group=staplo))+geom_point()+
  facet_wrap(~Stand, nrow=4)+scale_color_manual(values=c("black","blue","red","purple"))+theme_classic()+
xlab("Wavelength")+ylab("Normalized reflectance")+ theme(text=element_text(size=22))+
  ggtitle("Visible wavelengths of light")+theme(legend.position="bottom")

library(ggpubr)

ggarrange(g1,g2, nrow=1)


irg<-aggregate(list(refl=ir$refl), by=list(Stand=ir$Stand, band=ir$wvl, Age=ir$Age,Treatment=ir$Treatment), FUN="mean", na.rm=T)
irg$staplo<-paste(irg$Stand, irg$Treatment)

ggplot(irg, aes(x=band, y=refl, col=Treatment,group=staplo))+geom_point()+geom_line()+
  facet_wrap(~Stand, nrow=4)+scale_color_manual(values=c("black","blue","red","purple"))+theme_classic()+
  xlab("Wavelength")+ylab("Normalized reflectance")+ theme(text=element_text(size=20))+
  ggtitle("chlorophyll absorption bands")



irg<-aggregate(list(refl=ir$refl), by=list(Stand=ir$Stand, band=ir$wvl, Age=ir$Age,Treatment=ir$Treatment), FUN="mean", na.rm=T)
irg$staplo<-paste(irg$Stand, irg$Treatment)

ggplot(irg, aes(x=band, y=refl, col=Treatment,group=staplo))+geom_point()+geom_line()+
  facet_wrap(~Stand, nrow=4)+scale_color_manual(values=c("black","blue","red","purple"))+theme_classic()+
  xlab("Wavelength")+ylab("Normalized reflectance")+ theme(text=element_text(size=20))+
  ggtitle("chlorophyll absorption bands")


table(rd$Stand, rd$Treatment)



# calculate median value reflectance for all tree tops in a plot
dim(dada)
names(dada)
dg<-gather(dada,"band","refl",5:349 )

dim(dg)
head(dg)



### 
dg$wvl<-as.numeric(sub(".", "", dg$band))

names(dg)

dg[which(dg$wvl >=1345),9] <- dg[which(dg$wvl >=1345),9]+100
dg[which(dg$wvl >1790),9] <- dg[which(dg$wvl >1790),9] +160




head(dg)
table(dg$wvl)
str(dg)

ggplot(dg, aes(x=wvl, y=refl, col=Treatment))+geom_point()+facet_wrap( ~Stand)


#good <- which((as.numeric(wvl)>400 & as.numeric(wvl)<1340|
#                 as.numeric(wvl)>1445 & as.numeric(wvl)<1790|
#                 as.numeric(wvl)>1955 & as.numeric(wvl)<2400)==T)





an<-aggregate(dg$refl, by=list(Stand=dg$Stand, wvl=dg$wvl, Age=dg$Age,Treatment=dg$Treatment, Ntrmt=dg$Ntrmt, Ptrmt=dg$Ptrmt), FUN="mean", na.rm=T)

head(an)

ggplot(an, aes(x=wvl, y=x,col=Age, group=Age))+geom_point()+geom_smooth()+facet_wrap(~Age)


str(an)
ggplot(an, aes(x=wvl, y=x))+geom_point()

#re-spread data to loop through columns
span<-spread(an, "band","x")

## well, ok.  Now I want to loop over this model statement.
head(span)
anova(lmer(X520 ~Ntrmt*Ptrmt+Age+(1|Stand), data=span))

x520<-ggplot(dada, aes(x=Stand, y=X530, fill=Treatment))+geom_boxplot()+scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()
x540<-ggplot(dada, aes(x=Stand, y=X540, fill=Treatment))+geom_boxplot()+scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()
x560<-ggplot(dada, aes(x=Stand, y=X560, fill=Treatment))+geom_boxplot()+scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()
x600<-ggplot(dada, aes(x=Stand, y=X600, fill=Treatment))+geom_boxplot()+scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()
x690<-ggplot(dada, aes(x=Stand, y=X690, fill=Treatment))+geom_boxplot()+scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()
x720<-ggplot(dada, aes(x=Stand, y=X720, fill=Treatment))+geom_boxplot()+scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()

x720



library(ggpubr)
ggarrange(x520,x540,x560,x600, x690,x720, common.legend = T)






##############################################
ggplot(span, aes(x=Treatment, y=X720, fill=Treatment)) + scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()+
  geom_boxplot(position=position_dodge(0.8))+geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.8))



############################################################################

## see which bands are significant with N or P
dls2$wvl[dls2$Ptrmt<0.05]
dls2$wvl[dls2$Ntrmt<0.05]

# 520, 540, 545, 560, 690, 720, 725

## write a function
s.lme <- function(y, Age,Stand, Ntrmt, Ptrmt){
  lme1<-anova(lmer(y ~ Ntrmt*Ptrmt+Age+(1|Stand)))
  return(lme1)}

## create output list to store loop output 
output.lme<-list()
for(i in c(6:350)){ 
  y = span[,i]
  Ntrmt=span$Ntrmt
  Ptrmt=span$Ptrmt
  Age=span$Age
  Stand= span$Stand
  output.lme[[i-5]] <- s.lme(y, Age,Stand, Ntrmt, Ptrmt)}

## manipulate loop output
d.lme<- as.data.frame(rbindlist(output.lme))
d.lme$Source<-rep(c("Ntrmt","Ptrmt","Age","N*P"))
d.lme$resp.var<-rep(names(span)[c(6:350)], each=4)

## spot check, do we see the same result for wavelength 1,010?
d.lme[d.lme$resp.var=="X705",]
anova(lmer(X705 ~Ntrmt*Ptrmt+Age+(1|Stand), data=span))

### looks like we do. nice!

## re-order dataframe columns for convenience
d.l.result<-d.lme[ ,c(8,7,3,4,1,2,5,6)]
head(d.l.result)

# export p-value results
dls<-spread(d.l.result[ ,c(1,2,8)] , Source,`Pr(>F)`)
head(dls)
#write.csv(dls, file="./R_output/response_for_wavelenths_10_21_19.csv")

# plot results 
library(spectrolab)
dadax <- dada[complete.cases(dada),]
names(dadax)[5:349] <- substr(names(dadax)[5:349],2,nchar(names(dadax)[5:349]))

# dadaxx <- dadax
# ### mask water absorption bands
# wvls <- names(dadaxx)
# out <- wvls[c(192:213,282:315)]
# dadaxx[,names(dadaxx)%in% out] <- NA

### make spectra object
spec <- as.spectra(dadax,name_idx = 1, meta_idxs = c(2:4,350:352))

dls2 <- dls
dls2$wvl <- substr(dls$resp.var,2,nchar(dls$resp.var))
dls2$wvl <- as.numeric(dls2$wvl)
dls2 <- dls2[order(dls2$wvl),]



head(dls2)


# this does a bonferroni correction for the absorption
dls2$Age <- p.adjust(dls2$Age, method = "bonferroni", n = length(dls2$Age))
dls2$Ntrmt <- p.adjust(dls2$Ntrmt, method = "bonferroni", n = length(dls2$Ntrmt))
dls2$Ptrmt <- p.adjust(dls2$Ptrmt, method = "bonferroni", n = length(dls2$Ptrmt))
dls2$`N*P` <- p.adjust(dls2$`N*P`, method = "bonferroni", n = length(dls2$`N*P`))

## see which exact treatments are less than 0.05 after Tukey
dls2$wvl[dls2$Ptrmt<0.05]
dls2$wvl[dls2$Ntrmt<0.05]

############################################################################3333
# Alex adjusted this for N plot and P plot. 
#   pdf("./R_output/sign_wvl.pdf",width = 5,height = 4)


par(mfrow=c(2,1))

# Age
# forest age
plot(mean(spec), xlim=c(400,1300),main="Forest Age",
     col="red3", lwd=2,type="n", xlab="Wavelength (nm)", ylab="")
mtext("normalized reflectance", side = 2, line = 2.2)

segments(x0 = c(dls2$wvl[dls2$`Age`<0.05]), 
         y0=rep(-0.02,length(dls2$wvl[dls2$`Age`<0.05])), 
         x1 = c(dls2$wvl[dls2$`Age`<0.05]), 
         y1=rep(0.5, length(dls2$wvl[dls2$`Age`<0.05])),
         col="green")
plot_quantile(spec, col = rgb(0,0,0,0.5),add=T,border = F)
plot(mean(spec), col="black", lwd=2,add=T)


# N
plot(mean(spec), xlim=c(400,1300),main="Different reflectance with N addition",
     col="red3", lwd=2,type="n", xlab="Wavelength (nm)", ylab="")
mtext("normalized reflectance", side = 2, line = 2.2)
segments(x0 = c(dls2$wvl[dls2$Ntrmt<0.05]), 
         y0=rep(-0.02,length(dls2$wvl[dls2$Ntrmt<0.05])), 
         x1 = c(dls2$wvl[dls2$Ntrmt<0.05]), 
         y1=rep(0.48, length(dls2$wvl[dls2$Ntrmt<0.05])),
         col=rgb(0,0,1,0.4))
plot_quantile(spec, col = rgb(0,0,0,0.5),add=T,border = F)
plot(mean(spec), col="black", lwd=2,add=T)

# P
plot(mean(spec), xlim=c(400,1300),main="Different reflectance with P addition",
     col="red3", lwd=2,type="n", xlab="Wavelength (nm)", ylab="")
mtext("normalized reflectance", side = 2, line = 2.2)
segments(x0 = c(dls2$wvl[dls2$Ptrmt<0.05]), 
         y0=rep(-0.02,length(dls2$wvl[dls2$Ptrmt<0.05])), 
         x1 = c(dls2$wvl[dls2$Ptrmt<0.05]), 
         y1=rep(0.5, length(dls2$wvl[dls2$Ptrmt<0.05])),
         col=rgb(1,0,0,0.4))
plot_quantile(spec, col = rgb(0,0,0,0.5),add=T,border = F)
plot(mean(spec), col="black", lwd=2,add=T)





### mask water absorption bands
# segments(x0 = c(1340:1445), 
#          y0=rep(0,length(1340:1445)), 
#          x1 = c(1340:1445), 
#          y1=rep(0.1,length(1340:1445)),
#          col="white")
# segments(x0 = c(1790:1955), 
#          y0=rep(0,length(1790:1955)), 
#          x1 = c(1790:1955), 
#          y1=rep(0.1,length(1790:1955)),
#          col="white")

legend(1100,0.025, c("N", "P", "N*P"), 
       lwd=1.5,col=c("blue","red","magenta"),bty ="n", cex=0.7) 
dev.off()







