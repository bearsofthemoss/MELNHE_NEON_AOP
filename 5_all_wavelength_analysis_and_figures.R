## Alex Young 10/21/2019

## Test all bands for treatment effects and for differences in age
## MELNHE stands in Bartlett, NH-  NEON AOP reflectance.
library(ggplot2)
library(lmerTest)
library(lme4)
library(tidyr)
library(data.table)





## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada<-read.csv("actual_tops_10_04_greater_0.1.csv")
dada<-dada[,-1]

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

# make a 'long' version of dada
ldada<-gather(dada, "wvl","refl",7:351)
ldada$wvl<-as.numeric(gsub(".*_","",ldada$wvl))
ldada<-na.omit(ldada) # take out NA values- about half were NA 10_3 Ary
ldada$staplo<-paste(ldada$Stand, ldada$Treatment)


# look at number of obs per plot
table(ldada$Treatment, ldada$Stand)/345  
table(is.na(ldada$refl), ldada$Treatment) # but alot are NA

# min,max, and mean number of tree tops by plot.  6 is probably too low right?
min(table(ldada$staplo))/345
max(table(ldada$staplo))/345
mean(table(ldada$staplo))/345

dim(ldada)

# view individual trees
 # Here Alex tried to visualize the spectra---


li<-subset(ldada, ldada$wvl<=1340)
sw<-subset(ldada, ldada$wvl>=1445 & ldada$wvl<=1790)
ir<-subset(ldada, ldada$wvl>=1995)
# check the values
li$group<-"1"
sw$group<-"2"
ir$group<-"3"
#
table(li$wvl)
table(sw$wvl)
table(ir$wvl)
# move forward without 'band' bands
ldada<-rbind(li,sw,ir)
ldada$tree<-paste(ldada$Stand, ldada$Treatment, ldada$treeID)
ldada$group.tree<-paste(ldada$tree, ldada$group)

# Just view 1 stand
C1_ldada<-ldada[ldada$staplo=="C3 Control",]

# Nice!
ggplot(C1_ldada, aes(x=wvl,col=Stand,group=group.tree, y=refl))+geom_line()


#ggplot(ldada, aes(x=wvl,col=Treatment,group=group, y=refl))+geom_line()+facet_wrap(~Age, nrow=3)


### calculate average plot level recltance by wavelength
ldada.mean <-aggregate(list(refl=ldada$refl), by=list(Stand=ldada$Stand,Age=ldada$Age, wvl=ldada$wvl, Treatment=ldada$Treatment), FUN="mean", na.rm=T)
head(ldada.mean)

da.mean.stand <-aggregate(list(refl=ldada$refl), by=list(wvl=ldada$wvl,group=ldada$group, Age=ldada$Age,Stand=ldada$Stand), FUN="mean", na.rm=T)
head(da.mean.stand)
dim(da.mean.stand)/337

library(ggplot2)
ggplot(da.mean.stand, aes(x=wvl, y=refl,col=Stand, group=group))+geom_line( )+
theme_classic()+theme(text=element_text(size=16))+
  xlab("")+ylab("Normalized reflectance")+ggtitle("Spectral signature of 9 stands")



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


###########################################################################################

# Do ANova on Pri
#N*P Anova
ldada$Treatment<-factor(ldada$Treatment, levels=c("Control","N","P","NP"))
ldada$Ntrmt <- factor(  ifelse(ldada$Treatment == "N" | ldada$Treatment == "NP", "N", "NoN"))
ldada$Ptrmt <- factor(  ifelse(ldada$Treatment %in% c("P", "NP"), "P", "NoP"))


#######################################################



head(ldada)
names(ldada)

library(tidyr)
gat<-spread(ldada, "wvl","refl")
head(gat)

gat$pri<-(gat$`528.76`-gat$`553.8`)/(gat$`528.76` +gat$`553.8`)

anova(lmer(pri ~Ntrmt*Ptrmt*Age+(1|Stand/staplo), data=gat))

ggplot(gat, aes(x=Treatment, y=pri*-1, fill=Treatment))+geom_boxplot()+
  facet_wrap(~Age)+scale_fill_manual(values=c("grey","blue","red","purple"))+
  theme_classic()+scale_y_log10()








## choose the bands for the red edge
red<-subset(ldada, ldada$wvl>400 & ldada$wvl<700)





rd<-aggregate(list(refl=red$refl), by=list(Stand=red$Stand,wvl=red$wvl, Treatment=red$Treatment, group=red$group, Age=red$Age), FUN="mean")


#33 graph to show treatment effect in red edge
ggplot(rd, aes(x=wvl, y=refl, col=Treatment))+geom_point()+
facet_wrap(~Stand, nrow=3)+scale_color_manual(values=c("black","blue","red","purple"))+theme_classic()+
xlab("Wavelength")+ylab("Normalized reflectance")+ theme(text=element_text(size=22))+
  ggtitle("Visible wavelengths of light")+theme(legend.position="bottom")


############################################

avg.vis<-aggregate(list(refl=red$refl), by=list(Stand=red$Stand, Treatment=red$Treatment, group=red$group, Age=red$Age), FUN="mean")

ggplot(avg.vis, aes(x=Stand, y=refl, col=Treatment))+geom_point()+
scale_color_manual(values=c("black","blue","red","purple"))+theme_classic()+
  xlab("Wavelength")+ylab("Normalized reflectance")+ theme(text=element_text(size=22))+
  ggtitle("Visible wavelengths of light")+theme(legend.position="bottom")


###################################################################################
## write a function
s.lme <- function(y, Age,Stand, Ntrmt, Ptrmt){
  lme1<-anova(lmer(y ~ Ntrmt*Ptrmt+Age+(1|Stand/staplo)))
  return(lme1)}

## create output list to store loop output 
output.lme<-list()
names(dada)
dada$staplo<-paste(dada$Stand, dada$Treatment)
for(i in c(7:351)){ 
  y = dada[,i]
  Ntrmt=dada$Ntrmt
  Ptrmt=dada$Ptrmt
  Age=dada$Age
  Stand= dada$Stand
  staplo= dada$staplo
  output.lme[[i-5]] <- s.lme(y, Age,Stand, Ntrmt, Ptrmt)}

## manipulate loop output
d.lme<- as.data.frame(rbindlist(output.lme))
d.lme$Source<-rep(c("Ntrmt","Ptrmt","Age","N*P"))
d.lme$resp.var<-rep(names(dada)[c(7:351)], each=4)

## spot check, do we see the same result for wavelength 1,010?
d.lme[d.lme$resp.var=="Band_754.12",]

anova(lmer(dada$Band_754.12 ~Ntrmt*Ptrmt+Age+(1|Stand/staplo), data=dada))


### looks like we do. nice!

## re-order dataframe columns for convenience
d.l.result<-d.lme[ ,c(8,7,3,4,1,2,5,6)]
head(d.l.result)



d.l.result$adj.p<-p.adjust(d.l.result$`Pr(>F)`, method="hochberg",n=length(d.l.result$`Pr(>F)`))

d.l.result[d.l.result$adj.p <0.05,]



# export p-value results
dls<-spread(d.l.result[ ,c(1,2,8)] , Source,`Pr(>F)`)
head(dls)



#write.csv(dls, file="./R_output/response_for_wavelenths_10_21_19.csv")

