## Alex Young 10/21/2019

## Test all bands for treatment effects and for differences in age
## MELNHE stands in Bartlett, NH-  NEON AOP reflectance.
library(ggplot2)
library(lmerTest)
library(lme4)
library(tidyr)
library(data.table)

## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada<-read.csv("R_input/actual_tops_10_26_greater_0.1.csv")
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

# make a 'long' version of dada
ldada<-gather(dada, "wvl","refl",7:351)
ldada$wvl<-as.numeric(gsub(".*_","",ldada$wvl))
ldada<-na.omit(ldada) # take out NA values- about half were NA 10_3 Ary
ldada$staplo<-paste(ldada$Stand, ldada$Treatment)

# min,max, and mean number of tree tops by plot.  6 is probably too low right?
min(table(ldada$staplo))/345
max(table(ldada$staplo))/345
mean(table(ldada$staplo))/345

#### this is for line plots
li<-subset(ldada, ldada$wvl<=1340)
sw<-subset(ldada, ldada$wvl>=1445 & ldada$wvl<=1790)
ir<-subset(ldada, ldada$wvl>=1995)
# check the values
li$group<-"1"
sw$group<-"2"
ir$group<-"3"
# move forward without 'bad' bands
ldada<-rbind(li,sw,ir)
ldada$tree<-paste(ldada$Stand, ldada$Treatment, ldada$treeID)
ldada$group.tree<-paste(ldada$tree, ldada$group)
#################################################################################################

#### For figure of abs loadings
### calculate average plot level reflectance by wavelength
ldada.mean <-aggregate(list(refl=ldada$refl), by=list( wvl=ldada$wvl, group=ldada$group), FUN="mean", na.rm=T)
abs.tr<-read.csv("R_output/PLSDA_abs_loadings_treat_10comps.csv")
abs.age<-read.csv("R_output/PLSDA_abs_loadings_age_11comps.csv")

# bring in abs into ldada.mean
ldada.mean$abs.load.treatment<-abs.tr$mean_abs_loading[match(ldada.mean$wvl, abs.tr$wvl)]
ldada.mean$abs.load.age<-abs.age$mean_abs_loading[match(ldada.mean$wvl, abs.age$wavelength)]
abs<-gather(ldada.mean, "type","value",4:5)
abs$group<-paste(abs$group, abs$type)
head(abs)
### Graphs for abs loadinggs in comparison to spectra
f2<-ggplot(abs, aes(x=wvl, y=value, linetype=type, group=group))+geom_line(lwd=.8)+theme_classic()+
  theme(text=element_text(size=16))+xlab("wavelength (nm)")+ylab("abs(loading)")+ggtitle("b)  Importance for prediction")+
  scale_linetype_manual(values=c("solid","dotdash"))+theme(legend.position = c(.95, .95),
    legend.justification = c("right", "top"), legend.box.just = "right",legend.margin = margin(2, 2, 2, 2))
f2

f1<-ggplot(ldada, aes(x=wvl,col=Stand,group=group.tree, y=refl))+geom_line()+theme_classic()+
  theme(text=element_text(size=16))+xlab("wavelength (nm)")+ylab("abs(loading)")+ggtitle("a)  Hyperspectral reflectance for all trees")+
  theme(legend.position = "right")

f1 
library(ggpubr)
ggarrange(f1, f2, nrow=2)


# Just view 1 stand
C1_ldada<-ldada[ldada$staplo=="C7 Control",]
ggplot(C1_ldada, aes(x=wvl,col=Stand,group=group.tree, y=refl))+geom_line()
#######################################################################################################

## Univariate analysis
# for N*P Anova
ldada$Treatment<-factor(ldada$Treatment, levels=c("Control","N","P","NP"))
ldada$Ntrmt <- factor(  ifelse(ldada$Treatment == "N" | ldada$Treatment == "NP", "N", "NoN"))
ldada$Ptrmt <- factor(  ifelse(ldada$Treatment %in% c("P", "NP"), "P", "NoP"))

# addplot basal area
tree<-read.csv("R_input/10+cm.csv")
tree<-tree[tree$Plot!="5",] # no calcium
library(tidyr)
bap<-aggregate(tree$BA.m2, list(Stand=tree$Stand, Plot=tree$Plot, Age=tree$Age), FUN="sum", simplify=T)
bap$staplo<-paste(bap$Stand, bap$Plot)
bap$Treatment<-sapply(bap$staplo,switch,
                       "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                       "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                       "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                       "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                       "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                       "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                       "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                       "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                       "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N")
bap$staplo<-paste(bap$Stand, bap$Treatment)
ldada$BA<-bap$x[match(ldada$staplo, bap$staplo)]

## calculate plot-level PRI avg
gat<-spread(ldada, "wvl","refl")
gat$pri<-(gat$`528.76`-gat$`553.8`)/(gat$`528.76` +gat$`553.8`)
gav<-aggregate(list(pri=gat$pri), by=list(Stand=gat$Stand, Treatment=gat$Treatment, BA=gat$BA), FUN="mean", na.rm=T)


## choose the bands for the red edge and AVG VIS
red<-subset(ldada, ldada$wvl>400 & ldada$wvl<700)
names(red)
red.wav<-aggregate(list(refl=red$refl), by=list(Stand=red$Stand,wvl=red$wvl, Treatment=red$Treatment,Age=red$Age), FUN="mean")
rd<-aggregate(list(refl=red$refl), by=list(Stand=red$Stand,BA=red$BA, Treatment=red$Treatment,Age=red$Age), FUN="mean")
avg.vis<-aggregate(list(refl=red$refl), by=list(Stand=red$Stand,BA=red$BA, tree=red$group.tree,Treatment=red$Treatment,staplo=red$staplo, Ntrmt=red$Ntrmt, Ptrmt=red$Ptrmt, Age=red$Age), FUN="mean")

#33 graph to show treatment effect in red edge
ggplot(red.wav, aes(x=wvl, y=refl, col=Treatment))+geom_point()+
facet_wrap(~Stand, nrow=3)+scale_color_manual(values=c("black","blue","red","purple"))+theme_classic()+
xlab("Wavelength")+ylab("Normalized reflectance")+ theme(text=element_text(size=22))+
  ggtitle("Visible wavelengths of light")+theme(legend.position="bottom")


############################################


g1<-ggplot(gav, aes(x=BA, y=pri, col=Treatment))+geom_point()+scale_color_manual(values=c("black","blue","red","purple"))+
  scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()+geom_smooth(method="lm", se=F)+
  ylab("Photochemical reflective index")+xlab("Basal area (m2)")+theme(text=element_text(size=20))+ggtitle("a")
g1
g2<-ggplot(rd, aes(x=BA, y=refl, col=Treatment))+geom_point()+
scale_color_manual(values=c("black","blue","red","purple"))+theme_classic()+
  xlab("Basal area (m2)")+ylab("Average VIS reflectance")+ theme(text=element_text(size=20))+
  ggtitle("b")+theme(legend.position="bottom")+geom_smooth(method="lm", se=F)
g2
ggarrange(g1, g2, common.legend=T, legend="bottom")

head(avg.vis)
anova(lmer(refl ~log(BA)+Ntrmt*Ptrmt+(1|Stand/staplo), data=avg.vis))
anova(lmer(pri ~log(BA)+Ntrmt*Ptrmt+(1|Stand/staplo), data=gat))

table(gat$Stand,gat$staplo)

head(gat)
