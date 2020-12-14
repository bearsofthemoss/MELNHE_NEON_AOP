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


co.var <- function(x)(    100*sd(x)/mean(x))
ldada$Treatment<-factor(ldada$Treatment, levels=c("Control","N","P","NP"))
ldada.cv <-aggregate(ldada$refl, by=list( wvl=ldada$wvl,group=paste(ldada$group, ldada$Treatment) , Treatment=ldada$Treatment), co.var)
ggplot(ldada.cv, aes(x=wvl, y=x, col=Treatment, group=group))+geom_line(lwd=.8)+theme_classic()+
  theme(text=element_text(size=14))+xlab("wavelength (nm)")+ylab("Coefficient of Variation")+ggtitle("c)  CV for all wavelengths by Treatment")+
  scale_color_manual(values=c("black","blue","red","purple"))



# bring in abs into ldada.mean
ldada.mean$Treatment_classification<-abs.tr$mean_abs_loading[match(ldada.mean$wvl, abs.tr$wvl)]
ldada.mean$Age_classification<-abs.age$mean_abs_loading[match(ldada.mean$wvl, abs.age$wavelength)]
abs<-gather(ldada.mean, "type","value",4:5)  # for plotting both in one graph
abs$group<-paste(abs$group, abs$type)
head(abs)
### Graphs for abs loadinggs in comparison to spectra

f2<-ggplot(abs, aes(x=wvl, y=value, col=type, group=group))+geom_line(lwd=.8)+theme_classic()+
  theme(text=element_text(size=17))+xlab("wavelength (nm)")+ylab("abs(loading)")+ggtitle("b)  Important wavelengths for classification")+
 theme(legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",legend.margin = margin(2, 2, 2, 2))+
  scale_color_manual(values=c("green","black"))+ theme(legend.title = element_blank())+
  geom_text(x=720, y=.235, size=5,label="710-735", col="black")+
  geom_text(x=940, y=.207, size=5, label="935-945", col="black")+
  geom_text(x=1122, y=.28, size=5, label="1120-1135", col="black")+ylim(0,.3)+
  geom_text(x=1550, y=.19, size=5, label="1545-1555", col="black")
 
f2


quantile(abs$value[abs$type=="Age_classification"], .97)
abs[abs$value>quantile(abs$value[abs$type=="Age_classification"], .96),]

quantile(abs$value[abs$type=="Treatment_classification"], .95)
abs[abs$value>quantile(abs$value[abs$type=="Treatment_classification"], .97),]


f1<-ggplot(ldada, aes(x=wvl,col=Stand,group=group.tree, y=refl))+geom_line()+theme_classic()+
  theme(text=element_text(size=17))+xlab("wavelength (nm)")+ylab("Normalized reflectance")+ggtitle("a)  Tree top spectral reflectance")+
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"), legend.box.just = "right",legend.margin = margin(2, 2, 2, 2))

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
gav<-aggregate(list(pri=gat$pri), by=list(Stand=gat$Stand,Age=gat$Age, Treatment=gat$Treatment, BA=gat$BA), FUN="mean", na.rm=T)


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

################

############################################


g1<-ggplot(gav, aes(x=BA, y=pri, col=Treatment,group=Treatment, shape=Age))+geom_point(size=2,)+scale_color_manual(values=c("black","blue","red","purple"))+
  scale_fill_manual(values=c("grey","blue","red","purple"))+theme_classic()+geom_smooth(method="lm", se=F, size=2, linetype="dashed")+
  ylab("Photochemical reflectance index")+xlab("Basal area"~(m^2)*"")+theme(text=element_text(size=20))+ggtitle("b)")
g1


g2<-ggplot(rd, aes(x=BA, y=refl, shape=Age,size=3, group=Treatment, col=Treatment))+geom_point(size=2)+
scale_color_manual(values=c("black","blue","red","purple"))+theme_classic()+
  xlab("Basal area"~(m^2)*"")+ylab("Average VIS reflectance")+ theme(text=element_text(size=20))+
  ggtitle("a)")+theme(legend.position="bottom")+geom_smooth(method="lm", se=F, size=2)
g2


write.csv(rd, file="avg.vis.csv")

library(ggpubr)
ggarrange(g2, g1, common.legend=T, legend="bottom")

head(avg.vis)
anova(lmer(refl ~log(BA)+Ntrmt*Ptrmt+(1|Stand/staplo), data=avg.vis))
anova(lmer(pri ~log(BA)+Ntrmt*Ptrmt+(1|Stand/staplo), data=gat))

table(gat$Stand,gat$staplo)

head(gat)

######################################


#corr plots

tabs_perc<-read.csv("R_output/PLSDA_confuperc_age_12_2.csv")

col <- colorRampPalette(c("black","black","brown","gold","forestgreen")) 


corrplot::corrplot(tabs_perc, p.mat = tabs_perc, insig = "p-value", sig.level = -1, addCoef.col = 1,
                   tl.srt = 70,col = col(20),cl.lim = c(0, 1),tl.col = 1, tl.offset =1.5, 
                   cl.ratio = 0.2, cl.align.text = "l", cl.cex = 0.9, 
                   mar=c(1,3,3,3))
mtext("Prediction",2,at=3, line=-3, cex=1.3)
mtext("Reference",at = 2, line = 0, cex=1.3)
dev.off()

finmods_age_11comps









##############################

# ggplot for corr plots
trt<-read.csv("R_output/PLSDA_confuperc_treat_12_2.csv")
age<-read.csv("R_output/PLSDA_confuperc_age_12_2.csv")

head(age)
head(trt)

trt$perc<-round(trt[,3], 2)
trt$Prediction<-factor(trt$Prediction, levels=c("Control","N","P","NP"))
trt$Reference<-factor(trt$Reference, levels=c("NP","P","N","Control"))
ggplot(trt, aes(Prediction, Reference )) +
  xlab("Prediction") +ylab("Reference") +
  geom_tile(aes(fill = perc), color = "black") +theme_classic()+
  scale_fill_gradientn(colours = c("red", "light green", "forest green"), values = c(0,0.1,1))+
  stat_bin2d(geom="text", aes(label=perc), size=6)+coord_fixed()+  
  theme(legend.text = element_text(size = 12),
        axis.title=element_text(size=16),
        axis.title.x = element_text(size = 20, margin = margin(10,10,0,0)),
        axis.text.x = element_text(angle = 340, hjust = 1),
        legend.title = element_blank(),
        axis.text=element_text(size=14),legend.position = "none",
        axis.line = element_blank(),  axis.ticks = element_blank(),
        panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill="white"))+  scale_x_discrete(position = "top")





