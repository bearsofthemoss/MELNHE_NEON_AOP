## Alex Young 10/21/2019

## Test all bands for treatment effects and for differences in age
## MELNHE stands in Bartlett, NH-  NEON AOP reflectance.
library(ggplot2)
library(lmerTest)
library(lme4)
library(tidyr)
library(data.table)

## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada<- read.csv(here::here("data_folder","actual_tops.csv"))
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

# min,max, and mean number of tree tops by plot.  6 is probably too low right?
min(table(ldada$staplo))/345
max(table(ldada$staplo))/345
mean(table(ldada$staplo))/345


## Univariate analysis
# for N*P Anova
ldada$Treatment<-factor(ldada$Treatment, levels=c("Control","N","P","NP"))
ldada$Ntrmt <- factor(  ifelse(ldada$Treatment == "N" | ldada$Treatment == "NP", "N", "NoN"))
ldada$Ptrmt <- factor(  ifelse(ldada$Treatment %in% c("P", "NP"), "P", "NoP"))

##########


## calculate plot-level PRI avg
gat<-spread(ldada, "wvl","refl")
gat$pri<-(gat$`528.99`- gat$`549.02`)/(gat$`528.99` +gat$`549.02`)

avg_pri <-aggregate(list(height=gat$height,
                            pri = gat$pri), 
                       by=list(Stand=gat$Stand,
                               Age=gat$Age,
                               Treatment = gat$Treatment), 
                       FUN="mean", na.rm=T)

avg_pri$Age <- factor(avg_pri$Age, levels=c("~30 years old",
                                            "~60 years old",
                                            "~100 years old"))

ggplot(avg_pri, aes(x=Age, y=pri,
                           col = Treatment,
                           shape= Treatment))+
  geom_point(position = position_dodge(.5), size= 3)+
  #geom_violin()+
  scale_color_manual( values=c("black","blue","red","purple"))+
  geom_smooth(se=F, method="lm")+
  scale_shape_manual( values=c(1, 3, 4, 8))+
  theme_bw()+theme(panel.grid = element_blank())+
  labs(x="Stand age class", y= "Average Photochemical Reflective Index")

pri_mod <- lmer(pri ~ Ntrmt*Ptrmt+Age+(1|Stand/staplo), data=gat)
anova( pri_mod)

library(emmeans)
emm_object <- emmeans(pri_mod, specs = "Ntrmt")

effect_sizes <- eff_size(emm_object, sigma = sigma(pri_mod), edf = df.residual(pri_mod))
print(effect_sizes)

-.14 - -.152 / ((-.14 + -.152)/2)

.152-.14

.12 / .14
########
names(dada)
vis <-gather(dada, "wvl","refl",c(7:66))

vis$Age <- dada$Age[match(vis$Stand, dada$Stand)]

total_vis<-aggregate(list(vis=vis$refl), 
by=list(Stand=vis$Stand,
        Age=vis$Age, 
        treeID=paste(vis$treeID, vis$Stand),
        Treatment=vis$Treatment), 
FUN="sum", na.rm=T)

total_vis$Ntrmt <- factor(  ifelse(total_vis$Treatment == "N" | total_vis$Treatment == "NP", "N", "NoN"))
total_vis$Ptrmt <- factor(  ifelse(total_vis$Treatment %in% c("P", "NP"), "P", "NoP"))


anova(lmer(vis ~ Ntrmt*Ptrmt+Age+(1|Stand), data=total_vis))

total_vis$Treatment <- factor( total_vis$Treatment, levels=c("Control","N","P","NP"))

ggplot(total_vis, aes(x=Treatment, y=vis, fill = Treatment))+
  geom_col(position= position_dodge(), aes(group=Stand))+
  facet_wrap(~Age, scales="free_x")+
  scale_fill_manual( values=c("black","blue","red","purple"))
