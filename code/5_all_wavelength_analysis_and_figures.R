## Alex Young 10/21/2019

## Test all bands for treatment effects and for differences in age
## MELNHE stands in Bartlett, NH-  NEON AOP reflectance.
library(ggplot2)
library(lmerTest)
library(lme4)
library(tidyr)
library(dplyr)

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
names(gat)

pri_Anna <-  gat[ , c("Stand","treeID","height","Treatment","Ntrmt","Ptrmt","pri")]

write.csv(pri_Anna , file="PRI_data_Bartlett_NP.csv")

avg_pri <-aggregate(list(height=gat$height,
                            pri = gat$pri), 
                       by=list(Stand=gat$Stand,
                               Age=gat$Age,
                               Treatment = gat$Treatment,
                               Ntrmt =gat$Ntrmt,
                               Ptrmt = gat$Ptrmt,
                               Stand = gat$Stand,
                               staplo = gat$staplo), 
                       FUN="mean", na.rm=T)

avg_pri$Age <- factor(avg_pri$Age, levels=c("~30 years old",
                                            "~60 years old",
                                            "~100 years old"))

sel_age <- "~30 years old"
plot_data <- gat[gat$Age==sel_age,] %>%
  group_by(Stand, staplo, Ntrmt, Ptrmt) %>%
  summarise(pri_mean = mean(pri))
dim(plot_data)

model <- lmer(pri_mean ~ Ntrmt*Ptrmt + (1|Stand), data=plot_data)
young_pri_mod_tree <- as.data.frame(anova(model))
young_pri_mod_tree$Age <- sel_age

sel_age <- "~60 years old"
plot_data <- gat[gat$Age==sel_age,] %>%
  group_by(Stand, staplo, Ntrmt, Ptrmt) %>%
  summarise(pri_mean = mean(pri))
dim(plot_data)
model <- lmer(pri_mean ~ Ntrmt*Ptrmt + (1|Stand), data=plot_data)
mid_pri_mod_tree <-as.data.frame( anova(model))
mid_pri_mod_tree$Age <- sel_age

##########
sel_age <- "~100 years old"

plot_data <- gat[gat$Age==sel_age,] %>%
  group_by(Stand, staplo, Ntrmt, Ptrmt) %>%
  summarise(pri_mean = mean(pri))
dim(plot_data)
model <- lmer(pri_mean ~ Ntrmt*Ptrmt + (1|Stand), data=plot_data)
old_pri_mod_tree <- as.data.frame(anova(model))
old_pri_mod_tree$Age <- sel_age



pri_results_anova <- rbind(old_pri_mod_tree, mid_pri_mod_tree, young_pri_mod_tree)

write.csv(pri_results_anova, file=here::here("R_output","PRI_results.csv"))

##########


