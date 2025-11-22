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
#dati <- read.csv("tree_spectra_processed.csv")
dada<-dada[,-1]

# stand ages
dada$Age[dada$Stand=="C1"]<-"Young forest"
dada$Age[dada$Stand=="C2"]<-"Young forest"
dada$Age[dada$Stand=="C3"]<-"Young forest"
dada$Age[dada$Stand=="C4"]<-"Mid-aged forest"
dada$Age[dada$Stand=="C5"]<-"Mid-aged forest"
dada$Age[dada$Stand=="C6"]<-"Mid-aged forest" 
dada$Age[dada$Stand=="C7"]<-"Mature forest"
dada$Age[dada$Stand=="C8"]<-"Mature forest"
dada$Age[dada$Stand=="C9"]<-"Mature forest"

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
dim(gat)

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


avg_pri$Age <- factor(avg_pri$Age, levels=c("Young forest",
                                            "Mid-aged forest",
                                            "Mature forest"))

sel_age <- "Young forest"

model <- lm(pri ~ Ntrmt*Ptrmt + Stand, data=avg_pri[avg_pri$Age== sel_age,])
young_pri_mod_tree <- as.data.frame(anova(model))
young_pri_mod_tree$Age <- sel_age

sel_age <- "Mid-aged forest"
model <- lm(pri ~ Ntrmt*Ptrmt + Stand, data=avg_pri[avg_pri$Age== sel_age,])

mid_pri_mod_tree <-as.data.frame( anova(model))
mid_pri_mod_tree$Age <- sel_age

##########
sel_age <- "Mature forest"

model <- lm(pri ~ Ntrmt*Ptrmt + Stand, data=avg_pri[avg_pri$Age== sel_age,])

old_pri_mod_tree <- as.data.frame(anova(model))
old_pri_mod_tree$Age <- sel_age



pri_results_anova <- rbind(old_pri_mod_tree, mid_pri_mod_tree, young_pri_mod_tree)

write.csv(pri_results_anova, file=here::here("R_output","PRI_results.csv"))

##########

head(gat)

plot_data <- gat %>%
  group_by(Age, Stand, staplo, Ntrmt, Ptrmt) %>%
  summarise(pri_mean = mean(pri))


ndf_wide <- plot_data[, c(1,2,4,5,6)] %>%
  pivot_wider(
    names_from = Ntrmt,
    values_from = pri_mean
  )


ndf_wide$Age <- factor(ndf_wide$Age , 
                       values= c("Young forest","Mid-aged forest","Mature forest"))


gN <- ggplot(ndf_wide, aes(x=NoN, y=N, col= Ptrmt))+
  geom_point()+
  geom_line(aes(group=Stand), col="black")+
  facet_wrap(~Age)+
  scale_color_manual(values=c("black","red"))+
  geom_abline(linetype="dashed")+
  theme_bw()+
  coord_fixed()+
  theme(panel.grid = element_blank())

gN

#######

pdf_wide <- plot_data[, c(1,2,4,5,6)] %>%
  pivot_wider(
    names_from = Ptrmt,
    values_from = pri_mean
  )
pdf_wide

pdf_wide$Age <- factor(pdf_wide$Age , 
                       values= c("Young forest","Mid-aged forest","Mature forest"))

gP <- ggplot(pdf_wide, aes(x=NoP, y=P, col= Ntrmt))+
  geom_point()+
  geom_line(aes(group=Stand), col="black")+
  facet_wrap(~Age)+
  scale_color_manual(values=c("blue", "black"))+
  geom_abline(linetype="dashed")+
  theme_bw()+
  coord_fixed()+
  theme(panel.grid = element_blank())

gP
