## Alex Young 10/21/2019

## Test all bands for treatment effects and for differences in age
## MELNHE stands in Bartlett, NH-  NEON AOP reflectance.
library(ggplot2)
library(lmerTest)
library(lme4)
library(tidyr)
library(dplyr)
library(agricolae)

## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada <- read.csv(here::here("data_folder", "processed_spectra3.csv"))
summary(dada$winRadius)



age_class <- c("Young forest","Mid-aged forest","Mature forest")



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
ldada<-tidyr::gather(dada, "wvl","refl",8:352)
ldada$wvl<-as.numeric(gsub(".*_","",ldada$wvl))
ldada<-na.omit(ldada) # take out NA values- about half were NA 10_3 Ary
ldada$staplo<-paste(ldada$Stand, ldada$Treatment)



# min,max, and mean number of tree tops by plot.
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
gat<-tidyr::spread(ldada, "wvl","refl")


xya <- aggregate( list(a535 = gat$`534`,
                       a735 = gat$`734.31`,
                       a985 = gat$`984.71`),
                  by=list(Stand = gat$Stand,
                          Treatment = gat$Treatment,
                          Ntrmt = gat$Ntrmt,
                          Ptrmt = gat$Ptrmt,
                          Age = gat$Age),
                  FUN="mean", na.rm=T)

xya

car_mod <- lmer( a535 ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
cardf <- as.data.frame(anova(car_mod))
cardf$model <- "Carotenoids"

red_mod <- lmer( a735 ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
reddf <- as.data.frame(anova(red_mod))
reddf$model <- "Red edge"

nir_mod <- lmer( a985 ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
nirdf <- as.data.frame(anova(nir_mod))
nirdf$model <- "NIR"

nir_mod2 <- lmer( a985 ~ Ntrmt   + (1|Stand), data=xya[xya$Ptrmt!="P",])
nirdf2 <- as.data.frame(anova(nir_mod2))

emm_NIR <- emmeans(nir_mod2, ~ Ntrmt)
emm_NIR
((0.0918 - 0.0915) / (0.0915)) *100

emm_vis <- emmeans(vis_mod2, ~ Ntrmt)
(0.0055 - 0.00654) / (0.00654) * 100

emm_RED <- emmeans(red_mod2, ~ Ntrmt)
emm_RED
((0.0561  - 0.0588 ) / 0.0588 ) * 100


red_mod2 <- lmer( a735 ~ Ntrmt   + (1|Stand), data=xya[xya$Ptrmt!="P",])
reddf2 <- as.data.frame(anova(red_mod2))

vis_mod2 <- lmer( a535 ~ Ntrmt   + (1|Stand), data=xya[xya$Ptrmt!="P",])
visdf2 <- as.data.frame(anova(vis_mod2))


nirdf2$model <- "NIR2"
nirdf2


mod_output <- rbind(cardf, reddf, nirdf)

write.csv(mod_output, file="anova_output.csv")


###########################################

xya$Age <- factor(xya$Age, levels=c("Young forest","Mid-aged forest","Mature forest"))

fg1 <-ggplot(xya)+
  geom_point(aes(x=Stand, y=a535, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="535 nm normalised reflectance",
       x="")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="none")
fg1

fg2 <-ggplot(xya)+
  geom_point(aes(x=Stand, y=a735, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="735 nm normalised reflectance",
       x="")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="bottom")
fg2

fg3 <-ggplot(xya)+
  geom_point(aes(x=Stand, y=a985, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="985 nm normalised reflectance",
       x="")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="none")
fg3
library(patchwork)

f5 <-  fg1 + fg2  +  fg3  

 
 ggsave("figure_5.png", f5, 
        width = 9, height = 3.5, dpi = 300, bg = "white")
 
 

###############################
gat$pri550 <-(gat$`528.99`- gat$`549.02`)/(gat$`528.99` +gat$`549.02`)
gat$pri570<-(gat$`528.99` - gat$`569.06`)/( gat$`528.99` + gat$`569.06`)


names(gat)

pri_mod <- lmer( pri570 ~ Ntrmt * Ptrmt * Age + (1|Stand/staplo), data=gat)

pri_res <- as.data.frame(anova(pri_mod))

pri_res$source <- rownames(pri_res)
pri_res$DenDF <-round( pri_res$DenDF, 2)
pri_res$`F value` <-round( pri_res$`F value`, 2)
pri_res$`Pr(>F)` <-round( pri_res$`Pr(>F)`, 2)


pri_res[pri_res$`Pr(>F)`< 0.01 ,"Pr(>F)"] <- "< 0.01"

# write.csv(pri_res[,c("source","NumDF","DenDF","F value","Pr(>F)")],
#           file="PRI_anova_results.csv")



pri_res
names(gat)
vis <- gather(gat, "WVL", "value",  24:70)

av <- aggregate(list(value = vis$value), by=list(
           WVL = vis$WVL,
           Stand = vis$Stand,
           Age = vis$Age,
           Treatment = vis$Treatment),
           FUN= "mean",na.rm=T)
str(av)
av$WVL <- as.numeric(av$WVL)
vis_reg <- ggplot(av, aes(x = WVL, y = value, col = Treatment)) +
  geom_line(aes(group = Treatment)) +
  facet_wrap(~ Stand, scales = "free_y") +
  scale_color_manual(values = c("black", "blue", "red", "purple")) +
  theme_bw() +
  geom_vline(xintercept= 535 , linetype="dashed")+
  labs(y = "Normalized reflectance", x = "Wavelength (nm)") +
  theme(
    legend.position  = "bottom",
    
    # ── move strip labels inside, top-left ──────────────────────────────
    strip.background  = element_blank(),   # remove grey box entirely
    strip.text        = element_text(
      hjust   = 0,  # push toward left edge
      vjust   = .5,     # push toward top
      margin  = margin(b = 4),  # pull text into panel
      size    = 12
    ),
    
    # ── reduce whitespace between panels ────────────────────────────────
    panel.spacing.x  = unit(0.2, "lines"),
    panel.spacing.y  = unit(0.4, "lines")
  )+
  theme_bw()+theme(panel.grid = element_blank())

vis_reg

ggsave("figure_S2.png", vis_reg, 
       width = 8, height = 4, dpi = 300, bg = "white")



## Red edge
names(gat)
re <- gather(gat, "WVL", "value",  76:80)

red <- aggregate(list(value = re$value), by=list(
  WVL = re$WVL,
  Stand = re$Stand,
  Age = re$Age,
  Treatment = re$Treatment),
  FUN= "mean",na.rm=T)
str(red)
red$WVL <- as.numeric(red$WVL)

red_reg <- ggplot(red, aes(x= WVL, y= value, 
               col=Treatment ))+
  geom_line(aes(group = Treatment))+
  facet_wrap(~Stand, scales="free_y")+
  scale_color_manual(values= c("black","blue","red","purple"))+
  theme_bw()+
  geom_vline(xintercept= 735 , linetype="dashed")+
  labs(y="Normalized reflectance", 
       x="Wavelength (nm)")+
  theme(legend.position="bottom")+
  theme_bw()+theme(panel.grid= element_blank())


ggsave("figure_S3.png", red_reg, 
       width = 6, height = 4, dpi = 300, bg = "white")



############


names(gat)
nir <- gather(gat, "WVL", "value",  126:130)

nir <- aggregate(list(value = nir$value), by=list(
  WVL = nir$WVL,
  Stand = nir$Stand,
  Age = nir$Age,
  Treatment = nir$Treatment),
  FUN= "mean",na.rm=T)
str(nir)
nir$WVL <- as.numeric(nir$WVL)

nir_reg <- ggplot(nir, aes(x= WVL, y= value, 
                col=Treatment ))+
  geom_line(aes(group = Treatment))+
  facet_wrap(~Stand, scales="free_y")+
  scale_color_manual(values= c("black","blue","red","purple"))+
  theme_bw()+
  geom_vline(xintercept= 985 , linetype="dashed")+
  labs(y="Normalized reflectance", 
       x="Wavelength (nm)")+
  theme(legend.position="right",
        panel.grid = element_blank())

nir_reg

ggsave("figure_S4.png", nir_reg, 
       width = 6, height = 4, dpi = 300, bg = "white")


library(emmeans)
library(ggplot2)

# Get estimated marginal means for the P × Age interaction
# (marginalising over N treatment)
emm_PA <- emmeans(pa570, ~ Ptrmt * Ntrmt+Age)
emm_df  <- as.data.frame(emm_PA)

# Rename for clarity
names(emm_df)[names(emm_df) == "emmean"] <- "emmean"

avg_pri$Age <- factor(avg_pri$Age, levels=c(
  "Young forest","Mid-aged forest","Mature forest"
))

emm_df$Age <- factor(emm_df$Age, levels=c(
  "Young forest","Mid-aged forest","Mature forest"
))


emm_df[emm_df$Ptrmt=="NoP" & emm_df$Ntrmt == "NoN", "Treatment"] <- "Control"
emm_df[emm_df$Ptrmt=="NoP" & emm_df$Ntrmt == "N", "Treatment"] <- "N"
emm_df[emm_df$Ptrmt=="P" & emm_df$Ntrmt == "NoN", "Treatment"] <- "P"
emm_df[emm_df$Ptrmt=="P" & emm_df$Ntrmt == "N", "Treatment"] <- "NP"


emm_df$Treatment <- factor(emm_df$Treatment, levels=c("Control","N","P", "NP"))
library(ggbeeswarm)
library(ggnewscale)

prin <- spread(avg_pri[ , c("Stand","Age","pri570","Ntrmt","Ptrmt")], "Ntrmt","pri570")

# prin[prin$Ntrmt=="No N","Ntrmt"] <- "No added N"
# prin[prin$Ntrmt=="Yes N","Ntrmt"] <- "Added N"
prin[prin$Ptrmt=="No N","Ptrmt"] <- "No added P"
prin[prin$Ptrmt=="Yes N","Ptrmt"] <- "Added P"

fxy <- ggplot(prin,
               aes(x=NoN, y= N, shape=Age, group=Stand))+
  geom_point(aes(color=Ptrmt), size=3)+
  scale_color_manual(values=c("red","black"))+
  geom_line(aes(group=Stand))+
  geom_abline()+
  coord_fixed()+
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )
fxy

ggsave("figure_5xy_P.png", fxy, 
       width = 7, height = 3.5, dpi = 300, bg = "white")



avg_pri$Treatment <- factor(avg_pri$Treatment, levels = c("Control", "N", "P", "NP"))

pri_fig_570 <- ggplot() +
  geom_point(
    data = avg_pri,
    aes(x     = Treatment,
        y     = pri570,
        col  = Treatment
        ,group = interaction(Treatment, Age, lex.order = TRUE)
        )
    ,
    position  = position_dodge(0.7),
    alpha     = 0.4
  ) +
  
  scale_color_manual(
    values = c("Control" = "black", "N" = "blue", "P" = "red", "NP" = "purple"),
    name   = "Treatment"
  ) +

  labs(
    x = "Age class",
    y = "PRI"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )+
  facet_wrap(~Age)

pri_fig_570


ggsave("figure_5a.png", pri_fig_570, 
       width = 7, height = 3.5, dpi = 300, bg = "white")

#################################################################################

# Analysis of PRI

