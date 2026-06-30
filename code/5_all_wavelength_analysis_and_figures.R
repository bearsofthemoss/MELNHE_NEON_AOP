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


xya <- aggregate( list(a531 = gat$`528.99`,
                       a570 = gat$`569.06`,
                       a440 = gat$`438.85`,
                       a480 = gat$`478.91`),
                  by=list(Stand = gat$Stand,
                          Treatment = gat$Treatment,
                          Ntrmt = gat$Ntrmt,
                          Ptrmt = gat$Ptrmt,
                          Age = gat$Age),
                  FUN="mean", na.rm=T)

xya

ggplot(xya, aes( x= a570, y=a531, col=Treatment))+
  geom_point()+facet_wrap(~Stand)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(x="570 nm", y="531 nm")+
  theme_bw()+
  geom_abline(linetype="dashed")

### Calculate PRI

xya$pri <- (xya$a531 - xya$a570) / (xya$a531 + xya$a570)


pri_mod <- lmer( pri ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
anova(pri_mod)


chla_mod <- lmer( a440 ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
anova(chla_mod)

chlb_mod <- lmer( a480 ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
anova(chlb_mod)

car_mod <- lmer( a531 ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
anova(car_mod)

ref_mod <- lmer( a570 ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
anova(ref_mod)


# diff
xya$diff <- xya$a531 - xya$a570

ref_mod <- lmer( diff ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
anova(ref_mod)



###########################################
xya

xya$n531 <- xya$a531 / sum(xya$a531)
xya$n440 <- xya$a440 / sum(xya$a440)
xya$n480 <- xya$a480 / sum(xya$a480)
xya$n570 <- xya$a570 / sum(xya$a570)

head(xya)
#sel <- xya[ , c(1:5,10:14)]

xya$Age <- factor(xya$Age, levels=c("Young forest","Mid-aged forest","Mature forest"))

fg1 <-ggplot(xya)+
  geom_point(aes(x=Age, y=n440, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="440 nm normalized reflectance",
       x="Forest stand")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="none")
fg1

fg2 <-ggplot(xya)+
  geom_point(aes(x=Age, y=n480, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="480 nm normalized reflectance",
       x="Forest stand")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="none")
fg2
fg3 <-ggplot(xya)+
  geom_point(aes(x=Age, y=n531, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="531 nm normalized reflectance",
       x="Forest stand")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="none")
fg3
fg4 <-ggplot(xya)+
  geom_point(aes(x=Age, y=n570, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="570 nm normalized reflectance",
       x="Forest stand")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="none")
fg4
fg5 <-ggplot(xya)+
  geom_point(aes(x=Age, y=pri, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="PRI",
       x="Forest stand")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="none")

fg5

fgdif <-ggplot(xya)+
  geom_point(aes(x=Age, y=diff, group=Treatment, col=Treatment),
             position= position_dodge(.4), size =3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(y="531 - 570 nm difference",
       x="Forest stand")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position="none")

fgdif

fg5 + fgdif

library(cowplot)

( fg1 + fg2 ) / ( fg3  + fg4 )  



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
vis <- gather(gat, "WVL", "value",  20:60)

av <- aggregate(list(value = vis$value), by=list(
           WVL = vis$WVL,
           Stand = vis$Stand,
           Age = vis$Age,
           Treatment = vis$Treatment),
           FUN= "mean",na.rm=T)
str(av)
av$WVL <- as.numeric(av$WVL)
ggplot(av, aes(x = WVL, y = value, col = Treatment)) +
  geom_line(aes(group = Treatment)) +
  facet_wrap(~ Stand, scales = "free_y") +
  scale_color_manual(values = c("black", "blue", "red", "purple")) +
  theme_bw() +
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
  geom_vline( xintercept = 440, linetype = "dashed", col="forestgreen")+
  geom_vline( xintercept = 480, linetype = "dashed",col="forestgreen")+
  geom_vline( xintercept = 531, linetype = "solid",col="orange")+
  geom_vline( xintercept = 570, linetype = "dotted",col="black")



## Red edge
names(gat)
re <- gather(gat, "WVL", "value",  72:77)

nir <- aggregate(list(value = re$value), by=list(
  WVL = re$WVL,
  Stand = re$Stand,
  Age = re$Age,
  Treatment = re$Treatment),
  FUN= "mean",na.rm=T)
str(nir)
nir$WVL <- as.numeric(nir$WVL)

ggplot(nir, aes(x= WVL, y= value, 
               col=Treatment ))+
  geom_line(aes(group = Treatment))+
  facet_wrap(~Stand, scales="free_y")+
  scale_color_manual(values= c("black","blue","red","purple"))+
  theme_bw()+
  labs(y="Normalized reflectance", 
       x="Wavelength (nm)")+
  theme(legend.position="bottom")


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

