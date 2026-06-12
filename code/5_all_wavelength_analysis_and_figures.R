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
dada <- read.csv(here::here( "data_folder","processed_spectra3.csv"))

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
ldada<-gather(dada, "wvl","refl",8:352)
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
gat$pri550 <-(gat$`528.99`- gat$`549.02`)/(gat$`528.99` +gat$`549.02`)
gat$pri570<-(gat$`528.99` - gat$`569.06`)/( gat$`528.99` + gat$`569.06`)

head(gat[gat$Stand=="C6",])


avg_pri <-aggregate(list(
                            pri550 = gat$pri550,
                            pri570 = gat$pri570), 
                       by=list(Stand=gat$Stand,
                               Age=gat$Age,
                               Treatment = gat$Treatment,
                               Ntrmt =gat$Ntrmt,
                               Ptrmt = gat$Ptrmt,
                               staplo = gat$staplo), 
                       FUN="mean", na.rm=T)



pa570 <- lmer(pri570 ~ Ntrmt * Ptrmt * Age + (1 | Stand), data = avg_pri)


anova(pa570)




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


out_sel <- list()
out_res <- list()

for(i in 1:3){
  sel <- avg_pri %>% filter(Age== age_class[i] )
  
  seli <- sel %>% group_by(Stand,Ntrmt, Ptrmt,Treatment) %>%
    summarize_at(c("pri"), .funs = mean)
  
  
  modi <-lm(pri~Treatment+Stand,seli) 
  
  tt <- HSD.test(modi, "Treatment") 

# get p-value for contrast
TukeyHSD(aov(modi), "Treatment")
  
  results <- as.data.frame( anova(modi))
  results$age <- age_class[i]
  
  sel$group <- tt$groups$groups[match(sel$Treatment, rownames(tt$groups))]
  
  # Calculate group means and labels for positioning
  group_labels <- sel %>%
    group_by(Treatment) %>%
    summarise(
      max_y = max(pri, na.rm = TRUE),
      group = unique(group)
    )
  
  sel$Treatment <- factor(sel$Treatment, levels=c("Control","N","P","NP"))
  
  sel$group <- group_labels$group[match(sel$Treatment, group_labels$Treatment)]
  sel$max_y <- group_labels$max_y[match(sel$Treatment, group_labels$Treatment)]
  
  sel$age <- age_class[i]
  
  out_sel <- rbind(sel, out_sel)
  out_res <- rbind(results, out_res)
}

options(scipen=999)
out_res
out_sel

out_sel$age <- factor(out_sel$age, levels=c("Young forest","Mid-aged forest","Mature forest"))

library(ggbeeswarm)

fig5 <- ggplot(out_sel, aes(x = Treatment, y = pri, col=Treatment)) + 
  geom_beeswarm(side = -1L, size=3, shape = 19)+
#  ylim(-.17, -.11)+
  # geom_text(aes(x = Treatment, y = -.12, label = group),
  #           vjust = -0.5, size = 5,
  #           inherit.aes = FALSE) +
  facet_wrap(~age, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  scale_color_manual(values=c("black","blue","red","purple"))+
  theme(legend.position = "none") +
  labs(y = "PRI")+
  theme(strip.text = element_text(size = 12))
fig5


ggsave("figure_5.png", fig5, 
       width = 7, height = 3.5, dpi = 300, bg = "white")


gat
names(gat)


# #######################
# library(ggplot2)
# 
# 
# # Community weighted
# cwm <- read.csv(here::here("data_folder","CWM_2021-22.csv"))
lin <- read.csv(here::here("data_folder","litter_N_2018.csv"))

lin[lin$Treatment=="Con", "Treatment"] <- "Control"

lin$staplo <- paste(lin$Stand, lin$Treatment)
cwm$staplo <- paste(cwm$Stand, cwm$Treatment)

avg_pri$foliar_N <- cwm$Ncwm[match(avg_pri$staplo, cwm$staplo)]

avg_pri$litter_N <- lin$N[match(avg_pri$staplo, lin$staplo)]



library(dplyr)
correlations <- avg_pri %>%
  group_by(Treatment) %>%
  summarise(
    r = cor(litter_N, pri, use = "complete.obs"),
    p = cor.test(litter_N, pri)$p.value,
    n = n()
  )

# Create figure with correlation info
pub_fig <- ggplot(avg_pri, aes(x = litter_N, y = pri,
                    shape = Age, col = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.15, aes(group=Treatment)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  ) +
  #  facet_wrap(~Age)+
  scale_color_manual(values = c("black", "blue", "red", "purple")) +
  labs(x = "Litter N (mg/g)", 
       y = "Photochemical Reflectance Index (PRI)",
       color = "Treatment")


pub_fig

# 
# cwm$Treatment <- factor(cwm$Treatment, levels=c("Control","N","P","NP"))




########

# 
# 
# chem <-  read.csv(here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))
# chem[chem$trmt=="Con", "trmt"] <- "Control"
# chem$treat_stand<-paste(chem$Stand, chem$trmt)
# 
# head(chem)
# table(chem$Year)
# 
# avg_pri$N <- chem$NH4.plus.NO3[match(avg_pri$statr, chem$treat_stand )]
# 
# 
# anova(lm( pri ~ Ntrmt * Ptrmt * N + Age +Stand, data = avg_pri))
# 
# library(ggplot2)
# ggplot(avg_pri, aes(x= N, y = pri, col= Treatment, shape=Age))+
#   geom_point(size = 3)+
#   geom_smooth(method = "lm", se=F, aes(group = Treatment))+
#   scale_color_manual(values = c("black","blue","red","purple"))+
#   labs(x="Soil N", y="PRI")
# 




###########################################################

