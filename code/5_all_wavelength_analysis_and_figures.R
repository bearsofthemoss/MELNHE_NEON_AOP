## Alex Young 10/21/2019

## Test all bands for treatment effects and for differences in age
## MELNHE stands in Bartlett, NH-  NEON AOP reflectance.
library(ggplot2)
library(lmerTest)
library(lme4)
library(tidyr)
library(dplyr)
library(agricolae)
library(emmeans)
## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada <- read.csv(here::here("data_folder", "processed_spectra3.csv"))
summary(dada$winRadius)



age_class <- c("Young","Mid-aged","Mature")



# stand ages
dada$Age[dada$Stand=="C1"]<-"Young"
dada$Age[dada$Stand=="C2"]<-"Young"
dada$Age[dada$Stand=="C3"]<-"Young"
dada$Age[dada$Stand=="C4"]<-"Mid-aged"
dada$Age[dada$Stand=="C5"]<-"Mid-aged"
dada$Age[dada$Stand=="C6"]<-"Mid-aged" 
dada$Age[dada$Stand=="C7"]<-"Mature"
dada$Age[dada$Stand=="C8"]<-"Mature"
dada$Age[dada$Stand=="C9"]<-"Mature"

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

vis_mod <- lmer( a535 ~ Ntrmt * Ptrmt * Age + (1|Stand), data=xya)
visdf <- as.data.frame(anova(vis_mod))
visdf$model <- "Visible"

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

xya$Age <- factor(xya$Age, levels=c("Young","Mid-aged","Mature"))
library(tidyr)
library(ggplot2)

xya$Age <- factor(xya$Age, levels=c("Young","Mid-aged","Mature"))

band_levels <- c("a535","a735","a985")
band_labels <- c(a535="535 nm", a735="735 nm", a985="985 nm")

raw_long <- xya %>%
  pivot_longer(c(a535, a735, a985), names_to="Band", values_to="Refl") %>%
  mutate(Band = factor(Band, levels=band_levels))

avg_long <- raw_long %>%
  group_by(Age, Treatment, Band) %>%
  summarise(Refl = mean(Refl), .groups="drop")

# treatment shapes: Control=square(22), NP=diamond(23), N=down-tri(25), P=up-tri(24)
trt_shapes <- c(Control=22, NP=23, N=25, P=24)
trt_cols   <- c(Control="black", N="blue", P="red", NP="purple")

xya$Age <- factor(xya$Age, levels=c("Young","Mid-aged","Mature"))
band_levels <- c("a535","a735","a985")
band_labels <- c(a535="535 nm", a735="735 nm", a985="985 nm")
raw_long <- xya %>%
  pivot_longer(c(a535, a735, a985), names_to="Band", values_to="Refl") %>%
  mutate(Band = factor(Band, levels=band_levels))
avg_long <- raw_long %>%
  group_by(Age, Treatment, Band) %>%
  summarise(Refl = mean(Refl), .groups="drop")

# treatment shapes: Control=square(22), NP=diamond(23), N=down-tri(25), P=up-tri(24)
trt_shapes <- c(Control=22, NP=23, N=25, P=24)
trt_cols   <- c(Control="black", N="blue", P="red", NP="purple")



fig_5 <- ggplot(mapping=aes(x=Age, y=Refl, shape=Treatment)) +
  # raw values: solid fill, low alpha
  # geom_point(data=raw_long, aes(col=Treatment, fill=Treatment),
  #            size=1.8, alpha=.30) +
  geom_point(data=raw_long, aes(col=Treatment, fill=Treatment),
             size=0.8, alpha=.30,
             position=position_jitterdodge(jitter.width=0, dodge.width=0.4)) +
  # averages: hollow (fill=NA)
  geom_point(data=avg_long, aes(col=Treatment),
             size=2.5, stroke=1.4, fill=NA) +
  scale_color_manual(values=trt_cols) +
  scale_fill_manual(values=trt_cols) +
  scale_shape_manual(values=trt_shapes) +
  facet_wrap(~ Band, scales="free_y", nrow=1,
             labeller=labeller(Band=band_labels)) +
  labs(y="Normalised reflectance", x="Forest Age") +
  theme_bw() +
  theme(panel.grid       = element_blank(),
        strip.background  = element_blank(),
        strip.text        = element_text(face=2, hjust=0),
        legend.position   = "right",
        panel.spacing     = unit(0.6, "lines")) +
  guides(
    shape = guide_legend(override.aes = list(alpha=1, size=3.5)),
    fill = "none"
  )




ggsave("figure_5.png", fig_5,
       width = 8, height = 3.5, dpi = 300, bg = "white")

# #################################################################################
# 
# # Analysis of PRI
# 
