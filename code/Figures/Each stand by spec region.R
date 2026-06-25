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
dada <- read.csv(here::here( "R_output","processed_spectra3.csv"))

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
gat<-tidyr::spread(ldada, "wvl","refl")


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


ggplot(nir, aes(x = WVL, y = value, col = Treatment)) +
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
  )



##########################################

## NIR
names(gat)
plat <- gather(gat, "WVL", "value", 100:150)

plat <- aggregate(list(value = plat$value), by=list(
  WVL = plat$WVL,
  Stand = plat$Stand,
  Age = plat$Age,
  Treatment = plat$Treatment),
  FUN= "mean",na.rm=T)

plat$WVL <- as.numeric(plat$WVL)


ggplot(plat, aes(x = WVL, y = value, col = Treatment)) +
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
  )
