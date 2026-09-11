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

str(vis)

av <- aggregate(list(value = vis$value), by=list(
  WVL = vis$WVL,
  Stand = vis$Stand,
  Age = vis$Age,
  Treatment = vis$Treatment),
  FUN= "mean",na.rm=T)
str(av)

av$WVL <- as.numeric(av$WVL)


# one row per panel, positioned at WVL you choose
labels_df <- data.frame(
  Stand = unique(av$Stand),
  WVL   = 440,
  value = 0.009
)



fig_6 <- ggplot(av, aes(x = WVL, y = value, col = Treatment)) +
  geom_line(aes(group = Treatment), linewidth = 0.5, show.legend = FALSE) +
 # geom_point(aes(group = Treatment), size=1, shape=18, fill=NA, alpha=.4, show.legend = FALSE) +
  geom_point(aes(fill = Treatment), shape = 22, size = 0, stroke = 0,
             alpha = 0, show.legend = TRUE) +
  geom_text(data = labels_df, aes(x = WVL, y = value, label = Stand),
            inherit.aes = FALSE, hjust = 0, vjust = 0, size =7) +
  geom_vline(xintercept = 535, linetype="dashed")+
  facet_wrap(~ Stand, ncol = 3) +
  scale_color_manual(values = c(Control = "black", N = "blue",
                                P = "red", NP = "purple")) +
  scale_fill_manual(values = c(Control = "black", N = "blue",
                               P = "red", NP = "purple")) +
  labs(x = "Wavelength (nm)", y = "Normalized reflectance", fill = "Treatment") +
  theme_bw() +
  theme(
    panel.spacing   = unit(0, "lines"),
    strip.text      = element_blank(),
    strip.background = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "bottom"
  ) +
  guides(
    col  = "none",
    fill = guide_legend(override.aes = list(size = 4, alpha = 1))
  )

fig_6

ggsave("figure_6.png", fig_6,
       width = 8, height = 4, dpi = 300, bg = "white")

## Red edge
names(gat)
re <- gather(gat, "WVL", "value",  69:83)

nir <- aggregate(list(value = re$value), by=list(
  WVL = re$WVL,
  Stand = re$Stand,
  Age = re$Age,
  Treatment = re$Treatment),
  FUN= "mean",na.rm=T)
str(nir)
nir$WVL <- as.numeric(nir$WVL)

library(ggplot2)

library(ggplot2)

# one row per panel, positioned at WVL ~690, value 0.06
labels_df <- data.frame(
  Stand = unique(nir$Stand),
  WVL   = 685,
  value = 0.06
)


fig_7 <- ggplot(nir, aes(x = WVL, y = value, col = Treatment)) +
  geom_line(aes(group = Treatment), linewidth = 0.5, show.legend = FALSE) +
 # geom_point(aes(group = Treatment), size=2, shape=18, fill=NA, alpha=.4, show.legend = FALSE) +
  geom_point(aes(fill = Treatment), shape = 22, size = 0, stroke = 0,
             alpha = 0, show.legend = TRUE) +
  geom_text(data = labels_df, aes(x = WVL, y = value, label = Stand),
            inherit.aes = FALSE, hjust = 0, vjust = 0, size =7) +
  geom_vline(xintercept = 735, linetype="dashed")+
  facet_wrap(~ Stand, ncol = 3) +
  scale_color_manual(values = c(Control = "black", N = "blue",
                                P = "red", NP = "purple")) +
  scale_fill_manual(values = c(Control = "black", N = "blue",
                               P = "red", NP = "purple")) +
  labs(x = "Wavelength (nm)", y = "Normalized reflectance", fill = "Treatment") +
  theme_bw() +
  theme(
    panel.spacing   = unit(0, "lines"),
    strip.text      = element_blank(),
    strip.background = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "bottom"
  ) +
  guides(
    col  = "none",
    fill = guide_legend(override.aes = list(size = 4, alpha = 1))
  )

fig_7

ggsave("figure_7.png", fig_7,
       width = 8, height = 4, dpi = 300, bg = "white")

##########################################

## NIR
names(gat)
plat <- gather(gat, "WVL", "value", 120:143)

plat <- aggregate(list(value = plat$value), by=list(
  WVL = plat$WVL,
  Stand = plat$Stand,
  Age = plat$Age,
  Treatment = plat$Treatment),
  FUN= "mean",na.rm=T)

plat$WVL <- as.numeric(plat$WVL)


# one row per panel, positioned at WVL you choose
labels_df <- data.frame(
  Stand = unique(plat$Stand),
  WVL   = 940,
  value = 0.097
)



fig_8 <- ggplot(plat, aes(x = WVL, y = value, col = Treatment)) +
  geom_line(aes(group = Treatment), linewidth = 0.5, show.legend = FALSE) +
#  geom_point(aes(group = Treatment), size=2, shape=18, fill=NA, alpha=.4, show.legend = FALSE) +
  geom_point(aes(fill = Treatment), shape = 22, size = 0, stroke = 0,
             alpha = 0, show.legend = TRUE) +
  geom_text(data = labels_df, aes(x = WVL, y = value, label = Stand),
            inherit.aes = FALSE, hjust = 0, vjust = 0, size =7) +
  geom_vline(xintercept = 985, linetype="dashed")+
  facet_wrap(~ Stand, ncol = 3) +
  scale_color_manual(values = c(Control = "black", N = "blue",
                                P = "red", NP = "purple")) +
  scale_fill_manual(values = c(Control = "black", N = "blue",
                               P = "red", NP = "purple")) +
  labs(x = "Wavelength (nm)", y = "Normalized reflectance", fill = "Treatment") +
  theme_bw() +
  theme(
    panel.spacing   = unit(0, "lines"),
    strip.text      = element_blank(),
    strip.background = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "bottom"
  ) +
  guides(
    col  = "none",
    fill = guide_legend(override.aes = list(size = 4, alpha = 1))
  )

fig_8

ggsave("figure_8.png", fig_8,
       width = 8, height = 4, dpi = 300, bg = "white")



