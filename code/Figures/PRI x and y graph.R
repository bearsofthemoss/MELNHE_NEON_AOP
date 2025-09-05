dati <- read.csv("./data_folder/actual_tops.csv", row.names = 1)

dati$Age[dati$Stand=="C1"]<-"Young forest"
dati$Age[dati$Stand=="C2"]<-"Young forest"
dati$Age[dati$Stand=="C3"]<-"Young forest"
dati$Age[dati$Stand=="C4"]<-"Mid-aged forest"
dati$Age[dati$Stand=="C5"]<-"Mid-aged forest"
dati$Age[dati$Stand=="C6"]<-"Mid-aged forest" 
dati$Age[dati$Stand=="C7"]<-"Mature forest"
dati$Age[dati$Stand=="C8"]<-"Mature forest"
dati$Age[dati$Stand=="C9"]<-"Mature forest"


head(dati)
names(dati)

library(tidyr)
library(ggplot2)
a <- gather(dati, "wvl","value", 7:351)
a
a$wvl <- as.numeric(sub('.*_', '', a$wvl))

head(a)

a$one_tree <- paste(a$Stand, a$Treatment, a$treeID)

a$Treatment <- factor(a$Treatment, levels=c("Control","N","P","NP"))
a$Age <- factor(a$Age, levels=c("Young forest","Mid-aged forest","Mature forest"))
a$staplo <- paste(a$Stand, a$Treatment)

a$Stand <- paste(a$Stand, a$Age)
ggplot(a[a$wvl<700, ], aes(x= wvl, y= value, col=Treatment))+
facet_wrap(~Stand, nrow=3)+
#  geom_point(alpha = .2, size= .4)+
  geom_line(aes(group= one_tree), alpha=.2)+
  geom_vline(xintercept = 530, linetype = "dashed")+
  geom_vline(xintercept = 550, linetype = "dashed")+
  geom_smooth(linewidth = .8, se=F)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(x="Wavelength (nm)", y="Reflectance",
       title="VIS region 400-700 nm")+
  theme_bw()+theme(panel.grid = element_blank())



ggplot(a[a$wvl>750 & a$wvl<1400, ], aes(x= wvl, y= value, col=Treatment))+
  facet_wrap(~Stand, nrow=3)+
#  geom_point(alpha = .2, size= .4)+
  geom_line(aes(group= one_tree), alpha=.2)+
  geom_smooth(linewidth = .8, se=F)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(x="Wavelength (nm)", y="Reflectance",
       title="Red edge 750-1400 nm")+
  theme_bw()+theme(panel.grid = element_blank())

ggplot(a[a$wvl>1500 & a$wvl<1880, ], aes(x= wvl, y= value, col=Treatment))+
  facet_wrap(~Stand, nrow=3, scales="free")+
#  geom_point(alpha = .2, size= .4)+
  geom_line(aes(group= one_tree), alpha=.2)+
  geom_smooth()+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(x="Wavelength (nm)", y="Reflectance",
       title="SWIR 1500-2000 nm ")+
  theme_bw()+theme(panel.grid = element_blank())


ggplot(a[a$wvl>2100 & a$wvl<2500, ], aes(x= wvl, y= value, col=Treatment))+
  facet_wrap(~Stand, nrow=3)+
  #  geom_point(alpha = .2, size= .4)+
  geom_line(aes(group= one_tree), alpha=.2)+
  geom_smooth()+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(x="Wavelength (nm)", y="Reflectance",
       title="SWIR 2000-2500 nm")+
  theme_bw()+theme(panel.grid = element_blank())

######


### 530 and 550
a530 <- a[a$wvl==528.99, ]

a550 <- a[a$wvl==549.02, ]


a530$a550 <- a550$value[match(a530$one_tree, a550$one_tree)]

head(a530)
ggplot(a530, aes(x= value, y=a550, col=Treatment))+
  geom_point()+
  facet_wrap(~Stand, nrow=3)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(x="530 (nm) reflectance", y="550 (nm) reflectance",
       title="Two wavelengths used in PRI index")+
  theme_bw()+theme(panel.grid = element_blank())


ggplot(a, aes(x=shade_intensity, y=value)) +
  geom_point()

              


##########




# Calculate means and standard errors by treatment
summary_data <- a530 %>%
  group_by(staplo, Treatment,Stand,Age) %>%
  summarise(
    mean_530 = mean(value, na.rm = TRUE),
    se_530 = sd(value, na.rm = TRUE) / sqrt(n()),
    mean_550 = mean(a550, na.rm = TRUE),
    se_550 = sd(a550, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# Create the plot
ggplot(summary_data, aes(x = mean_530, y = mean_550, color = Treatment)) +
  geom_point(size = 3) +
  facet_wrap(~Age, nrow=1)+
  geom_errorbar(aes(ymin = mean_550 - se_550, ymax = mean_550 + se_550), 
                width = 0.0001, size = 0.8) +
  geom_errorbarh(aes(xmin = mean_530 - se_530, xmax = mean_530 + se_530), 
                 height = 0.0001, size = 0.8) +
  scale_color_manual(values = c("black","blue","red","purple")) +
  labs(x = "530 nm reflectance (mean ± SE)", 
       y = "550 nm reflectance (mean ± SE)",
       title = "Treatment effects on PRI wavelengths") +
  theme_bw() +
  coord_fixed()+
  geom_abline(linetype = "dashed")+
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 12, hjust = 0.5)
  )

