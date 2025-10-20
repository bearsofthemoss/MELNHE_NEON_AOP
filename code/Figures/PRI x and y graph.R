dati <- read.csv(here::here("data_folder/actual_tops.csv"))

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

unique(a$wvl)

### 530 and 550
a530 <- a[a$wvl==528.99, ]

a550 <- a[a$wvl==549.02, ]


a530$a550 <- a550$value[match(a530$one_tree, a550$one_tree)]


##########

a530$PRI <-   ( a530$value - a530$a550 )/ ( a530$value + a530$a550 )

summary_data <- aggregate(list(
  PRI = a530$PRI ),
  by= list( staplo = a530$staplo,
            Treatment = a530$Treatment,
            Stand = a530$Stand,
            Age = a530$Age),
  FUN= "mean", na.rm=T)

st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

summary_se <- aggregate(list(
  PRI_se = a530$value / a530$a550),
  by= list( staplo = a530$staplo,
            Treatment = a530$Treatment,
            Stand = a530$Stand,
            Age = a530$Age),
  FUN= st.err, na.rm=T)

summary_data$se <- summary_se$PRI_se[match(summary_data$staplo, summary_se$staplo)]



sum_2 <- aggregate(list(
  PRI = summary_data$PRI ),
  by= list(Treatment = summary_data$Treatment,
            Age = summary_data$Age),
  FUN= "mean", na.rm=T)

sum_2_se <- aggregate(list(
  PRI_se = summary_data$PRI ),
  by= list(Treatment = summary_data$Treatment,
           Age = summary_data$Age),
  FUN= st.err, na.rm=T)

sum_2$se <- sum_2_se$PRI_se


ggplot( sum_2, aes(x= Treatment, y= PRI, col=Treatment))+
  geom_point(data=sum_2, aes(x= Treatment, y=PRI), size=3)+
  geom_errorbar(aes(ymin = PRI-se, ymax = PRI+se))+
  facet_wrap(~Age, scales="free_x")+
  labs( y= "PRI  (530 - 550 ) / (530 + 550)",
        col="")+
  scale_color_manual(values=c("black","blue","red","purple"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
                   legend.position = "bottom")+
  geom_jitter(data=summary_data, aes(x= Treatment, y=PRI),
             position=position_dodge(.3),
             alpha = .2)






