
# Variable importance for each age class

wvl <- colnames(read.csv(here::here("data_folder","actual_tops.csv")))
# select wvl columns
wvl <- wvl[8:352]
wvl_nm <- round(as.numeric(sapply(strsplit(wvl, '_'), `[`, 2)),0)

y_vip <- read.csv(here::here("R_output","PLSDA_output","Young forest","vip_scores.csv"))
m_vip <- read.csv(here::here("R_output","PLSDA_output","Mid-aged forest","vip_scores.csv"))
o_vip <- read.csv(here::here("R_output","PLSDA_output","Mature forest","vip_scores.csv"))

y_vip$wvl <- wvl_nm
m_vip$wvl <- wvl_nm
o_vip$wvl <- wvl_nm

#one thing to add here is the actual wavelengths. Maybe also the avg spectral profile value

o_vip$Age <- paste0( "Mature forest ", table(o_vip$Important)[2] , " important wavelengths")
m_vip$Age <- paste0( "Mid-aged forest ", table(m_vip$Important)[2] , " important wavelengths")
y_vip$Age <- paste0( "Young forest ", table(y_vip$Important)[2] , " important wavelengths")

vip <- rbind(o_vip, m_vip, y_vip)

# 

g1 <- ggplot(vip, aes(x= wvl, y= VIP_Score))+
  geom_line()+theme_bw()+theme(panel.grid = element_blank())+
  geom_point(data=vip[vip$Important=="TRUE",], col="forestgreen")+
  geom_hline(yintercept=1, linetype = "dashed")+
  labs(x= "Wavelength", y="Variable Importance Value",
       legend="Young forest PLSDA")+
  annotate('rect', xmin=1340, xmax=1455, ymin=0, ymax=2.5, alpha=.2, fill='gray')+
  annotate('rect', xmin=1790, xmax=1995, ymin=0, ymax=2.5, alpha=.2, fill='gray')+
  facet_wrap(~Age, nrow=3)+
  scale_x_continuous(breaks=seq(400, 2500, 100))

g1
