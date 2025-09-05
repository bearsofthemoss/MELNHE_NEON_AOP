
# Variable importance for each age class

wvl <- colnames(read.csv(here::here("data_folder","actual_tops.csv")))
# select wvl columns
wvl <- wvl[8:352]
wvl_nm <- round(as.numeric(sapply(strsplit(wvl, '_'), `[`, 2)),0)

vip <- read.csv(here::here("R_output","PLSDA_output_September","vip_scores.csv"))

vip$wvl <- wvl_nm

#one thing to add here is the actual wavelengths. Maybe also the avg spectral profile value

g1 <- ggplot(vip, aes(x= wvl, y= VIP_Score))+
  geom_line()+theme_bw()+theme(panel.grid = element_blank())+
  geom_point(data=vip[vip$Important=="TRUE",], col="forestgreen")+
  geom_hline(yintercept=1, linetype = "dashed")+
  labs(x= "Wavelength", y="Variable Importance Value",
       legend="Young forest PLSDA")+
  annotate('rect', xmin=1340, xmax=1455, ymin=0, ymax=1.5, alpha=.2, fill='gray')+
  annotate('rect', xmin=1790, xmax=1995, ymin=0, ymax=1.5, alpha=.2, fill='gray')+
#  facet_wrap(~Age, nrow=3)+
  scale_x_continuous(breaks=seq(400, 2500, 100))

g1
