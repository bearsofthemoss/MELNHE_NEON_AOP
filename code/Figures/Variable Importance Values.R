
# Variable importance for each age class

y_vip <- read.csv(here::here("R_output","PLSDA_output","Young forest","vip_scores.csv"))
m_vip <- read.csv(here::here("R_output","PLSDA_output","Mid-aged forest","vip_scores.csv"))
o_vip <- read.csv(here::here("R_output","PLSDA_output","Mature forest","vip_scores.csv"))



#one thing to add here is the actual wavelengths. Maybe also the avg spectral profile value

o_vip$Age <- paste0( "Mature forest ", table(o_vip$Important)[2] , " important wavelengths")
m_vip$Age <- paste0( "Mid-aged forest ", table(m_vip$Important)[2] , " important wavelengths")
y_vip$Age <- paste0( "Young forest ", table(y_vip$Important)[2] , " important wavelengths")

vip <- rbind(o_vip, m_vip, y_vip)

# 

g1 <- ggplot(vip, aes(x= Variable_Index, y= VIP_Score))+
  geom_line()+theme_bw()+theme(panel.grid = element_blank())+
  geom_point(data=vip[vip$Important=="TRUE",], col="forestgreen")+
  geom_hline(yintercept=1, linetype = "dashed")+
  labs("Wavelength", y="Variable Importance Value",
       legend="Young forest PLSDA")+
  facet_wrap(~Age, nrow=3)

g1
