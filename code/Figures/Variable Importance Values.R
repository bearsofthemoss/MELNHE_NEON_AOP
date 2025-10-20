library(ggplot2)
# Variable importance for each age class
wvl <- colnames(read.csv(here::here("data_folder","actual_tops.csv")))
# select wvl columns
wvl <- wvl[8:352]
wvl_nm <- round(as.numeric(sapply(strsplit(wvl, '_'), `[`, 2)),0)
y_vip <- read.csv(here::here("R_output","PLSDA_output_August","Young forest","vip_scores.csv"))
m_vip <- read.csv(here::here("R_output","PLSDA_output_August","Mid-aged forest","vip_scores.csv"))
o_vip <- read.csv(here::here("R_output","PLSDA_output_August","Mature forest","vip_scores.csv"))
y_vip$wvl <- wvl_nm
m_vip$wvl <- wvl_nm
o_vip$wvl <- wvl_nm
#one thing to add here is the actual wavelengths. Maybe also the avg spectral profile value
o_vip$Age <- paste0( "Mature forest")
m_vip$Age <- paste0( "Mid-aged forest")
y_vip$Age <- paste0( "Young forest ")

# Create a grouping variable to break lines at grey regions
o_vip$line_group <- NA
o_vip$line_group[o_vip$wvl < 1340] <- "1"
o_vip$line_group[o_vip$wvl > 1450 & o_vip$wvl < 1780] <-"2"
o_vip$line_group[o_vip$wvl > 1960] <- "3"

y_vip$line_group <- NA
y_vip$line_group[y_vip$wvl < 1340] <- "1"
y_vip$line_group[y_vip$wvl > 1450 & y_vip$wvl < 1780] <- "2"
y_vip$line_group[y_vip$wvl > 1960] <- "3"

m_vip$line_group <- NA
m_vip$line_group[m_vip$wvl < 1340] <- "1"
m_vip$line_group[m_vip$wvl > 1450 & m_vip$wvl < 1780] <- "2"
m_vip$line_group[m_vip$wvl > 1960] <- "3"


vip <- rbind(o_vip, m_vip, y_vip)

vip <- vip[!is.na(vip$line_group),]


summary(vip$wvl/1000)
g1 <- ggplot(vip, aes(x= wvl, y= VIP_Score, group = line_group))+
  geom_line()+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_point(data=vip[vip$Important=="TRUE",], col="forestgreen")+
  geom_hline(yintercept=1, linetype = "dashed")+
  labs(x= "Wavelength (nm)", y="Variable Importance Value",
       legend="Young forest PLSDA")+
  annotate('rect', xmin=1340, xmax=1455, ymin=0, ymax=2.5, alpha=.2, fill='gray')+
  annotate('rect', xmin=1790, xmax=1960, ymin=0, ymax=2.5, alpha=.2, fill='gray')+
  facet_wrap(~Age, nrow=3)+
  scale_x_continuous(breaks=seq(400, 2500, 200))+
  theme(strip.text = element_text(size = 14))
g1
