
### Alex Young    9-8-2020
setwd("C:\\Users\\aryoung\\Desktop\\mel_NEON\\data_files\\new_stand_tt_csv_spec")


#### Read in the .csv files

C1<-read.csv("ttop_C1_6_25_2020.csv")
C2<-read.csv("ttop_C2_6_25_2020.csv")
C3<-read.csv("ttop_C3_6_25_2020.csv")
C4<-read.csv("ttop_C4_6_25_2020.csv")
C5<-read.csv("ttop_C5_6_25_2020.csv")
C6<-read.csv("ttop_C6_6_25_2020.csv")
C7<-read.csv("ttop_C7_6_25_2020.csv")
C8<-read.csv("ttop_C8_6_25_2020.csv")
C9<-read.csv("ttop_C9_6_25_2020.csv")


C1$Stand<-"C1"
C2$Stand<-"C2"
C3$Stand<-"C3"
C4$Stand<-"C4"
C5$Stand<-"C5"
C6$Stand<-"C6"
C7$Stand<-"C7"
C8$Stand<-"C8"
C9$Stand<-"C9"

str(C9)
C1$refl<-as.numeric(C1$refl)
C2$refl<-as.numeric(C2$refl)
C3$refl<-as.numeric(C3$refl)
C4$refl<-as.numeric(C4$refl)
C5$refl<-as.numeric(C5$refl)
C6$refl<-as.numeric(C6$refl)
C7$refl<-as.numeric(C7$refl)
C8$refl<-as.numeric(C8$refl)
C9$refl<-as.numeric(C9$refl)

library(tidyr)
library(reshape2)


##########################################################
C1_c<-C1[C1$Treatment=="Control",]
C1_n<-C1[C1$Treatment=="N",]
C1_p<-C1[C1$Treatment=="P",]
C1_np<-C1[C1$Treatment=="NP",]

#
C1_c$group=rep(c(1:(length(C1_c$refl)/345)), times=345)
C1_n$group=rep(c(1:(length(C1_n$refl)/345)), times=345)
C1_p$group=rep(c(1:(length(C1_p$refl)/345)), times=345)
C1_np$group=rep(c(1:(length(C1_np$refl)/345)), times=345)
#
C1_c<-na.omit(C1_c)
C1_n<-na.omit(C1_n)
C1_p<-na.omit(C1_p)
C1_np<-na.omit(C1_np)

head(C1_c)
sC1_c<-spread(C1_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC1_n<-spread(C1_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC1_p<-spread(C1_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC1_np<-spread(C1_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC1<-rbind(sC1_c, sC1_n, sC1_p, sC1_np)

str(sC1)

##########################################################
head(C2_c)

C2_c<-C2[C2$Treatment=="Control",]
C2_n<-C2[C2$Treatment=="N",]
C2_p<-C2[C2$Treatment=="P",]
C2_np<-C2[C2$Treatment=="NP",]
#
C2_c$group=rep(c(1:(length(C2_c$refl)/345)), times=345)
C2_n$group=rep(c(1:(length(C2_n$refl)/345)), times=345)
C2_p$group=rep(c(1:(length(C2_p$refl)/345)), times=345)
C2_np$group=rep(c(1:(length(C2_np$refl)/345)), times=345)
#
sC2_c<-spread(C2_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC2_n<-spread(C2_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC2_p<-spread(C2_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC2_np<-spread(C2_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC2<-rbind(sC2_c, sC2_n, sC2_p, sC2_np)

head(C2_c)
##########################################################
C3_c<-C3[C3$Treatment=="Control",]
C3_n<-C3[C3$Treatment=="N",]
C3_p<-C3[C3$Treatment=="P",]
C3_np<-C3[C3$Treatment=="NP",]


head(C3_c)
#
C3_c$group=rep(c(1:(length(C3_c$refl)/345)), times=345)
C3_n$group=rep(c(1:(length(C3_n$refl)/345)), times=345)
C3_p$group=rep(c(1:(length(C3_p$refl)/345)), times=345)
C3_np$group=rep(c(1:(length(C3_np$refl)/345)), times=345)
#
sC3_c<-spread(C3_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC3_n<-spread(C3_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC3_p<-spread(C3_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC3_np<-spread(C3_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC3<-rbind(sC3_c, sC3_n, sC3_p, sC3_np)
##########################################################
C4_c<-C4[C4$Treatment=="Control",]
C4_n<-C4[C4$Treatment=="N",]
C4_p<-C4[C4$Treatment=="P",]
C4_np<-C4[C4$Treatment=="NP",]
#
C4_c$group=rep(c(1:(length(C4_c$refl)/345)), times=345)
C4_n$group=rep(c(1:(length(C4_n$refl)/345)), times=345)
C4_p$group=rep(c(1:(length(C4_p$refl)/345)), times=345)
C4_np$group=rep(c(1:(length(C4_np$refl)/345)), times=345)
#
sC4_c<-spread(C4_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC4_n<-spread(C4_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC4_p<-spread(C4_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC4_np<-spread(C4_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC4<-rbind(sC4_c, sC4_n, sC4_p, sC4_np)
##########################################################
C5_c<-C5[C5$Treatment=="Control",]
C5_n<-C5[C5$Treatment=="N",]
C5_p<-C5[C5$Treatment=="P",]
C5_np<-C5[C5$Treatment=="NP",]
#
C5_c$group=rep(c(1:(length(C5_c$refl)/345)), times=345)
C5_n$group=rep(c(1:(length(C5_n$refl)/345)), times=345)
C5_p$group=rep(c(1:(length(C5_p$refl)/345)), times=345)
C5_np$group=rep(c(1:(length(C5_np$refl)/345)), times=345)
#
sC5_c<-spread(C5_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC5_n<-spread(C5_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC5_p<-spread(C5_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC5_np<-spread(C5_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC5<-rbind(sC5_c, sC5_n, sC5_p, sC5_np)
##########################################################
C6_c<-C6[C6$Treatment=="Control",]
C6_n<-C6[C6$Treatment=="N",]
C6_p<-C6[C6$Treatment=="P",]
C6_np<-C6[C6$Treatment=="NP",]
#
C6_c$group=rep(c(1:(length(C6_c$refl)/345)), times=345)
C6_n$group=rep(c(1:(length(C6_n$refl)/345)), times=345)
C6_p$group=rep(c(1:(length(C6_p$refl)/345)), times=345)
C6_np$group=rep(c(1:(length(C6_np$refl)/345)), times=345)
#
sC6_c<-spread(C6_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC6_n<-spread(C6_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC6_p<-spread(C6_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC6_np<-spread(C6_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC6<-rbind(sC6_c, sC6_n, sC6_p, sC6_np)
##########################################################
C7_c<-C7[C7$Treatment=="Control",]
C7_n<-C7[C7$Treatment=="N",]
C7_p<-C7[C7$Treatment=="P",]
C7_np<-C7[C7$Treatment=="NP",]
#
C7_c$group=rep(c(1:(length(C7_c$refl)/345)), times=345)
C7_n$group=rep(c(1:(length(C7_n$refl)/345)), times=345)
C7_p$group=rep(c(1:(length(C7_p$refl)/345)), times=345)
C7_np$group=rep(c(1:(length(C7_np$refl)/345)), times=345)
#
sC7_c<-spread(C7_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC7_n<-spread(C7_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC7_p<-spread(C7_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC7_np<-spread(C7_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC7<-rbind(sC7_c, sC7_n, sC7_p, sC7_np)

##########################################################
C8_c<-C8[C8$Treatment=="Control",]
C8_n<-C8[C8$Treatment=="N",]
C8_p<-C8[C8$Treatment=="P",]
C8_np<-C8[C8$Treatment=="NP",]
#
C8_c$group=rep(c(1:(length(C8_c$refl)/345)), times=345)
C8_n$group=rep(c(1:(length(C8_n$refl)/345)), times=345)
C8_p$group=rep(c(1:(length(C8_p$refl)/345)), times=345)
C8_np$group=rep(c(1:(length(C8_np$refl)/345)), times=345)
#
sC8_c<-spread(C8_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC8_n<-spread(C8_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC8_p<-spread(C8_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC8_np<-spread(C8_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC8<-rbind(sC8_c, sC8_n, sC8_p, sC8_np)
##########################################################
C9_c<-C9[C9$Treatment=="Control",]
C9_n<-C9[C9$Treatment=="N",]
C9_p<-C9[C9$Treatment=="P",]
C9_np<-C9[C9$Treatment=="NP",]
#
C9_c$group=rep(c(1:(length(C9_c$refl)/345)), times=345)
C9_n$group=rep(c(1:(length(C9_n$refl)/345)), times=345)
C9_p$group=rep(c(1:(length(C9_p$refl)/345)), times=345)
C9_np$group=rep(c(1:(length(C9_np$refl)/345)), times=345)
#
sC9_c<-spread(C9_c[ ,c(3,5,6,7,8)], "wavelength","refl")
sC9_n<-spread(C9_n[ ,c(3,5,6,7,8)], "wavelength","refl")
sC9_p<-spread(C9_p[ ,c(3,5,6,7,8)], "wavelength","refl")
sC9_np<-spread(C9_np[ ,c(3,5,6,7,8)], "wavelength","refl")
sC9<-rbind(sC9_c, sC9_n, sC9_p, sC9_np)
##################################################################################
#write.csv(sC1, file="spread_spec_C1.csv")
#write.csv(sC2, file="spread_spec_C2.csv")
#write.csv(sC3, file="spread_spec_C3.csv")
#write.csv(sC4, file="spread_spec_C4.csv")
#write.csv(sC5, file="spread_spec_C5.csv")
#write.csv(sC6, file="spread_spec_C6.csv")
#write.csv(sC7, file="spread_spec_C7.csv")
#write.csv(sC8, file="spread_spec_C8.csv")
#write.csv(sC9, file="spread_spec_C9.csv")
############################################################################################

dim(sC1)
dim(sC2)
#com<-rbind(sC1, sC2, sC3, sC4, sC5, sC7, sC8, sC9)
com<-rbind(sC1,sC2,sC3, sC4,sC5,sC6, sC7,sC8, sC9)



library(tidyr)
names(com)
cg<-gather(com,"wvl","refl", 4:348)
head(cg)

plot(cg$wvl, cg$refl)

#  I think this was already handled.  The wavelength numbers might be off due to the removing of water absorption bands.
#cg$wvl<-as.numeric(cg$wvl)
#cg[which(cg$wvl >=1340),4] <- cg[which(cg$wvl >=1340),4]+100
#table(cg$wvl)
#cg[which(cg$wvl >1780),4] <- cg[which(cg$wvl >1780),4] +160


head(cg)
plot(cg$wvl, cg$refl, main="new spectra")


table(cg$Stand, is.na(cg$refl))
write.csv(cg, file="actual_tops7_7a_2020.csv")
