
## Resin data


# Package ID: knb-lter-hbr.198.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Resin-available nutrients in the O horizon in the MELNHE study at Hubbard Brook Experimental Forest, Bartlett Experimental Forest and Jeffers Brook, central NH USA, 2011-present.
# Data set creator:  Melany Fisk -  
# Contact:    - Information Manager Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/198/1/f2c4d3b216d5dea191152d4c5844e24d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


res <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Site",     
                 "Age",     
                 "Stand",     
                 "Plot",     
                 "Subplot",     
                 "Treatment",     
                 "Year",     
                 "Date.deployed",     
                 "Days.incubated",     
                 "PO4.hyphen.P",     
                 "NH4.hyphen.N",     
                 "NO3.hyphen.N",     
                 "NH4.plus.NO3"    ), check.names=TRUE)



library(ggplot2)



head(res)

table(res$Year)
library(tidyr)

r17<-res[res$Year==2017,]
head(r17)
table(r17$Stand)
br17<-r17[r17$Stand==c("C7","C8","C9"),]


r17[r17$Stand==c("C7","C8","C9"),10:13]

names(res)
ra<-aggregate(res[,10:13], by=list(Year=res$Year, Stand=res$Stand, Plot=res$Plot, trmt=res$Treatment), FUN="mean", na.rm=T )
ra2<-aggregate(res[,10:13], by=list( Stand=res$Stand, Plot=res$Plot, trmt=res$Treatment), FUN="mean", na.rm=T )
dim(ra)

write.csv(ra, file="avg.resin.csv")


names(ra)

ra$total_N <- ra$NH4.hyphen.N

np.all<-ggplot(ra[ra$Year==2017,], aes(x=trmt, y=log(total_N), colour=trmt))+geom_point()+facet_wrap(~Stand)+theme_bw()+
  geom_smooth(, se=T)+ scale_colour_manual("legend", values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+ scale_y_log10()+
  ggtitle("Resin available Nitrate and Ammonium")+ylab("Resin Available N")

np.all

res<-res[!res$Trt=="Ca" ,]
head(res)


str(res)


res$Trt<-factor(res$Treatment, levels=c("Con","N","P","NP"))
cc <- palette()  # labels the dataframe 'cc' as your palette
palette(c(cc,"purple","brown")) #this adds purple, and brown...  just need purple though

np<-ggplot(res[res$Site=="Bartlett",], aes(x=Year, y=log(total_N), colour=Trt))+geom_point()+facet_wrap(~Stand)+theme_bw()+
  geom_smooth(, se=T)+ scale_colour_manual("legend", values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+ scale_y_log10()+
  ggtitle("Resin available Nitrate and Ammonium")+ylab("Resin Available N")

np

br<-res[res$Site=="Bartlett",]

ggplot(br, aes(x=log(PO4.hyphen.P), y=log(NH4.plus.NO3), colour=Trt, size=2))+geom_point()+theme_classic()+
  scale_color_manual("", values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+ scale_y_log10()+ scale_x_log10() + 
  theme(text=element_text(size=24)) +guides(size=F)+ggtitle("2017 resin available soil N and P") +
facet_wrap(~ Stand)  









nr<-ggplot(br7a, aes(x=Stand, y= log(total_N), fill=Trt ))+geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual("legend", values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+ scale_y_log10()+
  ggtitle("Resin available nitrate and ammonium")+ylab("Resin Available N")+theme_classic()




#write.csv(br7a, file="bart_resin_melnhe_10_30_2019_Young.csv")




#############


head(bres)
bres<-res[res$Site=="Bartlett", ]
bres$N<- bres$NH4.plus.NO3
bres$P<- bres$PO4.hyphen.P


st.err <- function(x) {  sd(x)/sqrt(length(x))}
br<- aggregate(list(N=bres$N, P = bres$P), by=list(Stand=bres$Stand, Treatment=bres$Treatment, Year=bres$Year), FUN="mean",na.rm=T )
br

brSE<-aggregate(list(se.total_N=br7$total_N, se.no3=br7$no3, se.nh4=br7$nh4, se.P=br7$P), by=list(Stand=br7$Stand, Trt=br7$Trt), FUN=st.err  )  
head(br7.err)



ggplot(br, aes(x=Year, y= N, col=Treatment ))+geom_point()+
  scale_color_manual("legend", values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+ scale_y_log10()+
  ggtitle("Resin available nitrate and ammonium")+ylab("Resin Available N")+theme_classic()+
  facet_wrap(~Stand)+geom_line()

ggplot(br, aes(x=Year, y= P, col=Treatment ))+geom_point()+
  scale_color_manual("legend", values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+ scale_y_log10()+
  ggtitle("Resin available nitrate and ammonium")+ylab("Resin Available N")+theme_classic()+
  facet_wrap(~Stand)+geom_line()


