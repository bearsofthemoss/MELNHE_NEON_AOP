
##  Retrieve MELNHE data and place files in MELNHE_input folder in the data folder.


## Outputs are: ####
#-  resin available N and P
#-  Stand height information
#-  Basal area information


## Resin available N and P data ####


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
library(tidyr)

# examine resin-available data object
head(res)
table(res$Year)

# take out ca?
res<-res[!res$Treatment=="Ca" ,]

# only Bartlett
res <- res[res$Site=="Bartlett",]

# only use year 17 data
res<-res[res$Year=="2017" ,]







ggplot(res, aes(x=log(PO4.hyphen.P), y=log(NH4.plus.NO3), colour=Treatment, size=.5))+geom_point()+theme_classic()+
  scale_color_manual("", values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+ 
  scale_y_log10()+ scale_x_log10() + 
  theme(text=element_text(size=14)) +guides(size=F)+ggtitle("2017 resin available soil N and P") 




res$Trt<-factor(res$Treatment, levels=c("Con","N","P","NP"))

np<-ggplot(res, aes(x=Trt, y=log(NH4.hyphen.N), colour=Trt))+geom_point()+facet_wrap(~Stand)+theme_bw()+
  geom_smooth(, se=T)+ scale_colour_manual("legend", values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+ scale_y_log10()+
  ggtitle("Resin available Nitrate and Ammonium")+ylab("Resin Available N")

np



## Calculate plot level value average from multiple subplots
ra<-aggregate(res[,10:13], 
              by=list(Year=res$Year, 
                      Stand=res$Stand, 
                      Plot=res$Plot, 
                      trmt=res$Treatment),
              FUN="mean", na.rm=T )

names(ra)



ggplot(ra, aes(x=Stand, y=log(NH4.hyphen.N), colour=trmt))+geom_point()+
  #facet_wrap(~Stand)+theme_bw()+
  geom_smooth(, se=T)+ 
  scale_colour_manual("", 
                      values = c("Con" = "black", "N" = "blue", "P" = "red", "NP" = "purple"))+
  scale_y_log10()+
  ggtitle("Resin available Nitrate and Ammonium")+ylab("Resin Available N")


dim(ra)
head(ra)

### Write resin N and P data ####
write.csv(ra, file=here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))




#  Read in Tree DBH information ####

# Package ID: knb-lter-hbr.183.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Tree DBH response to nitrogen and phosphorus fertilization in the MELNHE study, Hubbard Brook Experimental Forest, Bartlett Experimental Forest, and Jeffers Brook.
# Data set creator:  Melany Fisk - Miami University 
# Data set creator:  Ruth Yanai - SUNY-ESF 
# Data set creator:  Timothy Fahey - Cornell University 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/183/1/f1feffe8e287c662a1a62c6cfe494200" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Stand",     
                 "Plot",     
                 "Treatment",     
                 "Subplot",     
                 "Species",     
                 "PreviousTagNumber",     
                 "CurrentTagNumber",     
                 "DBH2008",     
                 "DBH2009",     
                 "DBH2010",     
                 "DBH2011",     
                 "DBH2015",     
                 "DBH2019",     
                 "dead2008.hyphen.2010",     
                 "dead2011",     
                 "dead2015",     
                 "dead2019"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Stand)!="factor") dt1$Stand<- as.factor(dt1$Stand)
if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Treatment)!="factor") dt1$Treatment<- as.factor(dt1$Treatment)
if (class(dt1$Subplot)!="factor") dt1$Subplot<- as.factor(dt1$Subplot)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$PreviousTagNumber)!="factor") dt1$PreviousTagNumber<- as.factor(dt1$PreviousTagNumber)
if (class(dt1$CurrentTagNumber)!="factor") dt1$CurrentTagNumber<- as.factor(dt1$CurrentTagNumber)
if (class(dt1$DBH2008)=="factor") dt1$DBH2008 <-as.numeric(levels(dt1$DBH2008))[as.integer(dt1$DBH2008) ]               
if (class(dt1$DBH2008)=="character") dt1$DBH2008 <-as.numeric(dt1$DBH2008)
if (class(dt1$DBH2009)=="factor") dt1$DBH2009 <-as.numeric(levels(dt1$DBH2009))[as.integer(dt1$DBH2009) ]               
if (class(dt1$DBH2009)=="character") dt1$DBH2009 <-as.numeric(dt1$DBH2009)
if (class(dt1$DBH2010)=="factor") dt1$DBH2010 <-as.numeric(levels(dt1$DBH2010))[as.integer(dt1$DBH2010) ]               
if (class(dt1$DBH2010)=="character") dt1$DBH2010 <-as.numeric(dt1$DBH2010)
if (class(dt1$DBH2011)=="factor") dt1$DBH2011 <-as.numeric(levels(dt1$DBH2011))[as.integer(dt1$DBH2011) ]               
if (class(dt1$DBH2011)=="character") dt1$DBH2011 <-as.numeric(dt1$DBH2011)
if (class(dt1$DBH2015)=="factor") dt1$DBH2015 <-as.numeric(levels(dt1$DBH2015))[as.integer(dt1$DBH2015) ]               
if (class(dt1$DBH2015)=="character") dt1$DBH2015 <-as.numeric(dt1$DBH2015)
if (class(dt1$DBH2019)=="factor") dt1$DBH2019 <-as.numeric(levels(dt1$DBH2019))[as.integer(dt1$DBH2019) ]               
if (class(dt1$DBH2019)=="character") dt1$DBH2019 <-as.numeric(dt1$DBH2019)
if (class(dt1$dead2008.hyphen.2010)!="factor") dt1$dead2008.hyphen.2010<- as.factor(dt1$dead2008.hyphen.2010)
if (class(dt1$dead2011)!="factor") dt1$dead2011<- as.factor(dt1$dead2011)
if (class(dt1$dead2015)!="factor") dt1$dead2015<- as.factor(dt1$dead2015)
if (class(dt1$dead2019)!="factor") dt1$dead2019<- as.factor(dt1$dead2019)

# Convert Missing Values to NA for non-dates

dt1$PreviousTagNumber <- as.factor(ifelse((trimws(as.character(dt1$PreviousTagNumber))==trimws("NA")),NA,as.character(dt1$PreviousTagNumber)))
dt1$CurrentTagNumber <- as.factor(ifelse((trimws(as.character(dt1$CurrentTagNumber))==trimws("NA")),NA,as.character(dt1$CurrentTagNumber)))
dt1$DBH2008 <- ifelse((trimws(as.character(dt1$DBH2008))==trimws("NA")),NA,dt1$DBH2008)               
suppressWarnings(dt1$DBH2008 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DBH2008))==as.character(as.numeric("NA"))),NA,dt1$DBH2008))
dt1$DBH2009 <- ifelse((trimws(as.character(dt1$DBH2009))==trimws("NA")),NA,dt1$DBH2009)               
suppressWarnings(dt1$DBH2009 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DBH2009))==as.character(as.numeric("NA"))),NA,dt1$DBH2009))
dt1$DBH2010 <- ifelse((trimws(as.character(dt1$DBH2010))==trimws("NA")),NA,dt1$DBH2010)               
suppressWarnings(dt1$DBH2010 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DBH2010))==as.character(as.numeric("NA"))),NA,dt1$DBH2010))
dt1$DBH2011 <- ifelse((trimws(as.character(dt1$DBH2011))==trimws("NA")),NA,dt1$DBH2011)               
suppressWarnings(dt1$DBH2011 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DBH2011))==as.character(as.numeric("NA"))),NA,dt1$DBH2011))
dt1$DBH2015 <- ifelse((trimws(as.character(dt1$DBH2015))==trimws("NA")),NA,dt1$DBH2015)               
suppressWarnings(dt1$DBH2015 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DBH2015))==as.character(as.numeric("NA"))),NA,dt1$DBH2015))
dt1$DBH2019 <- ifelse((trimws(as.character(dt1$DBH2019))==trimws("NA")),NA,dt1$DBH2019)               
suppressWarnings(dt1$DBH2019 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DBH2019))==as.character(as.numeric("NA"))),NA,dt1$DBH2019))


str(dt1)
## 

ten <- dt1 
names(ten)
tn <- ten[ , c("Stand","Plot","Treatment","Subplot","Species","CurrentTagNumber","DBH2019","dead2019")]

tn$DBH2019 <- as.numeric(tn$DBH2019)
tn$Age[tn$Stand=="C1"]<-"~30 years old"
tn$Age[tn$Stand=="C2"]<-"~30 years old"
tn$Age[tn$Stand=="C3"]<-"~30 years old"
tn$Age[tn$Stand=="C4"]<-"~60 years old"
tn$Age[tn$Stand=="C5"]<-"~60 years old"
tn$Age[tn$Stand=="C6"]<-"~60 years old" 
tn$Age[tn$Stand=="C7"]<-"~100 years old"
tn$Age[tn$Stand=="C8"]<-"~100 years old"
tn$Age[tn$Stand=="C9"]<-"~100 years old"

tn <- tn[!is.na(tn$Age),]

tn <- tn[tn$Treatment!="Ca",]

tn$BA <- (tn$DBH2019/2)^2 * 3.14159 / 10000

head(tn)
tn[tn$DBH2019==20.00,]

### Write tree dbh data 2-10  ####
write.csv(tn, file=here::here("data_folder","melnhe_input_files","ten_plus_DBH_2019.csv"))




