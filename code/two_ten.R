

#  Creates a .csv file of tree dbh and last year harvested


## Read in Tree DBH information

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



## 

ten <- dt1 
names(ten)
tn <- ten[ , c("Stand","Plot","Treatment","Subplot","Species","CurrentTagNumber","DBH2019","dead2019")]



write.csv(tn, file.path("R_output","ten_plus_DBH_2019.csv"))



# library(tidyr)
# library(ggplot2)
# tg <- gather(ten, "Year","DBH", 8:13)
# 
# plsp <- aggregate(tg$DBH, by=list(
#   Year = tg$Year,
#   dead = tg$dead2019,
#   Species = tg$Species,
#   Stand = tg$Stand,
#   Treatment = tg$Treatment
# ),
# FUN="sum", na.rm=T)
# 
# head(plsp)
# str(plsp)
# 
# ggplot(plsp, aes(x=Year, y=x, col=dead, fill=Species))+
#   geom_bar(stat="identity", position="stack")+facet_wrap(~Stand)






