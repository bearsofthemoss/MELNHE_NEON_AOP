



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



# first dataset above

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/183/1/732e21245d3390747e553abbc2392343" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Stand",     
                 "Plot",     
                 "Treatment",     
                 "Subplot",     
                 "Species",     
                 "Year",     
                 "DBH",     
                 "DeadOrFallen",     
                 "Notes"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Stand)!="factor") dt2$Stand<- as.factor(dt2$Stand)
if (class(dt2$Plot)!="factor") dt2$Plot<- as.factor(dt2$Plot)
if (class(dt2$Treatment)!="factor") dt2$Treatment<- as.factor(dt2$Treatment)
if (class(dt2$Subplot)!="factor") dt2$Subplot<- as.factor(dt2$Subplot)
if (class(dt2$Species)!="factor") dt2$Species<- as.factor(dt2$Species)
if (class(dt2$DBH)=="factor") dt2$DBH <-as.numeric(levels(dt2$DBH))[as.integer(dt2$DBH) ]               
if (class(dt2$DBH)=="character") dt2$DBH <-as.numeric(dt2$DBH)
if (class(dt2$DeadOrFallen)!="factor") dt2$DeadOrFallen<- as.factor(dt2$DeadOrFallen)
if (class(dt2$Notes)!="factor") dt2$Notes<- as.factor(dt2$Notes)

# Convert Missing Values to NA for non-dates

dt2$DBH <- ifelse((trimws(as.character(dt2$DBH))==trimws("NA")),NA,dt2$DBH)               
suppressWarnings(dt2$DBH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DBH))==as.character(as.numeric("NA"))),NA,dt2$DBH))


# Here is the structure of the input data frame:
str(dt2)                            

###


###  Work with the 10+ and 2-10


##
head(dt1)

head(dt2)

## 
ten <- dt1
two <- dt2


names(two)
table(two$Treatment, two$Year)


ggplot(two, aes(x=Year, y=DBH, col=Species))+geom_point()+
  geom_smooth(method="lm")

head(ten)

tg <- gather(ten, "Year","DBH", 8:13)

ggplot(tg, aes(x=Year, y=DBH, col=Species))+geom_point()+
  geom_smooth(method="lm")

ten_co <-aggregate( tg$DBH, by=list(Stand = tg$Stand,Plot=tg$Plot, Treatment = tg$Treatment, Species = tg$Species, Year = tg$Year), FUN="sum", na.rm=T)

ggplot(ten_co, aes(x=Year, y=x, col=Species))+facet_grid(Treatment~Stand)+geom_col(position="stack")

two_co <-aggregate( two$DBH, by=list(Stand = two$Stand,Plot=two$Plot, Treatment = two$Treatment, Species = two$Species, Year = two$Year), FUN="sum", na.rm=T)

ggplot(two_co, aes(x=Year, y=x, col=Species))+facet_grid(Treatment~Stand)+geom_col(position="stack")



## subest to stand levels ni Bart


## Only use 2019 values.



ten_co
