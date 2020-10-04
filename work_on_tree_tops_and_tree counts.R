

#3 here is where I'd like to 

dada$Age[dada$Stand=="C1"]<-"~30 years old"
dada$Age[dada$Stand=="C2"]<-"~30 years old"
dada$Age[dada$Stand=="C3"]<-"~30 years old"
dada$Age[dada$Stand=="C4"]<-"~60 years old"
dada$Age[dada$Stand=="C5"]<-"~60 years old"
dada$Age[dada$Stand=="C6"]<-"~60 years old" 
dada$Age[dada$Stand=="C7"]<-"~100 years old"
dada$Age[dada$Stand=="C8"]<-"~100 years old"
dada$Age[dada$Stand=="C9"]<-"~100 years old"


head(dada)
library(ggplot2)
ggplot(dada, aes(x=Treatment, y=winRadius, col=Stand))+geom_jitter(width=0.2)+facet_wrap(~Age)+
  ggtitle("Average Crown Width for Bartlett trees")
