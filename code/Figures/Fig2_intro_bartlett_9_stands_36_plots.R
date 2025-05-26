



## Create figs intro

library(ggrepel)
library(here)

# read in .csv files from R_output

sh <- read.csv(here::here("data_folder","melnhe_input_files","stand_heights.csv"))
sh[sh$Stand=="C1" |sh$Stand=="C2" |sh$Stand=="C3" , "Age" ] <- "Young"
sh[sh$Stand=="C4" |sh$Stand=="C5" |sh$Stand=="C6" , "Age" ] <- "Mid-aged"
sh[sh$Stand=="C7" |sh$Stand=="C8" |sh$Stand=="C9" , "Age" ] <- "Mature"

tree <- read.csv(here::here("data_folder","melnhe_input_files","ten_plus_DBH_2019.csv"))


# handle leading or trailing spaces
tree$Treatment <- trimws(tree$Treatment)

tree$Treatment <- gsub("Con", "Control", tree$Treatment)

# convert cm to basal area, m2 per hectare
tree$basal_area_m2 <- tree$DBH2019^2 * 0.00007854 

library(tidyr)
bap<-aggregate(list( BA_m2 =tree$basal_area_m2), 
               list(Stand=tree$Stand,
                    Plot=tree$Plot,
                    Age = tree$Age), 
               FUN="sum", simplify=T, na.rm=T)
bap$staplo<-paste(bap$Stand, bap$Plot)

bap$ba_m2_ha <- bap$BA_m2 * 11.11111 # conversion factor for 900m2 plot area

bap$Treatment<-sapply(bap$staplo,switch,
                      "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                      "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                      "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                      "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                      "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                      "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                      "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                      "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                      "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N")



# Figure 1, 

sh[sh$Stand=="C1", "Last_year_harv"] <- "1990"
sh[sh$Stand=="C2", "Last_year_harv"] <- "1988"
sh[sh$Stand=="C3", "Last_year_harv"] <- "1985"
sh[sh$Stand=="C4", "Last_year_harv"] <- "1979"
sh[sh$Stand=="C5", "Last_year_harv"] <- "1976"
sh[sh$Stand=="C6", "Last_year_harv"] <- "1975"
sh[sh$Stand=="C7", "Last_year_harv"] <- "1890"
sh[sh$Stand=="C8", "Last_year_harv"] <- "1883"
sh[sh$Stand=="C9", "Last_year_harv"] <- "1890"


heights <- aggregate(list(avg_height_m= sh$height), 
                     by=list(Stand = sh$Stand, 
                             Last_harv = sh$Last_year_harv,
                                       Age = sh$Age,
                            Treatment = sh$Treatment),
          FUN="mean", na.rm=T)


st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

SE_heights <- aggregate(list(se = sh$height), by= list(
  Stand = sh$Stand, 
  Last_harv = sh$Last_year_harv,
  Age = sh$Age,
  Treatment = sh$Treatment),
  FUN= st.err, na.rm=T)


# create staplo object
SE_heights$staplo <- paste(SE_heights$Stand, SE_heights$Treatment)
heights$staplo <- paste(heights$Stand, heights$Treatment)
diam$staplo <- paste(diam$Stand, diam$Treatment)


heights$years_since <- 2019 - as.numeric(heights$Last_harv)


# bring in BA from dbh df
bap$statr <- paste(bap$Stand, bap$Treatment)


heights$ba_m2_ha <- bap$ba_m2_ha[match(heights$staplo, bap$statr)]

heights[heights$Treatment=="NP","Treatment"] <- "N+P"

heights$Treatment <- factor(heights$Treatment, levels=c("Control","N","P","N+P"))
heights$Age <-factor(heights$Age, levels=c("Mature","Mid-aged","Young"))

heights$se <- SE_heights$se[match(heights$staplo, SE_heights$staplo)]

heights$Age <- factor(heights$Age, levels=c("Young","Mid-aged","Mature"))
g1 <- ggplot(heights, aes(x= Stand , y= ba_m2_ha,fill=Treatment,
                    label=Stand))+
  scale_fill_manual(values=c("black","blue","red","purple"))+
#  scale_color_manual(values=c("#E6AB02","#666666","#D95F02"))+  
 # scale_shape_manual(values=c(22, 24, 23))+
#  geom_text_repel()+
  geom_col(position=position_dodge(), col="black")+
#  geom_point(  shape=21, size=3,stroke=3 )+
  theme_bw()+theme(panel.grid.major=element_blank(), panel.grid.minor = element_blank())+
  labs(x="Stand", 
       y="Basal area")+
  facet_wrap(~Age, nrow=1, scales="free_x")

g1

###################################################


#  Community ordination
names(dbh)

tree$staplo <- paste(tree$Stand, tree$Plot)


tree$Age <- heights$Age[match(tree$Stand, heights$Stand)]

co<-aggregate(list(basal_area = tree$basal_area_m2), list(staplo =tree$staplo,Stand=tree$Stand,
                                      Treatment = tree$Treatment, Age=tree$Age, Species=tree$Species),
              FUN="sum", na.rm=T,
              simplify=T)
names(co)

com<-spread(co[ , c("staplo","Stand","Treatment","Species","basal_area","Age")], "Species","basal_area")
com <- com[!com$Treatment=="Ca",]
dim(com)

com[is.na(com)]=0
com<-as.data.frame(com)


# Ordinate the tree comunity matrix.

# Using metaMDS() command in 'vegan'. 'sites' will store the plot.ID values
names(com)
use<-com[,-c(1:4)]
dim(use)
names(use)

library(vegan)
# Ordinate
m1 <- metaMDS(use, distance = "bray", k = 2, trymax = 200,
              autotransform =FALSE,  
              wascores = TRUE, expand = TRUE,
              trace = 1, plot = TRUE,)

#sites<-rownames(use)


points<-as.data.frame(m1$points)
points$Age<-com$Age
points$Stand<-com$Stand
points$Treatment <- com$Treatment

points$Treatment<- factor(points$Treatment, levels=c("Control","N","P","NP"))

library(ggforce)
g2 <- ggplot(points, aes(x=MDS1, y=MDS2))+
#  geom_mark_ellipse(aes( group=Stand, label = Stand)) +
  geom_mark_ellipse(aes( group=Age, fill = Age, label=Age, linewidth=3)) +
  geom_text(aes(label=Stand, col=Treatment), size=4)+
  labs(x="NMDS 1", y="NMDS 2")+
  scale_fill_manual(values=c("#E6AB02","#666666","#D95F02"))+ 
  scale_color_manual(values=c("black","blue","red","purple"))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(linewidth=F)+xlim(-1.5, 1.5)+ylim(-1.2, 1.2)

g2


library(ggpubr)

ggarrange(g1, g2,  nrow=2)



#########

res <- read.csv(here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))

res$staplo <- paste(res$Stand, res$Plot)

res$Treatment<-sapply(res$staplo,switch,
                      "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                      "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                      "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                      "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                      "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                      "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                      "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                      "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                      "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N")

res[res$Treatment=="NP","Treatment"] <- "N+P"

res$Treatment <- factor(res$Treatment, levels=c("Control","N","P","N+P"))

res[res$Stand=="C1" |res$Stand=="C2" |res$Stand=="C3" , "Age" ] <- "Young"
res[res$Stand=="C4" |res$Stand=="C5" |res$Stand=="C6" , "Age" ] <- "Mid-aged"
res[res$Stand=="C7" |res$Stand=="C8" |res$Stand=="C9" , "Age" ] <- "Mature"


ggplot( res, aes(x=PO4.hyphen.P, y=NH4.hyphen.N, col=Age, fill=Treatment))+
  geom_point(size=4, shape=21, stroke=2)+
  scale_color_manual(values=c("#E6AB02","#666666","#D95F02"))+ 
  scale_fill_manual("", 
    values = c("Control" = "black", "N" = "blue", "P" = "red", "N+P" = "purple"))+
  scale_y_log10()+scale_x_log10()
  
