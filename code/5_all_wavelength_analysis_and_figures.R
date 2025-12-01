## Alex Young 10/21/2019

## Test all bands for treatment effects and for differences in age
## MELNHE stands in Bartlett, NH-  NEON AOP reflectance.
library(ggplot2)
library(lmerTest)
library(lme4)
library(tidyr)
library(dplyr)

## read in data, add 'ages', add 'YesN','NoN' for N*P ANOVA
dada<- read.csv(here::here( "data_folder","processed_spectra.csv"))

dada<-dada[,-1]

age_class <- c("Young forest","Mid-aged forest","Mature forest")



# stand ages
dada$Age[dada$Stand=="C1"]<-"Young forest"
dada$Age[dada$Stand=="C2"]<-"Young forest"
dada$Age[dada$Stand=="C3"]<-"Young forest"
dada$Age[dada$Stand=="C4"]<-"Mid-aged forest"
dada$Age[dada$Stand=="C5"]<-"Mid-aged forest"
dada$Age[dada$Stand=="C6"]<-"Mid-aged forest" 
dada$Age[dada$Stand=="C7"]<-"Mature forest"
dada$Age[dada$Stand=="C8"]<-"Mature forest"
dada$Age[dada$Stand=="C9"]<-"Mature forest"

names(dada)
# make a 'long' version of dada
ldada<-gather(dada, "wvl","refl",7:351)
ldada$wvl<-as.numeric(gsub(".*_","",ldada$wvl))
ldada<-na.omit(ldada) # take out NA values- about half were NA 10_3 Ary
ldada$staplo<-paste(ldada$Stand, ldada$Treatment)

# min,max, and mean number of tree tops by plot.  6 is probably too low right?
min(table(ldada$staplo))/345
max(table(ldada$staplo))/345
mean(table(ldada$staplo))/345


## Univariate analysis
# for N*P Anova
ldada$Treatment<-factor(ldada$Treatment, levels=c("Control","N","P","NP"))
ldada$Ntrmt <- factor(  ifelse(ldada$Treatment == "N" | ldada$Treatment == "NP", "N", "NoN"))
ldada$Ptrmt <- factor(  ifelse(ldada$Treatment %in% c("P", "NP"), "P", "NoP"))

##########




## calculate plot-level PRI avg
gat<-spread(ldada, "wvl","refl")
gat$pri<-(gat$`528.99`- gat$`549.02`)/(gat$`528.99` +gat$`549.02`)
names(gat)
dim(gat)

avg_pri <-aggregate(list(height=gat$height,
                            pri = gat$pri), 
                       by=list(Stand=gat$Stand,
                               Age=gat$Age,
                               Treatment = gat$Treatment,
                               Ntrmt =gat$Ntrmt,
                               Ptrmt = gat$Ptrmt,
                               staplo = gat$staplo), 
                       FUN="mean", na.rm=T)


avg_pri$Age <- factor(avg_pri$Age, levels=c("Young forest",
                                            "Mid-aged forest",
                                            "Mature forest"))

st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))}



all_se <- aggregate( list(se_pri = gat$pri),
                     by=list(age = gat$Age,
                             Stand = gat$Stand,
                             Treatment = gat$Treatment),
                     FUN= st.err, na.rm=T)


avg_pri$statr <- paste(avg_pri$Stand, avg_pri$Treatment)
all_se$statr <- paste(all_se$Stand, all_se$Treatment)

avg_pri$se <- all_se$se_pri[match(avg_pri$statr, all_se$statr)]


avg_pri$Treatment <- factor(avg_pri$Treatment , levels = c("Control","N","P","NP"))

avg_pri$Age <- factor(avg_pri$Age, levels=c("Young forest","Mid-aged forest","Mature forest"))

pos_dodge_width <- .8

ggplot(avg_pri, aes(x=Treatment, y=pri, fill= Treatment, group=Stand))+ 
  geom_errorbar(aes(ymin = pri - se, ymax = pri+se),
                position = position_dodge(pos_dodge_width),
                width = .3,
                col = "black")+
  geom_point(position = position_dodge(pos_dodge_width),
             col="black", stroke = 1,
             size = 3,
             shape = 21)+
  facet_wrap(~Age, scales= "free_x", nrow=1)+
  scale_fill_manual(values=c("black","blue","red","purple"))+
  theme_bw()+theme(panel.grid = element_blank())+
  labs( x = "Nutrient treatment", y = "Photochemical Reflectance Index")




########



chem <-  read.csv(here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))
chem[chem$trmt=="Con", "trmt"] <- "Control"
chem$treat_stand<-paste(chem$Stand, chem$trmt)

head(chem)
table(chem$Year)

avg_pri$N <- chem$NH4.plus.NO3[match(avg_pri$statr, chem$treat_stand )]


anova(lm( pri ~ Ntrmt * Ptrmt * N + Age +Stand, data = avg_pri))

library(ggplot2)
ggplot(avg_pri, aes(x= N, y = pri, col= Treatment, shape=Age))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se=F, aes(group = Treatment))+
  scale_color_manual(values = c("black","blue","red","purple"))+
  labs(x="Soil N", y="PRI")

tt <- HSD.test(lm( pri ~ Treatment + N + Age +Stand, data = avg_pri), "Age") 
tt





###########################################################

out_sel <- list()
out_res <- list()

for(i in 1:3){
  sel <- avg_pri %>% filter(Age== age_class[i] )
  
  seli <- sel %>% group_by(Stand,Ntrmt, Ptrmt,Treatment, N) %>%
    summarize_at(c("pri"), .funs = mean)
  
  
  modi <-lm(pri~Treatment+N+Stand,seli) 
  
  tt <- HSD.test(modi, "Treatment") 
  tt ###
  
  results <- as.data.frame( anova(modi))
  results$age <- age_class[i]
  
  sel$group <- tt$groups$groups[match(sel$Treatment, rownames(tt$groups))]
  
  # Calculate group means and labels for positioning
  group_labels <- sel %>%
    group_by(Treatment) %>%
    summarise(
      max_y = max(pri, na.rm = TRUE),
      group = unique(group)
    )
  
  sel$Treatment <- factor(sel$Treatment, levels=c("Control","N","P","NP"))
  
  sel$group <- group_labels$group[match(sel$Treatment, group_labels$Treatment)]
  sel$max_y <- group_labels$max_y[match(sel$Treatment, group_labels$Treatment)]
  
  sel$age <- age_class[i]
  
  out_sel <- rbind(sel, out_sel)
  out_res <- rbind(results, out_res)
}


out_res

tt$groups

tt$groups$avg <- mean(tt$groups$pri)
tt$groups$diff <- (tt$groups$pri - tt$groups$avg) * 100
