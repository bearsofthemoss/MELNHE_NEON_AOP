##############
library(ggplot2)
library(tidyverse)
library(agricolae)

vals <- read.csv("./PRI_data_Bartlett_NP.csv")

## add age info
dat <- as.data.frame(matrix(data = c(rep("Young forest",3), rep("Mid-aged forest",3), rep("Mature forest",3),
                                     "C1", "C2","C3","C4","C5","C6","C7","C8","C9"), ncol = 2))

colnames(dat) <- c("age", "Stand")
vals <- vals %>% merge(dat,"Stand")


vals$statr <- paste(vals$Stand, vals$Treatment)

age_class <- c("Young forest","Mid-aged forest","Mature forest")


## overall
all <- vals %>% group_by(Stand,Treatment, age) %>%
  summarize_at(c("pri"), .funs = mean)



all <- aggregate( list(pri = vals$pri),
           by=list(age = vals$age,
                   Stand = vals$Stand,
                   Ntrmt = vals$Ntrmt,
                   Ptrmt = vals$Ptrmt,
                   Treatment = vals$Treatment),
           FUN="mean", na.rm=T)


st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))}



all_se <- aggregate( list(se_pri = vals$pri),
                     by=list(age = vals$age,
                             Stand = vals$Stand,
                             Treatment = vals$Treatment),
                     FUN= st.err, na.rm=T)

all$statr <- paste(all$Stand, all$Treatment)
all_se$statr <- paste(all_se$Stand, all_se$Treatment)

all$se <- all_se$se_pri[match(all$statr, all_se$statr)]


all$Treatment <- factor(all$Treatment , levels = c("Control","N","P","NP"))

all$age <- factor(all$age, levels=c("Young forest","Mid-aged forest","Mature forest"))

pos_dodge_width <- .8

ggplot(all, aes(x=Treatment, y=pri, fill= Treatment, group=Stand))+ 
  geom_errorbar(aes(ymin = pri - se, ymax = pri+se),
                position = position_dodge(pos_dodge_width),
                width = .3,
                col = "black")+
  geom_point(position = position_dodge(pos_dodge_width),
             col="black", stroke = 1,
             size = 3,
             shape = 21)+
  facet_wrap(~age, scales= "free_x", nrow=1)+
  scale_fill_manual(values=c("black","blue","red","purple"))+
  theme_bw()+theme(panel.grid = element_blank())+
  labs( x = "Nutrient treatment", y = "Photochemical Reflectance Index")

##########

chem <-  read.csv(here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))
chem[chem$trmt=="Con", "trmt"] <- "Control"
chem$treat_stand<-paste(chem$Stand, chem$trmt)

head(chem)
table(chem$Year)

all$N <- chem$NH4.plus.NO3[match(all$statr, chem$treat_stand )]

vals$N <- chem$NH4.plus.NO3[match(vals$statr, chem$treat_stand )] 

head(all)

anova(lm( pri ~ Ntrmt * Ptrmt * N + Stand, data = all))


ggplot(all, aes(x= N, y = pri, col= Treatment, shape=age))+
  geom_point(size = 3)+
  #geom_smooth(method = "lm", se=F)+
  scale_color_manual(values = c("black","blue","red","purple"))+
  labs(x="Soil N", y="PRI")+
  facet_wrap(~age)

anova(lm( pri ~ Treatment * age + Stand, data = all))

tt <- HSD.test(lm( pri ~ Treatment +N + Stand, data = all), "Treatment") 
tt





###########################################################

out_sel <- list()
out_res <- list()

for(i in 1:3){
sel <- vals %>% filter(age== age_class[2] )

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

out_sel$age <- factor(out_sel$age, levels=c("young","mid","old"))

g1 <- ggplot(out_sel, aes(x = Treatment, y = pri, col=Treatment)) + 
  geom_boxplot(outlier.colour = "black", outlier.shape = 16,
               outlier.size = 2, notch = TRUE) +
  geom_text(aes(x = Treatment, y = max_y, label = group),
            vjust = -0.5, size = 5, fontface = "bold",
            inherit.aes = FALSE) +
    facet_wrap(~age, nrow = 1) +
  theme_bw() +
 # geom_jitter(width=.1)+
  scale_color_manual(values=c("black","blue","red","purple"))+
  theme(legend.position = "none") +
  labs(y = "PRI")
g1



out_res

tt$groups

tt$groups$avg <- mean(tt$groups$pri)
tt$groups$diff <- (tt$groups$pri - tt$groups$avg) * 100
