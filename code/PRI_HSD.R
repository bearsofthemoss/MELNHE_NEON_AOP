##############
library(ggplot2)
library(tidyverse)
library(agricolae)

vals <- read.csv("./PRI_data_Bartlett_NP.csv")

## add age info
dat <- as.data.frame(matrix(data = c(rep("young",3), rep("mid",3), rep("old",3),
       "C1", "C2","C3","C4","C5","C6","C7","C8","C9"), ncol = 2))

colnames(dat) <- c("age", "Stand")
vals <- vals %>% merge(dat,"Stand")


age_class <- c("young","mid","old")


###########################################################
### Young stands - all tree tops (no averaging by stand)

out_sel <- list()

for(i in 1:3){
sel <- vals %>% filter(age== age_class[i] )


mod <-lm(pri~Treatment+Stand,sel) 
anova(mod)
tt <- HSD.test(mod, "Treatment")
tt ### PRI is lower for P 

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



###########################################################
### Young stands - averaged by stand (and treatment)

seli <- sel %>% select(Stand, Treatment, pri) %>% group_by(Stand,Treatment) %>%
  summarize_at(c("pri"), .funs = mean)


model <- lmer(pri ~ Treatment + (1|Stand), data=seli)
young_pri_mod_tree <- as.data.frame(anova(model))

modi <-lm(pri~Treatment,seli) 
anova(modi)
tt <- HSD.test(modi, "Treatment") 
tt ### no sign differences.... with 3 observations by treatment that is to be expected

##### END #######

