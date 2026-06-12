
library(tidyverse)
library(MASS)
library(plotly)
library(vegan)
library(agricolae)
library(here)
library(caret)
library(dplyr)
library(ggforce)  # for stat_ellipse
library(metR) 
library(tidyr)


  
## dada contains the tree top reflectance.This was made in file 2. 
dada<-  read.csv(here::here("data_folder","processed_spectra3.csv"))

names(dada)
# add in stand ages
dada$Age[dada$Stand=="C1"]<-"Young stands"
dada$Age[dada$Stand=="C2"]<-"Young stands"
dada$Age[dada$Stand=="C3"]<-"Young stands"
dada$Age[dada$Stand=="C4"]<-"Mid-aged stands"
dada$Age[dada$Stand=="C5"]<-"Mid-aged stands"
dada$Age[dada$Stand=="C6"]<-"Mid-aged stands" 
dada$Age[dada$Stand=="C7"]<-"Mature stands"
dada$Age[dada$Stand=="C8"]<-"Mature stands"
dada$Age[dada$Stand=="C9"]<-"Mature stands"



library(tidyr)
# gather spectra for averaging
names(dada)
spectra_gather<-gather(dada, "wvl","refl",8:352)

spectra_gather$plot<-paste(spectra_gather$Stand, spectra_gather$Treatment)

pre_lda<-spread(spectra_gather, wvl,refl) ### means

names(pre_lda)
lda_obj<-pre_lda[  ,c(5,11:355)]
names(pre_lda)

nzv <- nearZeroVar(lda_obj[,-1])



lda_obj[,134:139]

lda_obj_cleaned <- lda_obj[ , -c(nzv$Position +1)]

summary(lda_obj_cleaned)

dim(lda_obj)
dim(lda_obj_cleaned)

names(lda_obj_cleaned)

# proportion explained by treatment
lda_res <- lda(as.factor(Treatment) ~ . , data = lda_obj_cleaned, CV=F) ### try resampling spectra to coarser resolution
prop.lda <- lda_res$svd^2/sum(lda_res$svd^2)*100 ### variability explained
lda_out <-  as.data.frame(as.matrix(lda_obj_cleaned[,-1]) %*% as.matrix(lda_res$scaling))

## Add back in plot level information
lda_out$Stand<- pre_lda[ , "Stand"]
lda_out$Age<-pre_lda[, "Age"]
lda_out$Treatment<-pre_lda[ , "Treatment"]
lda_out$Treatment<-factor(lda_out$Treatment, levels=c("Control","N","P","NP"))

lda_out$staplo<-paste(lda_out$Stand, lda_out$Treatment)

# Prepare data for ellipses
ellipse_data <- lda_out

######################################


# proportion explained by Age

lda_obj_age <- pre_lda[  ,c(9,11:355)]



lda_age <- lda(as.factor(Age) ~ . , data = lda_obj_age, CV=F) ### try resampling spectra to coarser resolution
prop.lda_age <- lda_res$svd^2/sum(lda_age$svd^2)*100 ### variability explained
lda_out_age <-  as.data.frame(as.matrix(lda_obj_age[,-1]) %*% as.matrix(lda_age$scaling))

lda_out_age$Age <- pre_lda[, "Age"]

lda_out_age$Age <- factor(lda_out_age$Age, levels=c("Young stands","Mid-aged stands",
                                                    "Mature stands"))
# Prepare data for ellipses
ellipse_data_age <- lda_out_age 

# Added in Age LDA above #############







# lda_out$total_N<-chem$NH4.hyphen.N[match(lda_out$staplo, chem$treat_stand )]
# lda_out$total_P<-chem$PO4.hyphen.P[match(lda_out$staplo, chem$treat_stand )]

out <- lda_out




library(ggplot2)
library(dplyr)

# # Create aggregated data (same as your base R code)
plot_avg <- aggregate(list(LD1 = out$LD1,
                           LD2 = out$LD2),
                      by = list(Stand = out$Stand,
                                Treatment = out$Treatment,
                                staplo = out$staplo ),
                      FUN = "mean", na.rm = TRUE)

#plot_avg <- out

# Prepare data for ellipses
plot_avg$Age <- dada$Age[match(plot_avg$Stand, dada$Stand)]



ellipse_data$Treatment

library(ggrepel)

plot_avg$Age <- factor(plot_avg$Age, c("Young stands","Mid-aged stands","Mature stands"))

# Create the ggplot
fig_trt <- ggplot() +
  # Add ellipses (using stat_ellipse for 95% confidence ellipses)
  stat_ellipse(data = ellipse_data,
               aes(x = LD1, y = LD2, color = Treatment),linewidth=1,
               geom = "path", alpha = 0.95, level = 0.95, type = "norm") +
  # Add points
  geom_point(data = lda_out,
             aes(x = LD1, y = LD2,
                 color = Treatment),
             size = 2, alpha = 0.6) +
  # geom_text_repel(data=plot_avg, 
  #                 aes(x = LD1, y = LD2,
  #                     label = Stand), size = 4)+
  # Set colors to match your original plot
  scale_color_manual(values = c("black", "blue", "red", "purple")) +
  scale_fill_manual(values = c("black", "blue", "red", "purple")) +
  # Set shapes to match your original plot (16=circle, 17=triangle, 15=square)
  scale_shape_manual(values = c(16, 17, 15)) +
  facet_wrap(~Stand)+
  # Labels
  labs(x = paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
       y = paste0("LD 2 (", round(prop.lda[2], 1), "%)")) +
  # Theme adjustments
  theme_classic() +
  theme(legend.position= "bottom")
fig_trt

#################################################

fig_age <- ggplot() +
  # Add ellipses (using stat_ellipse for 95% confidence ellipses)
  stat_ellipse(data = ellipse_data_age,
               aes(x = LD1, y = LD2, color = Age),linewidth=1,
               geom = "path", alpha = 0.95, level = 0.95, type = "norm") +
  # Add points
  geom_point(data = lda_out_age,
             aes(x = LD1, y = LD2,
                 color = Age, shape=Age),
             size = 1, alpha = 0.95, stroke =1.5) +
  scale_color_manual(values = c("Young stands" = "#E6AB02", 
                               "Mid-aged stands" = "#666666", 
                               "Mature stands" = "#D95F02")) +
  scale_shape_manual(values = c("Young stands" = 21, 
                                "Mid-aged stands" = 22, 
                                "Mature stands" = 24)) +
  # Labels
  labs(x = paste0("LD 1 (", round(prop.lda_age[1], 1), "%)"),
       y = paste0("LD 2 (", round(prop.lda_age[2], 1), "%)")) +
  # Theme adjustments
  theme_classic() +
  theme(legend.position= "bottom")
fig_age

library(patchwork)
fig2 <-  fig_trt + fig_age 

fig2
ggsave("figure_2.png", fig2, 
       width = 9, height = 4.5, dpi = 300, bg = "white")
