### Ordinations ################
### Anna Schweiger Nov 6 2019- Alex Young 10_4_2020 ###
library(tidyverse)
library(ggrepel)
library(MASS)
library(plotly)
library(vegan)
library(agricolae)
library(here)
library(caret)
library(tidyr)

library(ggplot2)
library(patchwork)
library(dplyr)
library(akima)


## dada contains the tree top reflectance.This was made in file 2. 
dada<- read.csv(here::here("data_folder","actual_tops.csv"))
dada<-dada[,-1]   # when saving the .csv, the first column values are just X
names(dada)
# add in stand ages
dada$Age[dada$Stand=="C1"]<-"Young forest"
dada$Age[dada$Stand=="C2"]<-"Young forest"
dada$Age[dada$Stand=="C3"]<-"Young forest"
dada$Age[dada$Stand=="C4"]<-"Mid-aged forest"
dada$Age[dada$Stand=="C5"]<-"Mid-aged forest"
dada$Age[dada$Stand=="C6"]<-"Mid-aged forest" 
dada$Age[dada$Stand=="C7"]<-"Mature forest"
dada$Age[dada$Stand=="C8"]<-"Mature forest"
dada$Age[dada$Stand=="C9"]<-"Mature forest"


## chem contains the resin available N and P from 2017 measurements
chem <-  read.csv(here::here("data_folder","melnhe_input_files","resin_available_N_P_melnhe.csv"))
chem[chem$trmt=="Con", "trmt"] <- "Control"
chem$treat_stand<-paste(chem$Stand, chem$trmt)

head(chem)

#############

###############################
#
#  Young Forest set up  (p1 and p2)
########
yd <- dada[dada$Age=="Young forest",]

lda_obj<-yd[complete.cases(yd),]

lda_obj<-lda_obj[,c(4,7:351)]

### create lda_obj_filtered


nzv <- nearZeroVar(lda_obj[, -1])

problem_vars <- nzv

# Remove vars with 0 variance

if(length(problem_vars) > 0) {
  lda_obj_cleaned <- lda_obj[, -(problem_vars + 1)]
 lda_obj_cleaned <- lda_obj[, -(problem_vars$Position + 1)]
   cat("Removed", length(problem_vars), "near-zero variance variables\n")
} else{
  lda_obj_cleaned <- lda_obj
}

# Function to identify constant variables within groups
find_constant_vars <- function(data, group_var) {
    group_col <- which(names(data) == group_var)
  predictor_data <- data[, -group_col]
  groups <- data[[group_var]]

  constant_vars <- c()
  
  for(i in 1:ncol(predictor_data)) {
    # Check if variable is constant within any group
    is_constant <- any(tapply(predictor_data[,i], groups, function(x) var(x, na.rm = TRUE) == 0))
    if(is_constant) {
      constant_vars <- c(constant_vars, i)
    }
    
  }
  return(constant_vars)
}

# Find constant variables
constant_vars <- find_constant_vars(lda_obj_cleaned, "Treatment")

# Remove constant variables

if(length(constant_vars) > 0) {
  lda_obj_filtered <- lda_obj_cleaned[, -constant_vars]
  cat("Removed", length(constant_vars), "constant variables\n")
} else {
  lda_obj_filtered <- lda_obj_cleaned
}


# Check for missing values
missing_summary <- colSums(is.na(lda_obj_filtered))
vars_with_missing <- which(missing_summary > 0)


# Remove highly correlated variables
cor_matrix <- cor(lda_obj_filtered[,-1])  # Exclude Treatment column
high_cor <- findCorrelation(cor_matrix, cutoff = 0.95)  # Adjust cutoff as needed

if(length(high_cor) > 0) {
  lda_obj_final <- lda_obj_filtered[, -(high_cor + 1)]  # +1 to account for Treatment column
  cat("Removed", length(high_cor), "highly correlated variables\n")
} else {
  lda_obj_final <- lda_obj_filtered
}

# Now try LDA
lda_test <- lda(as.factor(Treatment) ~ ., data = lda_obj_final, CV = FALSE)


# calculate proportion
lres <- lda(as.factor(Treatment) ~ ., data = lda_obj_final, CV = FALSE)
(prop.lda <- lres$svd^2/sum(lres$svd^2)*100) ### variability explained
out <-  as.data.frame(as.matrix(lda_obj_final[,-1]) %*% as.matrix(lres$scaling))

dim(out)
dim(yd)

out$Stand <- yd$Stand
out$Treatment <- yd$Treatment

# take plot-level average of LD1 and LD2 scores
out <- aggregate(list( LD1 = out$LD1, LD2 = out$LD2),
                 by=list(Treatment = out$Treatment,
                         Stand = out$Stand),
                 FUN= "mean", na.rm=T)


## Add back in plot level information

out$Treatment<-factor(out$Treatment, levels=c("Control","N","P","NP"))
table(out$Treatment)
out$staplo<-paste(out$Stand, out$Treatment)
out$total_N<-chem$NH4.hyphen.N[match(out$staplo, chem$treat_stand )]
out$total_P<-chem$PO4.hyphen.P[match(out$staplo, chem$treat_stand )]


### 
# Interpolate for smoother contours
library(akima)
interp_N <- with(out, interp(LD1, LD2, total_N, duplicate = "mean"))
interp_P <- with(out, interp(LD1, LD2, total_P, duplicate = "mean"))

# Convert to data frame
df_N <- expand.grid(x = interp_N$x, y = interp_N$y) %>%
  mutate(z = as.vector(interp_N$z))

df_P <- expand.grid(x = interp_P$x, y = interp_P$y) %>%
  mutate(z = as.vector(interp_P$z))


# For the Nitrogen plot (p1)
p1 <- ggplot(out, aes(x = LD1, y = LD2)) +
  geom_contour_filled(data = df_N, aes(x = x, y = y, z = z), 
                      alpha = 0.7, bins = 5) +
  geom_text_repel(aes(label = Stand)) +
  geom_point(aes(color = Treatment), size = 3) +
  stat_ellipse(aes(color = Treatment), linewidth = 1) +
  scale_fill_manual(values = colorRampPalette(c("lightblue", "darkblue"))(9),
                    name = "Total N") +
  scale_color_manual(values = c("Control" = "black", "N" = "blue", 
                                "P" = "red", "NP" = "purple")) +
  labs(x = paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
       y = paste0("LD 2 (", round(prop.lda[2], 1), "%)"),
       title = "Young forests") +
  theme_classic()

# For the Phosphorus plot (p2)
p2 <- ggplot(out, aes(x = LD1, y = LD2)) +
  geom_contour_filled(data = df_P, aes(x = x, y = y, z = z), 
                      alpha = 0.7, bins = 5) +
  geom_text_repel(aes(label = Stand)) +
  geom_point(aes(color = Treatment), size = 3) +
  stat_ellipse(aes(color = Treatment), linewidth = 1) +
  scale_fill_manual(values = colorRampPalette(c("lightpink", "darkred"))(9),
                    name = "Total P") +
  scale_color_manual(values = c("Control" = "black", "N" = "blue", 
                                "P" = "red", "NP" = "purple")) +
  labs(x = paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
       y = paste0("LD 2 (", round(prop.lda[2], 1), "%)"),
       title = "Young forests") +
  theme_classic()

p1+p2


###############################
#
#  Young Forest set up  (p3 and p4)
########
md <- dada[dada$Age=="Mid-aged forest",]

lda_obj<-md[complete.cases(md),]



###############################
#
#  Mature Forest set up  (p5 and p6)
########
od <- dada[dada$Age=="Mature forest",]

lda_obj<-od[complete.cases(od),]


################


# Now with all 6 ggplot figures made
(p1 + p2) / (p3 + p4) / (p5 + p6) + plot_layout(guides = "collect")

