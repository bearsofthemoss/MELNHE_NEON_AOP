
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
dada<-  read.csv("tree_spectra_processed.csv")
#dada<-dada[,-1]   # when saving the .csv, the first column values are just X
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

library(tidyr)
# gather spectra for averaging
names(dada)
spectra_gather<-gather(dada, "wvl","refl",7:351)

spectra_gather$plot<-paste(spectra_gather$Stand, spectra_gather$Treatment)

pre_lda<-spread(spectra_gather, wvl,refl) ### means

lda_obj<-pre_lda[  ,c(4,10:354)]


nzv <- nearZeroVar(lda_obj[,-1])
problem_vars <- nzv


# Remove vars with 0 variance
if(length(problem_vars) > 0) {
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

# Try LDA again
lda_res <- lda(as.factor(Treatment) ~ ., data = lda_obj_filtered, CV = FALSE)


# proportion explained by treatment
lda_res <- lda(as.factor(Treatment) ~ . , data = lda_obj_cleaned, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- lda_res$svd^2/sum(lda_res$svd^2)*100) ### variability explained
lda_out <-  as.data.frame(as.matrix(lda_obj_cleaned[,-1]) %*% as.matrix(lda_res$scaling))


## Add back in plot level information
lda_out$Stand<- pre_lda[ , "Stand"]
lda_out$Age<-pre_lda[, "Age"]
lda_out$Treatment<-pre_lda[ , "Treatment"]
lda_out$Treatment<-factor(lda_out$Treatment, levels=c("Control","N","P","NP"))

lda_out$staplo<-paste(lda_out$Stand, lda_out$Treatment)
lda_out$total_N<-chem$NH4.hyphen.N[match(lda_out$staplo, chem$treat_stand )]
lda_out$total_P<-chem$PO4.hyphen.P[match(lda_out$staplo, chem$treat_stand )]

out <- lda_out



library(ggplot2)
library(dplyr)

# Create aggregated data (same as your base R code)
plot_avg <- aggregate(list(LD1 = out$LD1,
                           LD2 = out$LD2),
                      by = list(Stand = out$Stand,
                                Age = out$Age,
                                Treatment = out$Treatment,
                                staplo = out$staplo), 
                      FUN = "mean", na.rm = TRUE)

# Prepare data for ellipses
ellipse_data <- out %>%
  select(LD1, LD2, Treatment) %>%
  filter(!is.na(LD1) & !is.na(LD2))

# Create the ggplot
ggplot() +
  # Add ellipses (using stat_ellipse for 95% confidence ellipses)
  stat_ellipse(data = ellipse_data,
               aes(x = LD1, y = LD2, fill = Treatment, color = Treatment),
               geom = "polygon", alpha = 0.05, level = 0.68, type = "norm") +
  # Add points
  geom_point(data = plot_avg,
             aes(x = LD1, y = LD2, 
                 color = Treatment, 
                 shape = Age),
             size = 3, alpha = 0.6) +
  # Set colors to match your original plot
  scale_color_manual(values = c("black", "blue", "red", "purple")) +
  scale_fill_manual(values = c("black", "blue", "red", "purple")) +
  # Set shapes to match your original plot (16=circle, 17=triangle, 15=square)
  scale_shape_manual(values = c(16, 17, 15)) +
  # Labels
  labs(x = paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
       y = paste0("LD 2 (", round(prop.lda[2], 1), "%)")) +
  # Theme adjustments
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0))

