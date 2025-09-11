
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


# do one age class at a time
sel_Age <- "Young forest"

  
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

library(tidyr)
# gather spectra for averaging
names(dada)
spectra_gather<-gather(dada, "wvl","refl",7:351)
table(spectra_gather$height)

names(spectra_gather)
spectra_gather$plot<-paste(spectra_gather$Stand, spectra_gather$Treatment)
head(spectra_gather)
table(spectra_gather$Stand)



###### LDA ##############

dim(pre_lda)

pre_lda<-spread(spectra_gather, wvl,refl) ### means

# Based on the specified sel_Age at the top, run the stats for that age class

################################################################

## Selected age above

lda_obj<-pre_lda[  ,c(4,10:354)]
#lda_obj<-pre_lda[pre_lda$Age==sel_Age ,c(4,10:354)]

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


dim(lda_out)


## Add back in plot level information
lda_out$Stand<- pre_lda[ , "Stand"]
lda_out$Age<-pre_lda[, "Age"]
lda_out$Treatment<-pre_lda[ , "Treatment"]
lda_out$Treatment<-factor(lda_out$Treatment, levels=c("Control","N","P","NP"))

lda_out$staplo<-paste(lda_out$Stand, lda_out$Treatment)
lda_out$total_N<-chem$NH4.hyphen.N[match(lda_out$staplo, chem$treat_stand )]
lda_out$total_P<-chem$PO4.hyphen.P[match(lda_out$staplo, chem$treat_stand )]

out <- lda_out


plot_avg <- aggregate(list(LD1 = out$LD1,
                           LD2 = out$LD2),
                      by= list(Stand=  out$Stand,
                               Age = out$Age,
                               Stand = out$Stand,
                               Treatment = out$Treatment,
                               staplo = out$staplo), FUN="mean", na.rm=T)

# Combined LDA visualization with P ordiellipse in red and N isoclines in blue
# Set up layout: 2 plots side by side, with shared legend on right
layout(matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE), 
       widths = c(1, 1), heights = c(4, 1))

# Alternative layout option (uncomment if you prefer):
# layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3), widths = c(1, 1, 0.5))

# Plot 1: Left figure (your existing plot)
par(mar = c(4, 4, 3, 1))  # smaller right margin since legend is separate


# Create the base plot
plot(out$LD1, out$LD2, type="n", bty="l", col="grey50", 
     xlab=paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
     ylab=paste0("LD 2 (", round(prop.lda[2], 1), "%)"))

title(main="Soil N availability", cex.main=1.5, adj = 0)

# Add the data points
points(plot_avg$LD1, plot_avg$LD2, 
       col=c("black","blue","red","purple")[as.factor(plot_avg$Treatment)],
       alpha.f=0.6,
       pch=c(16,17,15)[as.factor(plot_avg$Age)], cex=1)

# Add ellipse
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon",
           col = c("black","blue","red","purple")) 

# # Nitrogen contours in blue
ordisurf(out[,c(1,2)]~total_N, out, add=T, col="blue",
         lwd.cl = c(0.5, 1, 1.5, 2, 2.5), labcex=1.2)


# Plot 2: Right figure (your second plot)- P in the soil
par(mar = c(4, 3, 3, 1))  # smaller left margin too

# Create the base plot
plot(out$LD1, out$LD2, type="n", bty="l", col="grey50", 
     xlab=paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
     ylab=paste0("LD 2 (", round(prop.lda[2], 1), "%)"))

title(main="Soil P availability", cex.main=1.5, adj = 0)

# Add the data points
points(plot_avg$LD1, plot_avg$LD2, 
       col=c("black","blue","red","purple")[as.factor(plot_avg$Treatment)],
       alpha.f=0.6,
       pch=c(16,17,15)[as.factor(plot_avg$Age)], cex=1)

# Add ellipse
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon",
             col = c("black","blue","red","purple")) 

# # P contours in red
ordisurf(out[,c(1,2)]~total_P, out, add=T, col="red",
         lwd.cl = c(0.5, 1, 1.5, 2, 2.5), labcex=1.2)



# Plot 3: Shared legend below both plots
par(mar = c(0, 0, 0, 0))
plot.new()

# Create three separate legend columns
# Column 1: Age Classes
legend(0.1, 1, 
       legend = c("Age Classes:", unique(out$Age)), 
       pch = c(NA, c(16,17,15)[1:length(unique(out$Age))]),
       col = c(NA, rep("black", length(unique(out$Age)))),
       bty = "n", cex = 1.2)

# Column 2: Nutrient Treatments  
legend(0.45, 1, 
       legend = c("Nutrient Treatments:", "Control", "N", "P", "NP"), 
       pch = c(NA, rep(19, 4)),
       col = c(NA, "black", "blue", "red", "purple"),
       bty = "n", cex = 1.2)

# Column 3: Soil Nutrients
legend(0.8, 1, 
       legend = c("Soil Nutrients:", "Soil N", "Soil P"), 
       pch = c(NA, NA, NA),
       lty = c(NA, 1, 1),
       col = c(NA, "blue", "red"),
       bty = "n", cex = 1.2)

# Reset layout when done
layout(1)




