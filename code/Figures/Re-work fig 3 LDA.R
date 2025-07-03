
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



pre_lda<-spread(spectra_gather, wvl,refl) ### means

names(pre_lda)

par(mfrow=c(3,2))

#  Young stands #### 
sel_Age <- "Young forest"

lda_obj<-pre_lda[pre_lda$Age== sel_Age ,c(4,10:353)]

nzv <- nearZeroVar(lda_obj[,-1])
problem_vars <- nzv


# Remove vars with 0 variance
if(length(problem_vars) > 0) {
  lda_obj_cleaned <- lda_obj[, -(problem_vars + 1)]
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
lda_out$Stand<-pre_lda[pre_lda$Age==sel_Age, "Stand"]
lda_out$Age<-pre_lda[pre_lda$Age==sel_Age, "Age"]
lda_out$Treatment<-pre_lda[pre_lda$Age==sel_Age, "Treatment"]
lda_out$Treatment<-factor(lda_out$Treatment, levels=c("Control","N","P","NP"))

lda_out$staplo<-paste(lda_out$Stand, lda_out$Treatment)
lda_out$total_N<-chem$NH4.hyphen.N[match(lda_out$staplo, chem$treat_stand )]
lda_out$total_P<-chem$PO4.hyphen.P[match(lda_out$staplo, chem$treat_stand )]

out <- lda_out

plot(out$LD1, out$LD2, type="n",bty="l",col="grey50", 
     xlab=paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
     ylab=paste0("LD 2 (", round(prop.lda[2], 1), "%)"))
title(main=paste0(sel_Age, " Soil Nitrogen"),   cex.main=1.5,adj = 0)
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=1)

#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_N,out,add=T, col="grey50", lwd=1.5, labcex=1.2)
legend("topleft", legend = unique(out$Treatment), pch=19,col=c("black","blue","red","purple")[out$Treatment] ,bty ="n", cex=1.3) 

###
plot(out$LD1, out$LD2, type="n",bty="l",col="grey50",
     xlab=paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
     ylab=paste0("LD 2 (", round(prop.lda[2], 1), "%)"))
     title(main=paste0(sel_Age, " Soil Phosphorus"),   cex.main=1.5,adj = 0)
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=1)

#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_P,out,add=T, col="grey50", lwd=1.5, labcex=1.2)




#  Mid-aged forest #####
sel_Age <- "Mid-aged forest"

lda_obj<-pre_lda[pre_lda$Age== sel_Age ,c(4,10:353)]

nzv <- nearZeroVar(lda_obj[,-1])
problem_vars <- nzv


# Remove vars with 0 variance
if(length(problem_vars) > 0) {
  lda_obj_cleaned <- lda_obj[, -(problem_vars + 1)]
  cat("Removed", length(problem_vars), "near-zero variance variables\n")
} else{
  lda_obj_cleaned <- lda_obj
}


# proportion explained by treatment
lda_res <- lda(as.factor(Treatment) ~ . , data = lda_obj_cleaned, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- lda_res$svd^2/sum(lda_res$svd^2)*100) ### variability explained
lda_out <-  as.data.frame(as.matrix(lda_obj_cleaned[,-1]) %*% as.matrix(lda_res$scaling))


## Add back in plot level information
lda_out$Stand<-pre_lda[pre_lda$Age==sel_Age, "Stand"]
lda_out$Age<-pre_lda[pre_lda$Age==sel_Age, "Age"]
lda_out$Treatment<-pre_lda[pre_lda$Age==sel_Age, "Treatment"]
lda_out$Treatment<-factor(lda_out$Treatment, levels=c("Control","N","P","NP"))

lda_out$staplo<-paste(lda_out$Stand, lda_out$Treatment)
lda_out$total_N<-chem$NH4.hyphen.N[match(lda_out$staplo, chem$treat_stand )]
lda_out$total_P<-chem$PO4.hyphen.P[match(lda_out$staplo, chem$treat_stand )]

out <- lda_out

plot(out$LD1, out$LD2, type="n",bty="l",col="grey50",
     xlab=paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
     ylab=paste0("LD 2 (", round(prop.lda[2], 1), "%)"))
title(main=paste0(sel_Age, " Soil Nitrogen"),   cex.main=1.5,adj = 0)
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=1)

#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_N,out,add=T, col="grey50", lwd=1.5, labcex=1.2)
legend("topleft", legend = unique(out$Treatment), pch=19,col=c("black","blue","red","purple")[out$Treatment] ,bty ="n", cex=1.3) 

###
plot(out$LD1, out$LD2, type="n",bty="l",col="grey50",
     xlab=paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
     ylab=paste0("LD 2 (", round(prop.lda[2], 1), "%)"))
title(main=paste0(sel_Age, " Soil Phosphorus"),   cex.main=1.5,adj = 0)
points(out$LD1, out$LD2, col=c("black","blue","red","purple")[as.factor(out$Treatment)],
       pch=c(16,17,15)[as.factor(out$Age)], cex=1)

#text(out$LD1, out$LD2, labels=out$Stand, cex= 1,pos=4) ### label points
ordiellipse(out[,c(1,2)], groups = out$Treatment, draw = "polygon", lty = 1, col = c("black","blue","red","purple"))
ordisurf(out[,c(1,2)]~total_P,out,add=T, col="grey50", lwd=1.5, labcex=1.2)





# Mature forest #### 

sel_Age <- "Mature forest"

lda_obj<-pre_lda[pre_lda$Age== sel_Age ,c(4,10:353)]

nzv <- nearZeroVar(lda_obj[,-1])
problem_vars <- which(nzv$nzv == TRUE)


# Remove vars with 0 variance
if(length(problem_vars) > 0) {
  lda_obj_cleaned <- lda_obj[, -(problem_vars + 1)]
  cat("Removed", length(problem_vars), "near-zero variance variables\n")
} else{
  lda_obj_cleaned <- lda_obj
}


# proportion explained by treatment
lda_res <- lda(as.factor(Treatment) ~ . , data = lda_obj_cleaned, CV=F) ### try resampling spectra to coarser resolution
(prop.lda <- lda_res$svd^2/sum(lda_res$svd^2)*100) ### variability explained
lda_out <-  as.data.frame(as.matrix(lda_obj_cleaned[,-1]) %*% as.matrix(lda_res$scaling))


## Add back in plot level information
lda_out$Stand<-pre_lda[pre_lda$Age==sel_Age, "Stand"]
lda_out$Age<-pre_lda[pre_lda$Age==sel_Age, "Age"]
lda_out$Treatment<-pre_lda[pre_lda$Age==sel_Age, "Treatment"]
lda_out$Treatment<-factor(lda_out$Treatment, levels=c("Control","N","P","NP"))

lda_out$staplo<-paste(lda_out$Stand, lda_out$Treatment)
lda_out$total_N<-chem$NH4.hyphen.N[match(lda_out$staplo, chem$treat_stand )]
lda_out$total_P<-chem$PO4.hyphen.P[match(lda_out$staplo, chem$treat_stand )]

plot_data <- lda_out

library(akima)

# Create a grid for interpolation
ld1_range <- range(plot_data$LD1, na.rm = TRUE)
ld2_range <- range(plot_data$LD2, na.rm = TRUE)

# Interpolate to create a smooth surface
interp_result <- akima::interp(x = plot_data$LD1, 
                        y = plot_data$LD2, 
                        z = plot_data$total_P,
                        xo = seq(ld1_range[1], ld1_range[2], length = 50),
                        yo = seq(ld2_range[1], ld2_range[2], length = 50))

# Convert to data frame for ggplot
contour_data <- expand.grid(x = interp_result$x, y = interp_result$y)
contour_data$z <- as.vector(interp_result$z)
contour_data <- contour_data[!is.na(contour_data$z), ]

# Now plot with the interpolated data
p <- ggplot() +
  geom_contour(data = contour_data, aes(x = x, y = y, z = z, color = after_stat(level)), 
               size = 3, bins = 20) +
  scale_color_gradient(low = "pink", high = "darkred", name = "Soil P") +
  # Add transparent ellipses behind the points
  stat_ellipse(data = plot_data, aes(x = LD1, y = LD2, fill = Treatment), 
               alpha = 0.2, level = 0.68, geom = "polygon") +
  geom_point(data = plot_data, aes(x = LD1, y = LD2, fill = Treatment), 
             size = 3,shape=21, stroke = 0.3) +
  scale_fill_manual(values = c("black","blue","red","purple")) +

  scale_shape_manual(values = c(21, 24, 22)) +
  
  labs(x = paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
       y = paste0("LD 2 (", round(prop.lda[2], 1), "%)"),
       title = paste0(sel_Age, " Soil Phosphorus")) +
  theme_classic()

print(p)



#Here we could ask how much the tree species explained the spectral variation by plot

############ quick adonis test
## Add back in plot level information
dada$staplo<-paste(dada$Stand, dada$Treatment)
dada$total_N<-chem$total_N[match(dada$staplo, chem$treat_stand )]
dada$total_P<-chem$PO4.hyphen.P[match(dada$staplo, chem$treat_stand )]
dada$bap<-bap$x[match(dada$staplo, bap$staplo)]


names(dada)
spec.matrix<-dada[,7:351]
adonis2(spec.matrix ~ total_N, data=dada, permutations = 100, method = "bray",strata = dada$Stand)

spec.pca <- prcomp(spec.matrix ,center = TRUE, scale = TRUE) ## means per treat_stand
# spec.pca <- prcomp(dada[,-c(1:5)],center = TRUE, scale = TRUE) ## pixels
plot(spec.pca,type="l")
summary(spec.pca)

plot(spec.pca)



head(pcdat)
pcdat$Trt<-factor(pcdat$Trt, levels=c("Control","N","P","NP"))

PC1<-spec.pca$x[,1]
PC2<-spec.pca$x[,2]
PC3<-spec.pca$x[,3]
PC4<-spec.pca$x[,4]
PC5<-spec.pca$x[,5]
PC6<-spec.pca$x[,6]

pcdat<-data.frame(PC1,PC2,PC3,PC4, PC5, PC6)
pcdat
dim(dada)
perm<-cbind(dada[,c(1:5,351:355) ],pcdat)

names(dada)
head(perm[1:10])
names(perm)
adonis(perm[,11:16] ~ perm$Treatment,method="euclidean", strata=perm$Stand, data=perm)


