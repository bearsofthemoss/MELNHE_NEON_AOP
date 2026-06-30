
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
dada<-  read.csv(here::here( "data_folder","processed_spectra3.csv"))

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
lda_obj<-pre_lda[  ,c(5,6,11:355)]
names(pre_lda)

nzv <- nearZeroVar(lda_obj[,c(-1, -2)])



lda_obj[,134:139]

#lda_obj_cleaned <- lda_obj[ , -c(nzv +1)]
lda_obj_cleaned <- lda_obj[ , -c(nzv$Position +2)]


names(lda_obj_cleaned)

spec_avg <- tidyr::gather(lda_obj_cleaned, "Band", "value", 3:346)

############# PLot averages

library(ggplot2)
library(dplyr)

# # Create aggregated data (same as your base R code)
plot_avg <- aggregate(list( refl = spec_avg$value),
                      by = list(Stand = spec_avg$Stand,
                                Treatment = spec_avg$Treatment,
                                Band = spec_avg$Band ),
                      FUN = "mean", na.rm = TRUE)

# Prepare data for ellipses
plot_avg$Age <- dada$Age[match(plot_avg$Stand, dada$Stand)]


library(ggrepel)

plot_avg$Age <- factor(plot_avg$Age, c("Young stands","Mid-aged stands","Mature stands"))

lda_data <- spread(plot_avg, "Band","refl")


lda_data$trag <- paste(lda_data$Treatment, lda_data$Age)

names(lda_data)


lda_use <- lda_data[ ,c(348,4:347)]

names(lda_use)
# proportion explained by treatment
lda_res <- lda(as.factor(trag) ~ . , data = lda_use, CV=F) ### try resampling spectra to coarser resolution
prop.lda <- lda_res$svd^2/sum(lda_res$svd^2)*100 ### variability explained
lda_out <-  as.data.frame(as.matrix(lda_use[,-1]) %*% as.matrix(lda_res$scaling))

## Add back in plot level information
lda_out$Stand<- lda_data[ , "Stand"]
lda_out$Age<-lda_data[, "Age"]
lda_out$Treatment<-lda_data[ , "Treatment"]
lda_out$Treatment<-factor(lda_data$Treatment, levels=c("Control","N","P","NP"))

lda_out$staplo<-paste(lda_out$Stand, lda_out$Treatment)

# Prepare data for ellipses
ellipse_data <- lda_out

######################################


out <- lda_out





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
  geom_text_repel(data=lda_out,
                  aes(x = LD1, y = LD2,
                      label = Stand), size = 4)+
  # Set colors to match your original plot
  scale_color_manual(values = c("black", "blue", "red", "purple")) +
  scale_fill_manual(values = c("black", "blue", "red", "purple")) +
  # Set shapes to match your original plot (16=circle, 17=triangle, 15=square)
  scale_shape_manual(values = c(16, 17, 15)) +
  #  facet_wrap(~Stand)+
  # Labels
  labs(x = paste0("LD 1 (", round(prop.lda[1], 1), "%)"),
       y = paste0("LD 2 (", round(prop.lda[2], 1), "%)")) +
  # Theme adjustments
  theme_classic() +
  theme(legend.position= "bottom")
fig_trt




#####


# Extract the scaling (loadings) matrix
loadings_lda <- as.data.frame(lda_res$scaling)
loadings_lda <- loadings_lda[,1:2]
loadings_lda$wavelength <- as.numeric(sub(".*_", "", rownames(loadings_lda)))

# If rownames don't have the right format, use the column names of lda_use instead
loadings_lda$wavelength <- as.numeric(sub(".*_", "", colnames(lda_use)[-1]))


# Long format for plotting multiple LDs
loadings_long <- loadings_lda %>%
  tidyr::pivot_longer(cols      = -wavelength,
                      names_to  = "LD",
                      values_to = "Coefficient")

# Plot
ggplot(loadings_long, aes(x = wavelength, y = Coefficient, color = LD)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ LD, ncol = 1, scales = "free_y") +
  labs(x = "Wavelength (nm)", y = "LD Coefficient",
       title = "LDA discriminant coefficients by wavelength") +
  theme_minimal() +
  theme(legend.position = "none")

# Weight each LD's contribution by % variance explained
weighted_importance <- abs(as.matrix(loadings_lda[, -ncol(loadings_lda)])) * (prop.lda / 100)

importance_df <- data.frame(
  wavelength = loadings_lda$wavelength,
  importance = as.numeric(weighted_importance)
)

ggplot(importance_df, aes(x = wavelength, y = importance)) +
  geom_line(color = "darkgreen") +
  labs(x = "Wavelength (nm)", y = "Weighted |LD coefficient|",
       title = "Variable importance — weighted across discriminant axes") +
  theme_minimal()


##########

library(ggplot2)

# Sample scores (your existing lda_out, columns = LD1, LD2)
scores_df <- as.data.frame(lda_out[, 1:2])
names(scores_df)[1:2] <- c("LD1", "LD2")
scores_df$Treatment <- as.factor(lda_data$Treatment)
scores_df$Age <- as.factor(lda_data$Age)

scores_df$Treatment <- factor(scores_df$Treatment, levels=c("Control","N","P","NP"))

scores_df$Age <- factor(scores_df$Age, levels=c("Young stands","Mid-aged stands", "Mature stands"))


# Loadings (arrows) — using LD1 and LD2 columns from scaling
arrows_df <- as.data.frame(lda_res$scaling[, 1:2])
names(arrows_df) <- c("LD1", "LD2")
arrows_df$wavelength <- as.numeric(sub(".*_", "", rownames(arrows_df)))

# Select only the top N most important wavelengths (by arrow length / magnitude)
arrows_df$magnitude <- sqrt(arrows_df$LD1^2 + arrows_df$LD2^2)
top_arrows <- arrows_df[order(-arrows_df$magnitude), ][1:5, ]  # top 20

# Distance from origin (0,0) for both scores and arrows — not just abs range
score_range <- max(sqrt(scores_df$LD1^2 + scores_df$LD2^2))
arrow_range <- max(sqrt(top_arrows$LD1^2 + top_arrows$LD2^2))

scale_factor <- (score_range * 0.8) / arrow_range

top_arrows$LD1_scaled <- top_arrows$LD1 * scale_factor
top_arrows$LD2_scaled <- top_arrows$LD2 * scale_factor

scores_df$LD1 <- scores_df$LD1 - mean(scores_df$LD1)
scores_df$LD2 <- scores_df$LD2 - mean(scores_df$LD2)


top_arrows$wavelength<-round(top_arrows$wavelength,0)


# Plot: sample points + ellipses + wavelength arrows
ggplot() +
  geom_point(data = scores_df, aes(x = LD1, y = LD2, color = Treatment, shape=Age), size = 3) +
  stat_ellipse(data = scores_df, aes(x = LD1, y = LD2, group = Treatment, color = Treatment),
               linewidth = 0.6) +
  scale_color_manual(values=c("black","blue","red","purple"))+
  geom_segment(data = top_arrows,
               aes(x = 0, y = 0, xend = LD1_scaled, yend = LD2_scaled),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "red", linewidth = 0.5,
               linetype="solid") +
  ggrepel::geom_text_repel(data = top_arrows,
                           aes(x = LD1_scaled, y = LD2_scaled,
                               label = paste0(wavelength, "nm")),
                           color = "red", size = 3) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(x = "LD1", y = "LD2") +
  theme_bw() +
  theme(panel.grid=element_blank())+
    facet_wrap(~Age, nrow=3)



ggplot() +
  geom_point(data = scores_df, aes(x = LD1, y = LD2, color = Treatment, shape=Age), size = 3) +
  stat_ellipse(data = scores_df, aes(x = LD1, y = LD2, group = Treatment, color = Treatment),
               linewidth = 0.6) +
  scale_color_manual(values=c("black","blue","red","purple"))+
  geom_segment(data = top_arrows,
               aes(x = 0, y = 0, xend = LD1_scaled, yend = LD2_scaled),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "red", linewidth = 0.5,
               linetype="solid") +
  ggrepel::geom_text_repel(data = top_arrows,
                           aes(x = LD1_scaled, y = LD2_scaled,
                               label = paste0(wavelength, "nm")),
                           color = "red", size = 3) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(x = "LD1", y = "LD2") +
  theme_bw() +
  theme(panel.grid=element_blank())
#facet_wrap(~Age, nrow=3)
