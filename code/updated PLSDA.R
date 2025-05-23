library(caret)
library(mixOmics)
library(corrplot)
library(MLmetrics)

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# ## install mixOmics
# BiocManager::install('mixOmics')

dati <- read.csv("./data_folder/actual_tops.csv", row.names = 1)

dati$Age[dati$Stand=="C1"]<-"~30 years old"
dati$Age[dati$Stand=="C2"]<-"~30 years old"
dati$Age[dati$Stand=="C3"]<-"~30 years old"
dati$Age[dati$Stand=="C4"]<-"~60 years old"
dati$Age[dati$Stand=="C5"]<-"~60 years old"
dati$Age[dati$Stand=="C6"]<-"~60 years old" 
dati$Age[dati$Stand=="C7"]<-"~100 years old"
dati$Age[dati$Stand=="C8"]<-"~100 years old"
dati$Age[dati$Stand=="C9"]<-"~100 years old"

names(dati)

dati <- dati[dati$Age=="~30 years old",]

#spec <- dati[dati$Age=="~60 years old",]
spec <- dati[ , -ncol(dati)]


### 1. Prepare Data ###
# Assuming 'dati' contains your data with Treatment column
# and 'spec' contains your spectral data


spec <- spec[complete.cases(spec),] ### remove NAs

treatment_classes <- as.factor(spec$Treatment)

# select just the spectral data
spec <- spec[,c(6:350)] 

### 2. Data Splitting ###
set.seed(123)  # For reproducibility
train_index <- createDataPartition(treatment_classes, p = 0.75, list = FALSE)

# Training and testing sets
train_spec <- spec[train_index,]
test_spec <- spec[-train_index,]
train_classes <- treatment_classes[train_index]
test_classes <- treatment_classes[-train_index]

dim(spec)
dim(train_spec)
dim(test_spec)

length(train_classes)
length(test_classes)



### 3. Determine Optimal Number of Components ###
cat("\n=== Component Selection using PLSDA ===\n")

# Test different numbers of components with cross-validation
max_comp <- min(15, nrow(train_spec) - 1, ncol(train_spec))
cat("Testing up to", max_comp, "components\n")

# Use mixOmics perf() function for component selection
set.seed(123)
plsda_cv <- plsda(X = train_spec, Y = train_classes, ncomp = max_comp)

# Cross-validation to find optimal components
cv_results <- perf(plsda_cv, 
                   validation = "Mfold",
                   folds = 5,
                   nrepeat = 10,
                   progressBar = TRUE)

# Plot CV results
pdf("R_output/PLSDA_component_selection.pdf", width = 8, height = 6)
plot(cv_results, main = "PLSDA Component Selection")
dev.off()

# Find optimal components (minimum error rate)
opt_comp <- cv_results$choice.ncomp["BER", "max.dist"]

cat("Optimal number of components:", opt_comp, "\n")





### 4. Final PLSDA Model ###
# Build final model with optimal components
final_plsda <- plsda(X = train_spec, 
                     Y = train_classes, 
                     ncomp = opt_comp)

### 5. Model Validation ###
# Predictions on test set
predictions <- predict(final_plsda, newdata = spec)
preds <- as.data.frame(predictions$class$max.dist)
dim(preds)
predicted_classes <- preds[, opt_comp]

length(treatment_classes)
length(predicted_classes)

# Ensure same factor levels
predicted_classes <- factor(predicted_classes, levels = levels(test_classes))


# Confusion matrix
conf_matrix <- confusionMatrix(predicted_classes, treatment_classes)


### 7. Performance Metrics ###
cat("\n=== Performance Metrics ===\n")
accuracy <- conf_matrix$overall["Accuracy"]
kappa <- conf_matrix$overall["Kappa"]

cat("Overall Accuracy:", round(accuracy, 3), "\n")
cat("Kappa Statistic:", round(kappa, 3), "\n")
cat("95% CI for Accuracy:", round(conf_matrix$overall["AccuracyLower"], 3), 
    "to", round(conf_matrix$overall["AccuracyUpper"], 3), "\n")

# Per-class statistics
cat("\nPer-class Performance:\n")
class_stats <- conf_matrix$byClass
if(is.matrix(class_stats)) {
  for(i in 1:nrow(class_stats)) {
    class_name <- rownames(class_stats)[i]
    sensitivity <- class_stats[i, "Sensitivity"]
    specificity <- class_stats[i, "Specificity"]
    cat(class_name, "- Sensitivity:", round(sensitivity, 3), 
        "Specificity:", round(specificity, 3), "\n")
  }
}

### 8. Visualization ###
# Confusion matrix heatmap
conf_table <- conf_matrix$table
conf_prop <- prop.table(conf_table, 2)  # Proportions by reference


# Create heatmap
#pdf("PLSDA_confusion_matrix.pdf", width = 7, height = 6)

col <- colorRampPalette(c("black","black","brown","gold","forestgreen")) 
corrplot::corrplot(conf_prop, 
                   p.mat = tabs_perc, 
                   insig = "p-value", sig.level = -1, addCoef.col = 1,
                   tl.srt = 70,col = col(20),cl.lim = c(0, 1),tl.col = 1, tl.offset =1.5, 
                   cl.ratio = 0.2, cl.align.text = "l", cl.cex = 0.9, 
                   mar=c(1,3,3,3))
mtext("Prediction",2,at=3, line=-3, cex=1.3)
mtext("Reference",at = 2, line = 0, cex=1.3)

# create output folder
if(!exists(here::here("R_output","PLSDA_output"))){ 
  dir.create(here::here("R_output","PLSDA_output"))}

write.csv(conf_prop, here::here("R_output","PLSDA_output","young_stands_treatment_plsda.csv"))


# 
# 
# ###################################
# #dev.off()
# 
# 
# ### 8. Variable Importance ###
# # Get VIP scores for important wavelengths/variables
# vip_scores <- vip(final_plsda)
# important_vars <- which(vip_scores[, 1] > 1)  # VIP > 1 are considered important
# 
# cat("\n=== Important Variables ===\n")
# cat("Number of important variables (VIP > 1):", length(important_vars), "\n")
# 
# # Plot VIP scores
# pdf("PLSDA_VIP_scores.pdf", width = 10, height = 6)
# plot(vip_scores[, 1], type = "l", 
#      xlab = "Variable Index", 
#      ylab = "VIP Score",
#      main = "Variable Importance in Projection (VIP)")
# abline(h = 1, col = "red", lty = 2)
# points(important_vars, vip_scores[important_vars, 1], col = "red", pch = 19)
# dev.off()
# 
# ### 9. Cross-Validation Assessment ###
# # Perform cross-validation for more robust performance estimate
# set.seed(123)
# cv_results <- perf(final_plsda, validation = "Mfold", 
#                    folds = 10, nrepeat = 10)
# 
# cat("\n=== Cross-Validation Results ===\n")
# cat("CV Error Rate:", round(cv_results$error.rate[pls_model$bestTune$ncomp], 3), "\n")
# 
# # preds <- as.data.frame(predictions$class$max.dist)
# # dim(preds)
# # predicted_classes <- preds[, opt_comp]
# 
# # Plot CV results
# pdf("PLSDA_CV_performance.pdf", width = 8, height = 6)
# plot(cv_results, main = "Cross-Validation Performance")
# dev.off()
# 
# # Create comprehensive results summary
# results_summary <- data.frame(
#   Metric = c("Test_Accuracy", "Test_Kappa", "CV_Accuracy", 
#              "Components_Used", "Important_Variables", "Sample_Size"),
#   Value = c(round(accuracy, 4),
#             round(kappa, 4),
#             round(1 - cv_error, 4),
#             opt_comp,
#             length(important_vars),
#             nrow(train_spec)))
# 
# # Save summary files
# #write.csv(results_summary, "R_output/plsda_results_summary.csv", row.names = FALSE)
# write.csv(conf_matrix$table, "R_output/confusion_matrix_counts.csv")
# write.csv(conf_prop, "R_output/confusion_matrix_proportions.csv")
# 
# # Save VIP scores
# vip_df <- data.frame(
#   Variable_Index = 1:length(vip_scores[, 1]),
#   VIP_Score = vip_scores[, 1],
#   Important = vip_scores[, 1] > 1
# )
# write.csv(vip_df, "R_output/vip_scores.csv", row.names = FALSE)
# #######################################################
# 
