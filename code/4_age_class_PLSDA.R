library(caret)
library(mixOmics)
library(corrplot)
library(MLmetrics)

# Select age group
sel_stand_age <- "Young forest"

dati <- read.csv(here::here( "data_folder","processed_spectra.csv"))

min(table(dati$Treatment, dati$Stand))

# Provide the Age column  
dati$Age[dati$Stand=="C1"]<-"Young forest"
dati$Age[dati$Stand=="C2"]<-"Young forest"
dati$Age[dati$Stand=="C3"]<-"Young forest"
dati$Age[dati$Stand=="C4"]<-"Mid-aged forest"
dati$Age[dati$Stand=="C5"]<-"Mid-aged forest"
dati$Age[dati$Stand=="C6"]<-"Mid-aged forest" 
dati$Age[dati$Stand=="C7"]<-"Mature forest"
dati$Age[dati$Stand=="C8"]<-"Mature forest"
dati$Age[dati$Stand=="C9"]<-"Mature forest"

## Select Stand age
dati <- dati[dati$Age==sel_stand_age ,]

## Count tops
count_tops <- as.data.frame(table(dati$Treatment, dati$Stand, dati$Age))
count_tops <- count_tops[count_tops$Freq>0,]
print(count_tops)

train_min_75 <- ceiling(min(count_tops$Freq) * .75)
train_min_75

### 1. Data Splitting ###

# Initialize empty data frames
out_train_data <- data.frame()
out_test_data <- data.frame()

dati$statr <- paste(dati$Stand, dati$Treatment)

# Get unique plots
plots <- unique(dati[, "statr"])

# Process each plot
for(plot in plots) {
  
  plot_data <- dati[dati$statr == plot, ]
  
  
######
  
  # Randomly sample indices for this plot
  train_indices <- sample(1:nrow(plot_data), train_min_75, replace = FALSE)
  
  # Add to train and test sets 
  out_train_data <- rbind(out_train_data, plot_data[train_indices,] )
  out_test_data <- rbind(out_test_data,  plot_data[-train_indices,] )
  
  # Print summary for this plot
  cat(sprintf("Plot %s: Minumum=%d, Train=%d, Test=%d ✓\n", 
              plot, train_min_75, length(train_indices), nrow(plot_data) - length(train_indices)))
}

table(out_train_data$Stand, out_train_data$Treatment)
table(out_test_data$Stand, out_test_data$Treatment)

### 2. Data clean up - CRITICAL: Track complete cases
# Get complete cases BEFORE selecting columns
train_complete <- complete.cases(out_train_data[,7:351])
test_complete <- complete.cases(out_test_data[,7:351])

# Apply complete cases filter to both spectral data AND classes
train_spec <- out_train_data[train_complete, 7:351]
train_classes <- out_train_data$Treatment[train_complete]

test_spec <- out_test_data[test_complete, 7:351]
test_classes <- out_test_data$Treatment[test_complete]

cat("\nAfter removing incomplete cases:\n")
cat("Train:", nrow(train_spec), "\n")
cat("Test:", nrow(test_spec), "\n")

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
                   nrepeat = 20,
                   progressBar = TRUE)

# Plot CV results
plot(cv_results, main = "PLSDA Component Selection")

# Find optimal components (minimum error rate)
opt_comp <- cv_results$choice.ncomp["BER", "max.dist"]
cat("Optimal number of components:", opt_comp, "\n")

### 4. Final PLSDA Model ###
# Build final model with optimal components
final_plsda <- plsda(X = train_spec, 
                     Y = train_classes, 
                     ncomp = opt_comp)

### 5. Model Validation on Test Set ###

predictions <- predict(final_plsda, newdata = test_spec)
preds <- as.data.frame(predictions$class$max.dist)
predicted_classes <- preds[, opt_comp]

# Ensure same factor levels
predicted_classes <- factor(predicted_classes, levels = levels(factor(train_classes)))
test_classes <- factor(test_classes, levels = levels(factor(test_classes)))

cat("\nPrediction dimensions check:\n")
cat("Predicted classes:", length(predicted_classes), "\n")
cat("Test classes:", length(test_classes), "\n")

# Confusion matrix
conf_matrix <- confusionMatrix(predicted_classes, test_classes)
print(conf_matrix)

### 6. Performance Metrics ###
cat("\n=== Performance Metrics ===\n")
accuracy <- conf_matrix$overall["Accuracy"]
kappa <- conf_matrix$overall["Kappa"]

cat("Overall Accuracy:", round(accuracy, 3), "\n")
cat("Kappa Statistic:", round(kappa, 3), "\n")
cat("95% CI for Accuracy:", round(conf_matrix$overall["AccuracyLower"], 3), 
    "to", round(conf_matrix$overall["AccuracyUpper"], 3), "\n")

### After the perf() call for component selection ###

# Get the error rates for all repeats
error_rates <- cv_results$error.rate$BER[opt_comp, ]

# Calculate accuracy from error rates
accuracies <- 1 - error_rates

# Calculate mean and SD
mean_accuracy <- mean(accuracies)
sd_accuracy <- sd(accuracies)

cat("\n=== Cross-Validation Accuracy (", cv_results$nrepeat, " repeats) ===\n", sep="")
cat("Accuracy: ", round(mean_accuracy, 4), " ± ", round(sd_accuracy, 4), "\n", sep="")


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

### 7. Confusion Matrix Tables ###
conf_table <- conf_matrix$table
conf_prop <- prop.table(conf_table, 2)  # Proportions by reference

### 8. Variable Importance ###
vip_scores <- vip(final_plsda)
important_vars <- which(vip_scores[, 1] > 1)

cat("\n=== Important Variables ===\n")
cat("Number of important variables (VIP > 1):", length(important_vars), "\n")

# Plot VIP scores
plot(vip_scores[, 1], type = "l",
     xlab = "Variable Index",
     ylab = "VIP Score",
     main = "Variable Importance in Projection (VIP)")
abline(h = 1, col = "red", lty = 2)

### 9. Cross-Validation Assessment ###
set.seed(123)
cv_performance <- perf(final_plsda, validation = "Mfold",
                       folds = 10, nrepeat = 10)

plot(cv_performance, main = "Cross-Validation Performance")

### 10. Final Summary ###
cat("\n=== Cross-Validation Summary ===\n")
cv_error <- cv_performance$error.rate$BER[, "centroids.dist"][opt_comp]

cat("Cross-validation error rate:", round(cv_error, 3), "\n")
cat("Cross-validation accuracy:", round(1 - cv_error, 3), "\n")

# Create comprehensive results summary
results_summary <- data.frame(
  Metric = c("Test_Accuracy", "Test_Kappa", 
             "CV_Accuracy_Mean", "CV_Accuracy_SD",
             "Components_Used", "Important_Variables", 
             "Train_Sample_Size", "Test_Sample_Size"),
  Value = c(round(accuracy, 4),
            round(kappa, 4),
            round(mean_accuracy, 4),
            round(sd_accuracy, 4),
            opt_comp,
            length(important_vars),
            nrow(train_spec),
            nrow(test_spec)))

print(results_summary)

# Save VIP scores
vip_df <- data.frame(
  Variable_Index = 1:length(vip_scores[, 1]),
  VIP_Score = vip_scores[, 1],
  Important = vip_scores[, 1] > 1)

# Save outputs
plsda_out <- here::here("R_output","PLSDA_output", sel_stand_age)

if(!dir.exists(plsda_out)){ 
  dir.create(plsda_out, recursive = TRUE)
}

results_summary


write.csv(conf_prop, file.path(plsda_out,"prop_treatment_plsda.csv"))
write.csv(conf_table, file.path(plsda_out,"count_treatment_plsda.csv"))
write.csv(results_summary, file.path(plsda_out,"results_summary_plsda.csv"), row.names = FALSE)
write.csv(vip_df, file.path(plsda_out,"vip_scores.csv"), row.names = FALSE)

cat("\n=== Analysis Complete ===\n")
cat("Results saved to:", plsda_out, "\n")
