library(caret)
library(mixOmics)
library(corrplot)
library(MLmetrics)


# Age PLSDA 

#  sel_stand_age <- "Mature forest"
# # ## Select Stand age
#  dati <- dati[dati$Age==sel_stand_age ,]
# dim(dati)
# do with stands instead


#for(i in c(1:9)){
  
  dati <- read.csv("./data_folder/actual_tops.csv", row.names = 1)
  dati$Age[dati$Stand=="C1"]<-"Young forest"
  dati$Age[dati$Stand=="C2"]<-"Young forest"
  dati$Age[dati$Stand=="C3"]<-"Young forest"
  dati$Age[dati$Stand=="C4"]<-"Mid-aged forest"
  dati$Age[dati$Stand=="C5"]<-"Mid-aged forest"
  dati$Age[dati$Stand=="C6"]<-"Mid-aged forest" 
  dati$Age[dati$Stand=="C7"]<-"Mature forest"
  dati$Age[dati$Stand=="C8"]<-"Mature forest"
  dati$Age[dati$Stand=="C9"]<-"Mature forest"
  
  # Add in the 4X3 grid of age and nutrient addition
  dati$age_nutrient_class <- as.factor(paste(dati$Age, dati$Treatment))
  
  count_tops <- as.data.frame(table(dati$Treatment, dati$Stand))
  count_tops
  
  
  
  
  ### 1. Data Splitting ###

############    Sep 2025 edits- equal draws from each category
  n_train = 10
  n_test = 6
  
  # Initialize empty data frames
  out_train_data <- data.frame()
  out_test_data <- data.frame()
  
  out_train_classes<- data.frame()
  out_test_classes<- data.frame()
  
  dati$statr <- paste(dati$Stand, dati$Treatment)
  
  # Get unique plots
  plots <- unique(dati[, "statr"])

  
  # Process each plot
  for(plot in plots) {
    
    plot_data <- dati[dati$statr == plot, ]  # Fixed: was using 'data' instead of 'dati'
    
    # Show available data for this plot
    available_count <- nrow(plot_data)
    
    # Check if plot has enough data points
    # if(available_count < (n_train + n_test)) {
    #   cat(sprintf("Plot %s: SKIPPED - only %d points available, need %d\n", 
    #               plot, available_count, n_train + n_test))
    #   
    # 
    #   next
    # }
    
    # Randomly sample indices for this plot
    total_needed <- n_train + n_test
    sampled_indices <- sample(1:nrow(plot_data), total_needed, replace = FALSE)
    
    # Split into train and test
    train_indices <- sampled_indices[1:n_train]
    test_indices <- sampled_indices[(n_train + 1):total_needed]
    
    # Add to train and test sets (Fixed: was using wrong variables)
    out_train_data <- rbind(out_train_data, plot_data[train_indices, ])
    out_test_data <- rbind(out_test_data, plot_data[test_indices, ])
    
    out_train_classes <- rbind(out_train_classes, plot_data[,"age_nutrient_class" ])
    out_test_classes <- rbind(out_test_classes, plot_data[, "age_nutrient_class"])

        
    # Print summary for this plot
    cat(sprintf("Plot %s: Available=%d, Train=%d, Test=%d ✓\n", 
                plot, available_count, length(train_indices), length(test_indices)))

  }
  
  dim(out_train_data)
  dim(out_test_data)  

  # select just the spectral columns
  train_spec <- out_train_data[,c(7:351)] 
  test_spec <- out_test_data[,c(7:351)] 


  train_classes <- out_train_data$age_nutrient_class
  test_classes <- out_test_data$age_nutrient_class
    
  
  ### 2. Data clean up

  train_spec <- train_spec[complete.cases(train_spec),]
  test_spec <- test_spec[complete.cases(test_spec),]
  
    
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
  #pdf("R_output/PLSDA_component_selection.pdf", width = 8, height = 6)
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
  
  spec <- dati[complete.cases(dati), 7:351] 
  
  predictions <- predict(final_plsda, newdata = spec)
  preds <- as.data.frame(predictions$class$max.dist)
  dim(preds)
  predicted_classes <- preds[, opt_comp]
  
  length(age_classes)
  length(predicted_classes)
  
  # Ensure same factor levels
  predicted_classes <- factor(predicted_classes, levels = levels(test_classes))
  
  
  # Confusion matrix
  conf_matrix <- confusionMatrix(predicted_classes, age_classes)
  
  
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
  
  
  ### 8. Variable Importance ###
  # Get VIP scores for important wavelengths/variables
  vip_scores <- vip(final_plsda)
  important_vars <- which(vip_scores[, 1] > 1)  # VIP > 1 are considered important
  
  cat("\n=== Important Variables ===\n")
  cat("Number of important variables (VIP > 1):", length(important_vars), "\n")
  
  # Plot VIP scores
  #pdf("PLSDA_VIP_scores.pdf", width = 10, height = 6)
  plot(vip_scores[, 1], type = "l",
       xlab = "Variable Index",
       ylab = "VIP Score",
       main = "Variable Importance in Projection (VIP)")
  abline(h = 1, col = "red", lty = 2)
  #points(important_vars, vip_scores[important_vars, 1], col = "red", pch = 19)
  #dev.off()
  
  
  
  
  
  ### 9. Cross-Validation Assessment ###
  # Perform cross-validation for more robust performance estimate
  set.seed(123)
  cv_results <- perf(final_plsda, validation = "Mfold",
                     folds = 10, nrepeat = 10)
  
  
  preds <- as.data.frame(predictions$class$max.dist)
  dim(preds)
  predicted_classes <- preds[, opt_comp]
  
  # Plot CV results
  #pdf("PLSDA_CV_performance.pdf", width = 8, height = 6)
  plot(cv_results, main = "Cross-Validation Performance")
  #dev.off()
  
  ### 11. Final Cross-Validation Summary ###
  cat("\n=== Cross-Validation Summary ===\n")
  cv_error <- cv_results$error.rate$BER[, "centroids.dist"][opt_comp]
  
  cat("Cross-validation error rate:", round(cv_error, 3), "\n")
  cat("Cross-validation accuracy:", round(1 - cv_error, 3), "\n")
  
  
  
  # Create comprehensive results summary
  results_summary <- data.frame(
    Metric = c("Test_Accuracy", "Test_Kappa", "CV_Accuracy",
               "Components_Used", "Important_Variables", "Sample_Size"),
    Value = c(round(accuracy, 4),
              round(kappa, 4),
              round(1 - cv_error[1], 4),
              opt_comp,
              length(important_vars),
              nrow(train_spec)))
  
  
  # Save VIP scores
  vip_df <- data.frame(
    Variable_Index = 1:length(vip_scores[, 1]),
    VIP_Score = vip_scores[, 1],
    Important = vip_scores[, 1] > 1)
  
  
  # create output folder
  # if(!exists(here::here("R_output","PLSDA_output"))){ 
  #   dir.create(here::here("R_output","PLSDA_output"))}
  
  # Save summary files
  
  # make age-specific folder
  #plsda_out <- here::here("R_output","PLSDA_output",sel_stand_age)
  plsda_out <- here::here("R_output","PLSDA_output_September")
  
  if(!exists(plsda_out )){ 
    dir.create(plsda_out)}else {}
  
  write.csv(conf_prop,file.path(plsda_out,"prop_treatment_plsda.csv"))
  write.csv(conf_table, file.path(plsda_out,"count_treatment_plsda.csv"))
  write.csv(results_summary, file.path(plsda_out,"results_summary_plsda.csv"))
  write.csv(vip_df, file.path(plsda_out,"vip_scores.csv"), row.names = FALSE)
  
  #######################################################

  