# Read all datasets
o_data <- read.csv(here::here("R_output","PLSDA_output","Mature forest","count_treatment_plsda.csv"))
m_data <- read.csv(here::here("R_output","PLSDA_output","Mid-aged forest","count_treatment_plsda.csv"))
y_data <- read.csv(here::here("R_output","PLSDA_output","Young forest","count_treatment_plsda.csv"))

# Create list of datasets and their corresponding age labels
data_list <- list(
  list(data = y_data, age = "Young"),
  list(data = m_data, age = "Mid-aged"),
  list(data = o_data, age = "Mature")
)

# Initialize list to store results
results_list <- list()

# Loop through each dataset
for(i in seq_along(data_list)) {
  curr_data <- data_list[[i]]$data
  sel_Age <- data_list[[i]]$age
  
  # Extract confusion matrix from dataframe format
  cm_df <- data.frame(
    Control = as.numeric(curr_data[1,2:5]), 
    N = as.numeric(curr_data[2,2:5]), 
    P = as.numeric(curr_data[4,2:5]),
    NP = as.numeric(curr_data[3,2:5])
  )
  rownames(cm_df) <- c("Control", "N", "P", "NP")
  
  # Convert to matrix for calculations
  cm <- as.matrix(cm_df)
  
  # Overall Accuracy
  overall_acc <- sum(diag(cm)) / sum(cm)
  
  # Producer's Accuracy (Sensitivity/Recall) for each class
  prod_acc <- diag(cm) / rowSums(cm)
  
  # User's Accuracy (Precision) for each class  
  user_acc <- diag(cm) / colSums(cm)
  
  # Type I Error (False Positive Rate) for each class
  type1_error <- (colSums(cm) - diag(cm)) / (sum(cm) - rowSums(cm))
  
  # Type II Error (False Negative Rate) for each class
  type2_error <- (rowSums(cm) - diag(cm)) / rowSums(cm)
  
  # Create results dataframe
  res <- data.frame(
    Age = sel_Age,
    producers_acc = round(prod_acc, 3),
    users_acc = round(user_acc, 3),
    sensitivity = round(type1_error, 3),
    specificity = round(type2_error, 3),
    Treat_class = rownames(cm)
  )
  
  # Store in results list
  results_list[[sel_Age]] <- res
}

# Access individual results
young_res <- results_list[["Young"]]
mid_res <- results_list[["Mid-aged"]]
mature_res <- results_list[["Mature"]]

# Or combine all results into one dataframe
all_results <- do.call(rbind, results_list)

write.csv(all_results, file="3_age_class_PLSDA_metrics.csv")
