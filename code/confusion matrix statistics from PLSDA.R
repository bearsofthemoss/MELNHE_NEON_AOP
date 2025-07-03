### PLSDA accuracy statements (counts)


library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV

o_data <- read.csv(here::here("R_output","PLSDA_output","Mature forest","count_treatment_plsda.csv"))
m_data <- read.csv(here::here("R_output","PLSDA_output","Mid-aged forest","count_treatment_plsda.csv"))
y_data <- read.csv(here::here("R_output","PLSDA_output","Young forest","count_treatment_plsda.csv"))


out_conf <- list()

for( i in c(1:3)){

sel_Age <- "Young forest"  #  "Mature forest" and/or "Mid-aged forest"

# Extract confusion matrix from your dataframe format
cm_df <- data.frame(
  Control = as.numeric(data[1,2:5]), 
  N = as.numeric(data[2,2:5]), 
  P = as.numeric(data[4,2:5]),
  NP = as.numeric(data[3,2:5]))

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




res <- data.frame(
  Age = sel_Age,
  Overall = round(overall_acc, 5),
  producers_acc = round(prod_acc, 3),
  users_acc = round(user_acc, 3),
  sensitivity = round(type1_error, 3),
  specificity = round(type2_error, 3),
  Treat_class = rownames(cm)
)

out_conf <- rbind( res, out_conf) 

}

out_conf
