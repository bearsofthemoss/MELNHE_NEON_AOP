# Define age groups and file paths
age_groups <- c("Young forest", "Mid-aged forest", "Mature forest")

# determine file paths
file_paths <- c(
  here::here("R_output","PLSDA_output","Young forest","count_treatment_plsda.csv"),
  here::here("R_output","PLSDA_output","Mid-aged forest","count_treatment_plsda.csv"),
  here::here("R_output","PLSDA_output","Mature forest","count_treatment_plsda.csv")
)

out_conf <- list()

for(i in 1:3){
  # Read data for current age group
  data <- read.csv(file_paths[i])
  sel_Age <- age_groups[i]
  
  # Extract confusion matrix from your dataframe format
  cm_df <- data.frame(
    Control = as.numeric(data[1,2:5]), 
    N = as.numeric(data[2,2:5]), 
    P = as.numeric(data[4,2:5]),
    NP = as.numeric(data[3,2:5])
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
  
  # Store results
  res <- data.frame(
    Age = sel_Age,
    Overall = round(overall_acc, 5),
    Producers_accuracy = round(prod_acc, 3),
    Users_accuracy = round(user_acc, 3),
    type1_error = round(type1_error, 3),
    type2_error = round(type2_error, 3),
    Treat_class = rownames(cm)
  )
  
  out_conf[[i]] <- res
}

# Combine all results
final_results <- do.call(rbind, out_conf)


trt_acc <- tidyr::gather(final_results[ ,c(1,3,4,7 )], "acc_type","accuracy",2:3 )
unique(final_results[ , c("Age","Overall")])

trt_acc$Age <- factor(trt_acc$Age, levels=c("Young forest","Mid-aged forest","Mature forest"))

trt_acc$Treat_class <- factor(trt_acc$Treat_class, levels=c("Control","N","P","NP"))

ggplot(trt_acc, aes(x=Treat_class, y=accuracy, , fill=Treat_class, shape = acc_type))+
  geom_col(position=position_dodge(), aes(col = acc_type), linewidth = 2)+
  scale_fill_manual(values=c("black","blue","red","purple"))+
  facet_wrap(~ Age, nrow=1)+
  theme_bw()+theme(panel.grid = element_blank())+
  labs(x="Nutrient addition treatment", y="PLSDA model accuracy",
       fill="", col="")
