library(here)

# Define age classes
age_classes <- c("Young forest", "Mid-aged forest", "Mature forest")

# Initialize storage
all_treatment_accuracy <- data.frame()

# Loop through each age class
for(age in age_classes) {
  
  # Read confusion matrix
  conf_path <- here::here("R_output", "PLSDA_output", age, 
                          "count_treatment_plsda.csv")
  
  conf_table <- read.csv(conf_path, row.names = 1)
  conf_matrix <- as.matrix(conf_table)
  
  # Calculate per-treatment accuracy
  treatment_accuracy <- diag(conf_matrix) / colSums(conf_matrix) * 100
  
  # Store results
  temp_df <- data.frame(
    Age_Class = age,
    Treatment = names(treatment_accuracy),
    Accuracy_Percent = round(treatment_accuracy, 2)
  )
  
  all_treatment_accuracy <- rbind(all_treatment_accuracy, temp_df)
  
  cat("\n", age, ":\n", sep="")
  print(temp_df)
}

### Calculate Cross-Age-Class Averages ###
cat("\n=== Average Treatment Accuracy Across Age Classes ===\n")

# Aggregate by treatment
library(dplyr)

cross_age_summary <- all_treatment_accuracy %>%
  group_by(Treatment) %>%
  summarise(
    Mean_Accuracy = round(mean(Accuracy_Percent), 2),
    SD_Accuracy = round(sd(Accuracy_Percent), 2),
    Min_Accuracy = round(min(Accuracy_Percent), 2),
    Max_Accuracy = round(max(Accuracy_Percent), 2),
    .groups = 'drop'
  )

print(cross_age_summary)

cross_trt_summary <- all_treatment_accuracy %>%
  group_by(Age_Class) %>%
  summarise(
    Mean_Accuracy = round(mean(Accuracy_Percent), 2),
    SD_Accuracy = round(sd(Accuracy_Percent), 2),
    Min_Accuracy = round(min(Accuracy_Percent), 2),
    Max_Accuracy = round(max(Accuracy_Percent), 2),
    .groups = 'drop'
  )
cross_trt_summary

# Overall average across all treatments and age classes
overall_avg <- mean(all_treatment_accuracy$Accuracy_Percent)
cat("\nOverall Average Accuracy (all treatments, all ages):", 
    round(overall_avg, 2), "%\n")

# Save results
output_dir <- here::here("R_output", "PLSDA_output")
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write.csv(all_treatment_accuracy, 
          file.path(output_dir, "treatment_accuracy_by_age.csv"), 
          row.names = FALSE)

write.csv(cross_trt_summary, 
          file.path(output_dir, "avg_treatment_accuracy_summary.csv"), 
          row.names = FALSE)

write.csv(cross_age_summary, 
          file.path(output_dir, "avg_age_accuracy_summary.csv"), 
          row.names = FALSE)


### Visualization ###
library(ggplot2)

all_treatment_accuracy$Treatment <- factor(all_treatment_accuracy$Treatment, 
                                           levels=c("Control","N","P","NP"))
ggplot(all_treatment_accuracy, 
       aes(x = Treatment, y = Accuracy_Percent, fill = Age_Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 100/length(unique(all_treatment_accuracy$Treatment)), 
             linetype = "dashed", color = "red") +
  labs(title = "Treatment Classification Accuracy by Age Class",
       y = "Accuracy (%)", x = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "treatment_accuracy_comparison.png"), 
       width = 10, height = 6)
