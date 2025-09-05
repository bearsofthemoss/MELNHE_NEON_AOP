# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV

## all 12 in one
data <- read.csv(here::here("R_output","PLSDA_output_September","count_treatment_plsda.csv"))


## all 12 in one
prop <- read.csv(here::here("R_output","PLSDA_output_September","prop_treatment_plsda.csv"))

data$X

dim(data)
# Convert to long format for ggplot
# Modern approach using tidyr::pivot_longer()
all_long <- data[, 2:13] %>%
  mutate(Prediction = data$X) %>%
  tidyr::pivot_longer(
    cols = -Prediction,           # all columns except Prediction
    names_to = "Reference",       # column names go to "Reference" 
    values_to = "Count"          # values go to "Count"
  )
all_prop <- prop[, 2:13] %>%
  mutate(Prediction = data$X) %>%
  tidyr::pivot_longer(
    cols = -Prediction,           # all columns except Prediction
    names_to = "Reference",       # column names go to "Reference" 
    values_to = "Proportion"      # values go to "Proportion"
  )

desired_order <- c(
  # Young age class
  "Young forest Control", "Young forest N", "Young forest P", "Young forest NP",
  # Mid age class  
  "Mid-aged forest Control", "Mid-aged forest N", "Mid-aged forest P", "Mid-aged forest NP",
  # Mature age class
  "Mature forest Control", "Mature forest N", "Mature forest P", "Mature forest NP"
)

# get rid of . in prediction classes
all_long$Reference <- gsub(".", " ", all_long$Reference , fixed=TRUE)
all_prop$Reference <- gsub(".", " ", all_prop$Reference , fixed=TRUE)

# keep mid-aged
all_long$Reference <- gsub("Mid aged", "Mid-aged", all_long$Reference)   
all_long$Prediction <- gsub("Mid aged", "Mid-aged", all_long$Prediction) 

all_long$Reference <- factor(all_long$Reference, levels= desired_order)
all_long$Prediction <- factor(all_long$Prediction, levels= desired_order)

# Do again for prop
all_prop$Reference <- gsub("Mid aged", "Mid-aged", all_prop$Reference)   
all_prop$Prediction <- gsub("Mid aged", "Mid-aged", all_prop$Prediction) 

all_prop$Reference <- factor(all_prop$Reference, levels= desired_order)
all_prop$Prediction <- factor(all_prop$Prediction, levels= desired_order)

## Add in the proportion from the PLSDA, to have count and proportion of pixels

all_long$key <- paste(all_long$Prediction, all_long$Reference)
all_prop$key <- paste(all_prop$Prediction, all_prop$Reference)


all_long$prop <- all_prop$Proportion[match(all_long$key, all_prop$key)]


# Create custom color palette
col <- colorRampPalette(c("black","brown","gold","forestgreen"))

#all_long$Proportion <- round(all_long$Proportion, 2)

# Create ggplot confusion matrix heatmap
ggplot(all_long, aes(x = Reference, y = Prediction, fill = prop)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = Count), 
            color = "white",  fontface = "bold") +
  scale_fill_gradientn(colors = col(20),
                       name = "Proportion \nof classes",
                       limits = c(0, 1)) +
  labs(
       x = "Reference class",
       y = "Predicted class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid = element_blank()) +
  coord_fixed()



# Save the plot
#ggsave("ggplot_confusion_matrix.pdf", width = 8, height = 6, dpi = 300)


# Assuming your confusion matrix is called 'confusion_matrix' and ordered as above
confusion_matrix <- all_long

# Define age class groupings
young_classes <- c("Young forest Control", "Young forest N", "Young forest P", "Young forest NP")
mid_classes <- c("Mid-aged forest Control", "Mid-aged forest N", "Mid-aged forest P", "Mid-aged forest NP")
mature_classes <- c("Mature forest Control", "Mature forest N", "Mature forest P", "Mature forest NP")


# Calculate Young forest accuracies
young_tp <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% young_classes & 
                                         confusion_matrix$Prediction %in% young_classes])
young_total_actual <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% young_classes])
young_total_predicted <- sum(confusion_matrix$Count[confusion_matrix$Prediction %in% young_classes])
young_producer_acc <- young_tp / young_total_actual
young_user_acc <- young_tp / young_total_predicted

# Calculate Mid-aged forest accuracies  
mid_tp <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% mid_classes & 
                                       confusion_matrix$Prediction %in% mid_classes])
mid_total_actual <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% mid_classes])
mid_total_predicted <- sum(confusion_matrix$Count[confusion_matrix$Prediction %in% mid_classes])
mid_producer_acc <- mid_tp / mid_total_actual
mid_user_acc <- mid_tp / mid_total_predicted

# Calculate Mature forest accuracies
mature_tp <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% mature_classes & 
                                          confusion_matrix$Prediction %in% mature_classes])
mature_total_actual <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% mature_classes])
mature_total_predicted <- sum(confusion_matrix$Count[confusion_matrix$Prediction %in% mature_classes])
mature_producer_acc <- mature_tp / mature_total_actual
mature_user_acc <- mature_tp / mature_total_predicted

# Create summary table
age_accuracy_summary <- data.frame(
  Treatment = c("Young", "Mid-aged", "Mature"),
  Producer_Accuracy = c(young_producer_acc, mid_producer_acc, mature_producer_acc),
  User_Accuracy = c(young_user_acc, mid_user_acc, mature_user_acc),
  Total_Actual = c(young_total_actual, mid_total_actual, mature_total_actual),
  Total_Predicted = c(young_total_predicted, mid_total_predicted, mature_total_predicted)
)

# Format as percentages
age_accuracy_summary$Sensitivity <- round(age_accuracy_summary$Producer_Accuracy * 100, 2)
age_accuracy_summary$Precision <- round(age_accuracy_summary$User_Accuracy * 100, 2)

age_accuracy_summary

#######################################

## Now calculate nutrients over all

# Define nutrient class groupings
control_classes <- c("Young forest Control", "Mid-aged forest Control", "Mature forest Control")
n_classes <- c("Young forest N", "Mid-aged forest N", "Mature forest N")
p_classes <- c("Young forest P", "Mid-aged forest P", "Mature forest P")
np_classes <- c("Young forest NP", "Mid-aged forest NP", "Mature forest NP")

# Calculate Control accuracies
control_tp <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% control_classes & 
                                           confusion_matrix$Prediction %in% control_classes])
control_total_actual <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% control_classes])
control_total_predicted <- sum(confusion_matrix$Count[confusion_matrix$Prediction %in% control_classes])
control_producer_acc <- control_tp / control_total_actual
control_user_acc <- control_tp / control_total_predicted

# Calculate N treatment accuracies  
n_tp <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% n_classes & 
                                     confusion_matrix$Prediction %in% n_classes])
n_total_actual <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% n_classes])
n_total_predicted <- sum(confusion_matrix$Count[confusion_matrix$Prediction %in% n_classes])
n_producer_acc <- n_tp / n_total_actual
n_user_acc <- n_tp / n_total_predicted

# Calculate P treatment accuracies
p_tp <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% p_classes & 
                                     confusion_matrix$Prediction %in% p_classes])
p_total_actual <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% p_classes])
p_total_predicted <- sum(confusion_matrix$Count[confusion_matrix$Prediction %in% p_classes])
p_producer_acc <- p_tp / p_total_actual
p_user_acc <- p_tp / p_total_predicted

# Calculate NP treatment accuracies
np_tp <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% np_classes & 
                                      confusion_matrix$Prediction %in% np_classes])
np_total_actual <- sum(confusion_matrix$Count[confusion_matrix$Reference %in% np_classes])
np_total_predicted <- sum(confusion_matrix$Count[confusion_matrix$Prediction %in% np_classes])
np_producer_acc <- np_tp / np_total_actual
np_user_acc <- np_tp / np_total_predicted

age_accuracy_summary
# Create summary table
nutrient_accuracy_summary <- data.frame(
  Treatment = c("Control", "N", "P", "NP"),
  Producer_Accuracy = c(control_producer_acc, n_producer_acc, p_producer_acc, np_producer_acc),
  User_Accuracy = c(control_user_acc, n_user_acc, p_user_acc, np_user_acc),
  Total_Actual = c(control_total_actual, n_total_actual, p_total_actual, np_total_actual),
  Total_Predicted = c(control_total_predicted, n_total_predicted, p_total_predicted, np_total_predicted)
)
# Format as percentages
nutrient_accuracy_summary$Sensitivity <- round(nutrient_accuracy_summary$Producer_Accuracy * 100, 2)
nutrient_accuracy_summary$Precision <- round(nutrient_accuracy_summary$User_Accuracy * 100, 2)

nutrient_accuracy_summary

age_accuracy_summary

a <- rbind(nutrient_accuracy_summary, age_accuracy_summary)

b <- tidyr::gather(a, "type","value", 6:7)

b$Treatment <- factor(b$Treatment, levels=c("Control","N","P","NP","Young","Mid-aged","Mature"))

# Create the plot with improvements
ggplot(b, aes(x = Treatment, y = value, fill = Treatment)) +
  facet_wrap(~type, nrow = 1) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "Classification Performance: Nutrient Treatments and Age Classes",
    x = "Treatment/Age Class",
    y = "Accuracy (%)",
    fill = "Group"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_fill_manual(values=c("black","blue","red","purple","darkseagreen","forestgreen","darkolivegreen"))
  scale_y_continuous(limits = c(0, 100))

