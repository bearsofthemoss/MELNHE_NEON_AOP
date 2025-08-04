# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV
# Read all C datasets
C1_data <- read.csv(here::here("R_output","PLSDA_output","C1","prop_treatment_plsda.csv"))
C2_data <- read.csv(here::here("R_output","PLSDA_output","C2","prop_treatment_plsda.csv"))
C3_data <- read.csv(here::here("R_output","PLSDA_output","C3","prop_treatment_plsda.csv"))
C4_data <- read.csv(here::here("R_output","PLSDA_output","C4","prop_treatment_plsda.csv"))
C5_data <- read.csv(here::here("R_output","PLSDA_output","C5","prop_treatment_plsda.csv"))
C6_data <- read.csv(here::here("R_output","PLSDA_output","C6","prop_treatment_plsda.csv"))
C7_data <- read.csv(here::here("R_output","PLSDA_output","C7","prop_treatment_plsda.csv"))
C8_data <- read.csv(here::here("R_output","PLSDA_output","C8","prop_treatment_plsda.csv"))
C9_data <- read.csv(here::here("R_output","PLSDA_output","C9","prop_treatment_plsda.csv"))

# Convert to long format for ggplot
C1_long <- C1_data[, 2:5] %>%
  mutate(Prediction = C1_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

C2_long <- C2_data[, 2:5] %>%
  mutate(Prediction = C2_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

C3_long <- C3_data[, 2:5] %>%
  mutate(Prediction = C3_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

C4_long <- C4_data[, 2:5] %>%
  mutate(Prediction = C4_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

C5_long <- C5_data[, 2:5] %>%
  mutate(Prediction = C5_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

C6_long <- C6_data[, 2:5] %>%
  mutate(Prediction = C6_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

C7_long <- C7_data[, 2:5] %>%
  mutate(Prediction = C7_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

C8_long <- C8_data[, 2:5] %>%
  mutate(Prediction = C8_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

C9_long <- C9_data[, 2:5] %>%
  mutate(Prediction = C9_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

# Add category labels
C1_long$Category <- "C1"
C2_long$Category <- "C2"
C3_long$Category <- "C3"
C4_long$Category <- "C4"
C5_long$Category <- "C5"
C6_long$Category <- "C6"
C7_long$Category <- "C7"
C8_long$Category <- "C8"
C9_long$Category <- "C9"

# Combine all data
conf_data <- rbind(C1_long, C2_long, C3_long, C4_long, C5_long, 
                   C6_long, C7_long, C8_long, C9_long)

# Set factor levels
conf_data$Category <- factor(conf_data$Category, levels = c("C1","C2","C3","C4","C5","C6","C7","C8","C9"))
conf_data$Reference <- factor(conf_data$Reference, levels = c("Control","N","P","NP"))
conf_data$Prediction <- factor(conf_data$Prediction, levels = c("NP","P","N","Control"))

# Create custom color palette
col <- colorRampPalette(c("black","black","brown","gold","forestgreen"))

# Create ggplot confusion matrix heatmap
ggplot(conf_data, aes(x = Reference, y = Prediction, fill = Proportion)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(Proportion, 2)), 
            color = "white", size = 4, fontface = "bold") +
  scale_fill_gradientn(colors = col(20),
                       name = "Proportion\nof classes",
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
  coord_fixed() +
  facet_wrap(~Category, nrow = 3)


# Save the plot
ggsave("ggplot_stand_level_confusion_matrix.pdf", width = 8, height = 6, dpi = 300)
