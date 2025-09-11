# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV
# Read proportion data
o_data <- read.csv(here::here("R_output","PLSDA_output_August","Mature forest","prop_treatment_plsda.csv"))
m_data <- read.csv(here::here("R_output","PLSDA_output_August","Mid-aged forest","prop_treatment_plsda.csv"))
y_data <- read.csv(here::here("R_output","PLSDA_output_August","Young forest","prop_treatment_plsda.csv"))

# Read count data
o_c_data <- read.csv(here::here("R_output","PLSDA_output_August","Mature forest","count_treatment_plsda.csv"))
m_c_data <- read.csv(here::here("R_output","PLSDA_output_August","Mid-aged forest","count_treatment_plsda.csv"))
y_c_data <- read.csv(here::here("R_output","PLSDA_output_August","Young forest","count_treatment_plsda.csv"))

# Convert proportion data to long format for ggplot
o_long <- o_data[, 2:5] %>%
  mutate(Prediction = o_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

m_long <- m_data[, 2:5] %>%
  mutate(Prediction = m_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

y_long <- y_data[, 2:5] %>%
  mutate(Prediction = y_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

# Convert count data to long format for ggplot
o_c_long <- o_c_data[, 2:5] %>%
  mutate(Prediction = o_c_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Count")

m_c_long <- m_c_data[, 2:5] %>%
  mutate(Prediction = m_c_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Count")

y_c_long <- y_c_data[, 2:5] %>%
  mutate(Prediction = y_c_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Count")

# Add age labels to proportion data
o_long$Age <- "Mature forest"
m_long$Age <- "Mid-aged forest"
y_long$Age <- "Young forest"

# Add age labels to count data
o_c_long$Age <- "Mature forest"
m_c_long$Age <- "Mid-aged forest"
y_c_long$Age <- "Young forest"

# Combine proportion and count data
conf_prop_data <- rbind(o_long, m_long, y_long)
conf_count_data <- rbind(o_c_long, m_c_long, y_c_long)

# Set factor levels for proportion data
conf_prop_data$Age <- factor(conf_prop_data$Age, levels = c("Young forest","Mid-aged forest","Mature forest"))
conf_prop_data$Reference <- factor(conf_prop_data$Reference, levels = c("Control","N","P","NP"))
conf_prop_data$Prediction <- factor(conf_prop_data$Prediction, levels = c("NP","P","N","Control"))

# Set factor levels for count data (same levels)
conf_count_data$Age <- factor(conf_count_data$Age, levels = c("Young forest","Mid-aged forest","Mature forest"))
conf_count_data$Reference <- factor(conf_count_data$Reference, levels = c("Control","N","P","NP"))
conf_count_data$Prediction <- factor(conf_count_data$Prediction, levels = c("NP","P","N","Control"))

# Merge proportion and count data for plotting
conf_data <- merge(conf_prop_data, conf_count_data, 
                   by = c("Prediction", "Reference", "Age"))

# Create custom color palette
col <- colorRampPalette(c("black","black","brown","gold","forestgreen"))

# Create ggplot confusion matrix heatmap
ggplot(conf_data, aes(x = Reference, y = Prediction, fill = Proportion)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = Count), 
            color = "white", size = 4, fontface = "bold") +
  scale_fill_gradientn(colors = col(20),
                       name = "Proportion\nof classes",
                       limits = c(0, 1)) +
  labs(title = "PLSDA Confusion Matrix",
       subtitle = "Nutrient Treatment Classification (Count values shown)",
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
  facet_wrap(~Age, nrow = 1)
