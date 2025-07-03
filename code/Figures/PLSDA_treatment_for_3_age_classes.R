# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV

o_data <- read.csv(here::here("R_output","PLSDA_output","Mature forest","prop_treatment_plsda.csv"))
m_data <- read.csv(here::here("R_output","PLSDA_output","Mid-aged forest","prop_treatment_plsda.csv"))
y_data <- read.csv(here::here("R_output","PLSDA_output","Young forest","prop_treatment_plsda.csv"))


# Convert to long format for ggplot
o_long <- o_data[ ,2:5] %>%
  mutate(Prediction = o_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

m_long <- m_data[ ,2:5] %>%
  mutate(Prediction = m_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")

y_long <- y_data[ ,2:5] %>%
  mutate(Prediction = y_data$X) %>%
  melt(id.vars = "Prediction", 
       variable.name = "Reference", 
       value.name = "Proportion")



o_long$Age <- "Mature forest"
m_long$Age <- "Mid-aged forest"
y_long$Age <- "Young forest"


conf_data <- rbind(o_long, m_long, y_long)

conf_data$Age <- factor(conf_data$Age, levels=c("Young forest","Mid-aged forest","Mature forest"))

conf_data$Reference <- factor(conf_data$Reference, levels=c("Control","N","P","NP"))
conf_data$Prediction <- factor(conf_data$Prediction, levels=c("NP","P","N","Control"))

# fan the plot be facet_wrapped by the 3 age classes?

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
  labs(title = "PLSDA Confusion Matrix",
       subtitle = "Nutrient Treatment Classification",
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
  coord_fixed()+
  facet_wrap(~Age, nrow=1)




# Save the plot
ggsave("ggplot_confusion_matrix.pdf", width = 8, height = 6, dpi = 300)
