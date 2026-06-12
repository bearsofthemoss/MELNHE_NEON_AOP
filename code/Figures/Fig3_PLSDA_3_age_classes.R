# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV



ares <- read.csv(here::here("R_output","PLSDA_output_response","All stands","results_summary_plsda.csv"))
yres <- read.csv(here::here("R_output","PLSDA_output_response","Young forest","results_summary_plsda.csv"))
mres <- read.csv(here::here("R_output","PLSDA_output_response","Mid-aged forest","results_summary_plsda.csv"))
ores <- read.csv(here::here("R_output","PLSDA_output_response","Mature forest","results_summary_plsda.csv"))

ares
yres
mres
ores
### Add from the 'all age' PLSDA first

prop_data <- read.csv(here::here("R_output","PLSDA_output_response","All stands","prop_treatment_plsda.csv"))


# Reshape to long format and compute row-wise proportions
all_conf_prop_data <- melt(prop_data, id.vars = "X", variable.name = "Reference", value.name = "Proportion")
colnames(all_conf_prop_data)[1] <- "Prediction"

all_conf_prop_data$Proportion <- round(all_conf_prop_data$Proportion, 2) * 100
all_conf_prop_data$Age <- "All stands"



#######################
#########################################################



# Read count data
o_c_data <- read.csv(here::here("R_output","PLSDA_output_response","Mature forest","prop_treatment_plsda.csv"))
m_c_data <- read.csv(here::here("R_output","PLSDA_output_response","Mid-aged forest","prop_treatment_plsda.csv"))
y_c_data <- read.csv(here::here("R_output","PLSDA_output_response","Young forest","prop_treatment_plsda.csv"))

####  # Old Forest
o_conf_prop_data <- melt(o_c_data, id.vars = "X", variable.name = "Reference", value.name = "Proportion")
colnames(o_conf_prop_data)[1] <- "Prediction"

o_conf_prop_data$Proportion <- round(o_conf_prop_data$Proportion, 2)*100
o_conf_prop_data$Age <- "Mature forest"

####  # Mid-aged Forest
m_conf_prop_data <- melt(m_c_data, id.vars = "X", variable.name = "Reference", value.name = "Proportion")
colnames(m_conf_prop_data)[1] <- "Prediction"

m_conf_prop_data$Proportion <- round(m_conf_prop_data$Proportion, 2)*100
m_conf_prop_data$Age <- "Mid-aged forest"


####  # Young Forest
y_conf_prop_data <- melt(y_c_data, id.vars = "X", variable.name = "Reference", value.name = "Proportion")
colnames(y_conf_prop_data)[1] <- "Prediction"

y_conf_prop_data$Proportion <- round(y_conf_prop_data$Proportion, 2)*100
y_conf_prop_data$Age <- "Young forest"



####
# Rbind the 3 together

conf_prop_data <- rbind(
  all_conf_prop_data, o_conf_prop_data,
  m_conf_prop_data, y_conf_prop_data )


table(conf_prop_data$Reference, conf_prop_data$Prediction)

# Set factor levels
conf_prop_data$Reference  <- factor(conf_prop_data$Reference,  levels = c("Control", "N", "P", "NP"))
conf_prop_data$Prediction <- factor(conf_prop_data$Prediction, levels = c( "NP", "P","N","Control"))




# Create custom color palette
col <- colorRampPalette(c("black","black","darkgray","brown","gold","olivedrab","darkgreen"))



# Create ggplot confusion matrix heatmap
conf_prop_data$Age <- factor(conf_prop_data$Age, levels=c("All stands","Young forest","Mid-aged forest","Mature forest"))

fig3 <- ggplot(conf_prop_data, aes(x = Reference, y = Prediction, fill = Proportion)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(Proportion, 0)), 
            color = "white", size = 4, fontface = "bold") +
  scale_fill_gradientn(colors = col(20),
                       name = "",               # Blank legend title
                       limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c("0%", "25%", "50%", "75%", "100%")
  ) +
  labs(x = "Reference class",
       y = "Predicted class") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    strip.text = element_text(size = 16)
  ) +
  coord_fixed() +
  facet_wrap(~Age, nrow = 1)


fig3

ggsave("figure_3.png", fig3, 
       width = 10, height = 4, dpi = 300, bg = "white")



