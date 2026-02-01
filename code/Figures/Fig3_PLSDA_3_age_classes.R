# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV


# Read proportion data
o_data <- read.csv(here::here("R_output","PLSDA_output","Mature forest","prop_treatment_plsda.csv"))
m_data <- read.csv(here::here("R_output","PLSDA_output","Mid-aged forest","prop_treatment_plsda.csv"))
y_data <- read.csv(here::here("R_output","PLSDA_output","Young forest","prop_treatment_plsda.csv"))


# Read count data
o_c_data <- read.csv(here::here("R_output","PLSDA_output","Mature forest","count_treatment_plsda.csv"))
m_c_data <- read.csv(here::here("R_output","PLSDA_output","Mid-aged forest","count_treatment_plsda.csv"))
y_c_data <- read.csv(here::here("R_output","PLSDA_output","Young forest","count_treatment_plsda.csv"))

# Your dataframe is o_c_data
# Calculate grand total (excluding the first column which contains row names)
grand_total <- sum(o_c_data[, -1])

# Calculate proportions for columns 2 onwards
proportions <- o_c_data
proportions[, -1] <- o_c_data[, -1] / grand_total * 100

# Display proportions as decimals
print("Proportions (as decimals):")
print(proportions)

# If you want percentages instead
proportions_pct <- o_c_data
proportions_pct[, -1] <- (o_c_data[, -1] / grand_total) * 100

print("\nProportions (as percentages):")
print(proportions_pct)
sum(proportions_pct)




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


# Add age labels to proportion data
o_long$Age <- "Mature forest"
m_long$Age <- "Mid-aged forest"
y_long$Age <- "Young forest"


# Combine proportion and count data
conf_prop_data <- rbind(o_long, m_long, y_long)
#conf_count_data <- rbind(o_c_long, m_c_long, y_c_long)

# Set factor levels for proportion data
conf_prop_data$Age <- factor(conf_prop_data$Age, levels = c("Young forest","Mid-aged forest","Mature forest"))
conf_prop_data$Reference <- factor(conf_prop_data$Reference, levels = c("Control","N","P","NP"))
conf_prop_data$Prediction <- factor(conf_prop_data$Prediction, levels = c("NP","P","N","Control"))

# # Set factor levels for count data (same levels)
# conf_count_data$Age <- factor(conf_count_data$Age, levels = c("Young forest","Mid-aged forest","Mature forest"))
# conf_count_data$Reference <- factor(conf_count_data$Reference, levels = c("Control","N","P","NP"))
# conf_count_data$Prediction <- factor(conf_count_data$Prediction, levels = c("NP","P","N","Control"))

# Merge proportion and count data for plotting


# Create custom color palette
col <- colorRampPalette(c("black","black","brown","gold","olivedrab","darkgreen"))


conf_prop_data$Proportion <- round(conf_prop_data$Proportion, 1)

# Create ggplot confusion matrix heatmap


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
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 12),
    panel.grid = element_blank()
  ) +
  coord_fixed() +
  facet_wrap(~Age, nrow = 1)


fig3
ggsave("figure_3.png", fig3, 
       width = 6, height = 4, dpi = 300, bg = "white")

