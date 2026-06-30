# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV



# ares <- read.csv(here::here("R_output","PLSDA_output","LOSO all age","results_summary_plsda_loso.csv"))
# yres <- read.csv(here::here("R_output","PLSDA_output","Young stands loso","results_summary_plsda_loso.csv"))
# mres <- read.csv(here::here("R_output","PLSDA_output","Mid-aged stands loso","results_summary_plsda_loso.csv"))
# ores <- read.csv(here::here("R_output","PLSDA_output","Mature stands loso","results_summary_plsda_loso.csv"))

 ares <- read.csv(here::here("R_output","PLSDA_output_response_72_25","All stands","results_summary_plsda.csv"))
 yres <- read.csv(here::here("R_output","PLSDA_output_response_72_25","Young forest","results_summary_plsda.csv"))
 mres <- read.csv(here::here("R_output","PLSDA_output_response_72_25","Mid-aged forest","results_summary_plsda.csv"))
 ores <- read.csv(here::here("R_output","PLSDA_output_response_72_25","Mature forest","results_summary_plsda.csv"))
 



ares
yres
mres
ores


############

# Read count data
a_c_data <- read.csv(here::here("R_output","PLSDA_output","All stands loso","count_treatment_plsda_loso.csv"))
o_c_data <- read.csv(here::here("R_output","PLSDA_output","Mature stands loso","count_treatment_plsda_loso.csv"))
m_c_data <- read.csv(here::here("R_output","PLSDA_output","Mid-aged stands loso","count_treatment_plsda_loso.csv"))
y_c_data <- read.csv(here::here("R_output","PLSDA_output","Young stands loso","count_treatment_plsda_loso.csv"))

####  # Old Forest
o_count_long <- melt(o_c_data, id.vars = "X", variable.name = "Reference", value.name = "Count")
colnames(o_count_long)[1] <- "Prediction"

# Compute row totals and proportions
o_row_totals <- aggregate(Count ~ Prediction, data = o_count_long, sum)
o_count_long <- merge(o_count_long, o_row_totals, by = "Prediction", suffixes = c("", "_total"))
o_count_long$Proportion <- (o_count_long$Count / o_count_long$Count_total) * 100

o_conf_prop_data <- o_count_long[, c("Prediction", "Reference",  "Proportion")]
o_conf_prop_data$Proportion <- round(o_conf_prop_data$Proportion, 1)
o_conf_prop_data$Age <- "Mature forest"

####  # Mid-aged Forest
m_count_long <- melt(m_c_data, id.vars = "X", variable.name = "Reference", value.name = "Count")
colnames(m_count_long)[1] <- "Prediction"

# Compute row totals and proportions
m_row_totals <- aggregate(Count ~ Prediction, data = m_count_long, sum)
m_count_long <- merge(m_count_long, m_row_totals, by = "Prediction", suffixes = c("", "_total"))
m_count_long$Proportion <- (m_count_long$Count / m_count_long$Count_total) * 100

m_conf_prop_data <- m_count_long[, c("Prediction", "Reference", "Proportion")]
m_conf_prop_data$Proportion <- round(m_conf_prop_data$Proportion, 1)
m_conf_prop_data$Age <- "Mid-aged forest"


####  # Young Forest
y_count_long <- melt(y_c_data, id.vars = "X", variable.name = "Reference", value.name = "Count")
colnames(y_count_long)[1] <- "Prediction"

# Compute row totals and proportions
y_row_totals <- aggregate(Count ~ Prediction, data = y_count_long, sum)
y_count_long <- merge(y_count_long, y_row_totals, by = "Prediction", suffixes = c("", "_total"))
y_count_long$Proportion <- (y_count_long$Count / y_count_long$Count_total) * 100

y_conf_prop_data <- y_count_long[, c("Prediction", "Reference",  "Proportion")]
y_conf_prop_data$Proportion <- round(y_conf_prop_data$Proportion, 1)
y_conf_prop_data$Age <- "Young forest"


#####  #  All ages
a_count_long <- melt(a_c_data, id.vars = "X", variable.name = "Reference", value.name = "Count")
colnames(a_count_long)[1] <- "Prediction"

# Compute row totals and proportions
a_row_totals <- aggregate(Count ~ Prediction, data = a_count_long, sum)
a_count_long <- merge(a_count_long, a_row_totals, by = "Prediction", suffixes = c("", "_total"))
a_count_long$Proportion <- (a_count_long$Count / a_count_long$Count_total) * 100

a_conf_prop_data <- a_count_long[, c("Prediction", "Reference",  "Proportion")]
a_conf_prop_data$Proportion <- round(a_conf_prop_data$Proportion, 1)
a_conf_prop_data$Age <- "All stands"


####
# Rbind the 3 together

conf_prop_data <- rbind(
  a_conf_prop_data, o_conf_prop_data,
  m_conf_prop_data, y_conf_prop_data )
# 

#########

# Above uses the leave one stand out.

# below reads in the 75/25 test train split

# a_c_data <- read.csv(here::here("R_output","PLSDA_output_response_72_25","All stands","prop_treatment_plsda.csv"))
# o_c_data <- read.csv(here::here("R_output","PLSDA_output_response_72_25","Mature forest","prop_treatment_plsda.csv"))
# m_c_data <- read.csv(here::here("R_output","PLSDA_output_response_72_25","Mid-aged forest","prop_treatment_plsda.csv"))
# y_c_data <- read.csv(here::here("R_output","PLSDA_output_response_72_25","Young forest","prop_treatment_plsda.csv"))
# 
# y_c_data$Age <- "Young forest"
# m_c_data$Age <- "Mid-aged forest"
# o_c_data$Age <- "Mature forest"
# a_c_data$Age <- "All stands"
# 
# conf_prop_data <- rbind(
#   a_c_data, o_c_data,
#   m_c_data, y_c_data )



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



