

# plsda summary

o_data <- read.csv(here::here("R_output","PLSDA_output_November","Mature forest","results_summary_plsda.csv"))
m_data <- read.csv(here::here("R_output","PLSDA_output_November","Mid-aged forest","results_summary_plsda.csv"))
y_data <- read.csv(here::here("R_output","PLSDA_output_November","Young forest","results_summary_plsda.csv"))

o_data$age <- "Mature forest"
m_data$age <- "Mid-aged forest"
y_data$age <- "Young forest"

summary <- rbind(o_data, m_data, y_data)

summary$age <- factor(summary$age, levels=c("Young forest","Mid-aged forest","Mature forest"))

ggplot(summary, aes(x= age, y= Value ))+
  facet_wrap(~Metric, scales="free")+
  geom_point()

sum_out <- spread(summary, "Metric","Value")

write.csv(sum_out, file="PLSDA_summary.csv")
