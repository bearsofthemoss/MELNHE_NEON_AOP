library(dplyr)
library(caret)
library(mixOmics)

dati <- read.csv(here::here("data_folder","processed_spectra3.csv"))
dati <- dati[,-1]
min(table(dati$Treatment, dati$Stand))

names(dati)


dati_long <- tidyr::gather(dati, "wavelength", "refl", 7:351)

dati_long$statr <- paste(dati_long$Stand, dati_long$Treatment)

# Plot-level average (Stand x Treatment x wavelength)
dati_avg <- dati_long %>%
  dplyr::group_by(Stand, Treatment,  statr, wavelength) %>%
  dplyr::summarise(refl = mean(refl, na.rm = TRUE), .groups = "drop")

# Spread back to wide format — same shape as dati but one row per plot
dati_wide <- tidyr::spread(dati_avg, wavelength, refl)


##############################################

## This enforces plot averages!!
 # dati <- dati_wide

  dati #  just use dati if you want individual trees

##############################################

stands <- unique(dati$Stand)

compi  <- 20   # max components to test; cap lower given small n

ber_mat <- matrix(NA, nrow = compi, ncol = length(stands))
colnames(ber_mat) <- stands

names(dati)

# indiv tree here 
spec_complete <- complete.cases(dati[, 7:351])
spec          <- as.matrix(dati[spec_complete, 7:351])

# ## plot average here
# spec_complete <- complete.cases(dati[, 4:348])
# spec          <- as.matrix(dati[spec_complete, 4:348])

dati_complete <- dati[spec_complete, ]



classi        <- as.factor(dati$Treatment[spec_complete])

for (s in seq_along(stands)) {
  cat("Component tuning — leaving out stand:", stands[s], "\n")
  flush.console()
  
  inTrain    <- dati_complete$Stand != stands[s]
  traini     <- spec[inTrain, ]
  trainclass <- classi[inTrain]
  
  cat("  Train n =", nrow(traini), "\n")
  
  # Cap components at training set size - 1 to be safe
  ncomp_s <- min(compi, nrow(traini) - 1)
  
  plsda_fit <- mixOmics::plsda(X = traini, Y = trainclass, ncomp = ncomp_s)
  
  cv_res <- mixOmics::perf(plsda_fit,
                 validation  = "Mfold",
                 folds       = 5,
                 nrepeat     = 10,
                 progressBar = FALSE)
  
  ber_mat[1:ncomp_s, s] <- cv_res$error.rate$BER[, "max.dist"]
}

### Tukey HSD on accuracy (1 - BER) across folds
acc_mat       <- 1 - ber_mat
acc_df        <- as.data.frame(as.numeric(t(acc_mat)))
acc_df        <- cbind(acc_df, rep(1:compi, each = length(stands)))
names(acc_df) <- c("Accuracy", "ncomps")
acc_df$ncomps <- as.factor(acc_df$ncomps)

modi <- lm(Accuracy ~ ncomps, acc_df)
tuk  <- agricolae::HSD.test(modi, "ncomps")

tuk_dat     <- as.data.frame(tuk$groups)
tuk_dat$var <- as.numeric(row.names(tuk_dat))
tuk_dat     <- tuk_dat[order(-tuk_dat$Accuracy), ]
opt_comp   <- tuk_dat$var[1]



cat("\nTukey HSD results:\n")
print(tuk_dat[order(tuk_dat$var), ])
cat("\nOptimal components:", opt_comp, "\n")

### Leave-one-stand-out validation with fixed opt_comp
confus <- list()
finmods <- list()   # add this

for (s in seq_along(stands)) {
  cat("Validation — leaving out stand:", stands[s], "\n")
  flush.console()
  
  inTrain    <- dati_complete$Stand != stands[s]
  traini     <- spec[inTrain, ]
  testi      <- spec[!inTrain, ]
  trainclass <- classi[inTrain]
  testclass  <- classi[!inTrain]
  
  cat("  Train n =", nrow(traini), "| Test n =", nrow(testi), "\n")
  
  final_plsda     <- plsda(X = traini, Y = trainclass, ncomp = opt_comp)
  finmods[[s]]    <- final_plsda   # add this
  
  predictions       <- predict(final_plsda, newdata = testi)
  preds             <- as.data.frame(predictions$class$max.dist)
  predicted_classes <- preds[, opt_comp]
  
  lev               <- levels(factor(trainclass))
  predicted_classes <- factor(predicted_classes, levels = lev)
  testclass_f       <- factor(testclass,         levels = lev)
  
  confus[[s]] <- confusionMatrix(predicted_classes, testclass_f)
}
names(confus)  <- stands
names(finmods) <- stands

### Aggregate
accu_v  <- sapply(confus, function(x) x$overall["Accuracy"])
kappa_v <- sapply(confus, function(x) x$overall["Kappa"])
cat("\n=== LOSO VALIDATION PERFORMANCE ===\n")
cat("Accuracy:", round(mean(accu_v), 3), "+-", round(sd(accu_v), 3), "\n")
cat("Kappa:",    round(mean(kappa_v), 3), "+-", round(sd(kappa_v), 3), "\n")

### VIP across LOSO folds
# Extract VIP for each fold — use opt_comp column (accumulates across components)
vip_list <- lapply(finmods, function(m) vip(m)[, opt_comp])

# Matrix: rows = bands, cols = stands
vip_mat          <- do.call(cbind, vip_list)
colnames(vip_mat) <- stands
rownames(vip_mat) <- as.numeric(sub(".*_", "", colnames(spec))) # wavelength names as row labels

vip_long <- vip_mat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("wavelength") %>%
  tidyr::pivot_longer(cols      = -wavelength,
                      names_to  = "Stand",
                      values_to = "VIP") %>%
  dplyr::mutate(wavelength = as.numeric(wavelength))


# Per-stand and mean summary
vip_mean <- rowMeans(vip_mat)
vip_sd   <- apply(vip_mat, 1, sd)

vip_df <- data.frame(
  wavelength = colnames(spec),
  VIP_mean   = vip_mean,
  VIP_sd     = vip_sd,
  VIP_upper  = vip_mean + vip_sd,
  VIP_lower  = vip_mean - vip_sd
)

vip_df$wavelength <- as.numeric(sub(".*_", "", vip_df$wavelength))



vip_stand_df <- vip_long %>%
  dplyr::mutate(important = VIP > 1)


vip_long$Age[vip_long$Stand=="C1"]<-"Young forest"
vip_long$Age[vip_long$Stand=="C2"]<-"Young forest"
vip_long$Age[vip_long$Stand=="C3"]<-"Young forest"
vip_long$Age[vip_long$Stand=="C4"]<-"Mid-aged forest"
vip_long$Age[vip_long$Stand=="C5"]<-"Mid-aged forest"
vip_long$Age[vip_long$Stand=="C6"]<-"Mid-aged forest" 
vip_long$Age[vip_long$Stand=="C7"]<-"Mature forest"
vip_long$Age[vip_long$Stand=="C8"]<-"Mature forest"
vip_long$Age[vip_long$Stand=="C9"]<-"Mature forest"
### Plot
ggplot()+
#vip_df, aes(x = wavelength, y = VIP_mean)) +
  geom_line(data = vip_long,
            aes(x = wavelength, y = VIP, group = Stand),
            color = "black", linewidth = 0.5) +
  # geom_ribbon(aes(ymin = VIP_lower, ymax = VIP_upper),
  #             alpha = 0.3, fill = "olivedrab") +
  geom_line(color = "darkgreen", linewidth = 0.9) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.6) +
  labs(x     = "Wavelength (nm)",
       y     = "VIP score",
       title = "Variable Importance — mean ± SD across 9 LOSO folds") +
  theme_minimal()+
  facet_wrap(~Age, nrow=3)

#############################

acc <- paste0("Accuracy:", round(mean(accu_v)*100, 1), "+-", round(sd(accu_v)*100, 1))
kapp <- paste0("Kappa:",    round(mean(kappa_v), 2), "+-", round(sd(kappa_v), 2))

### Average confusion matrix
tabs     <- lapply(confus, function(x) x$table)
tabsi    <- Reduce("+", tabs)
tab_mean <- as.data.frame.matrix(tabsi / length(stands))

cat("\n=== AVERAGE CONFUSION MATRIX (mean over 9 LOSO folds) ===\n")
print(round(tab_mean, 2))

### with tab_mean do this!
# Convert tab_mean to long format
data_long <- tab_mean %>%
  tibble::rownames_to_column("Prediction") %>%
  tidyr::pivot_longer(cols = -Prediction,
                      names_to  = "Reference",
                      values_to = "Count")

# Proportion by reference class (columns sum to 100)
data_long <- data_long %>%
  dplyr::group_by(Reference) %>%
  dplyr::mutate(Proportion = Count / sum(Count) * 100) %>%
  dplyr::ungroup()


conf_prop_data <- data_long[, c("Prediction", "Reference", "Count", "Proportion")]
conf_prop_data$Proportion <- round(conf_prop_data$Proportion, 0)

# Set factor levels
conf_prop_data$Reference  <- factor(conf_prop_data$Reference,  levels = c("Control", "N", "P", "NP"))
conf_prop_data$Prediction <- factor(conf_prop_data$Prediction, levels = c("Control", "N", "P", "NP"))

# Color palette
col <- colorRampPalette(c("black","black","darkgray","brown","gold","olivedrab","darkgreen"))



conf_prop_data$Prediction <- factor(conf_prop_data$Prediction, levels=c("NP","P","N","Control"))

# Plot
fig_loso_trees <- ggplot(conf_prop_data, aes(x = Reference, y = Prediction, fill = Proportion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Proportion, 0)),
            color = "white", size = 8, fontface = "bold") +
  scale_fill_gradientn(
    colors = col(20),
    name = "",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = c("0%", "25%", "50%", "75%", "100%")
  ) +
  labs(
    x = "Reference class",
    y = "Predicted class",
    subtitle= paste0( acc,"; ", kapp)
  ) +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y   = element_text(size = 12),
    axis.title    = element_text(size = 12),
    legend.title  = element_blank(),
    legend.text   = element_text(size = 12),
    panel.grid    = element_blank()
  ) +
  coord_fixed()+
  ggtitle("LOSO all stands- individual trees")


 fig_loso_trees
ggsave("plot indiv_tree loso  fig.png", fig_loso_trees,
        width = 10, height = 4, dpi = 300, bg = "white")

# #fig_loso_plot_avg 
# ggsave("plot average loso fig2.png", fig_loso_plot_avg, 
#        width = 10, height = 4, dpi = 300, bg = "white")



library(patchwork)
fig_loso_plot_avg + fig_loso_trees



############################

# Extract VIP from each LOSO model
# finmods should be your list of 9 plsda fits from the validation loop
vip_list <- lapply(finmods, function(m) vip(m)[, 1])

# Combine into matrix: rows = wavelengths, cols = stands
vip_mat <- do.call(cbind, vip_list)
colnames(vip_mat) <- stands

# Summary across folds
vip_mean <- rowMeans(vip_mat)
vip_sd   <- apply(vip_mat, 1, sd)

vip_df <- data.frame(
  Variable_Index = 1:nrow(vip_mat),
  VIP_mean       = vip_mean,
  VIP_sd         = vip_sd,
  VIP_upper      = vip_mean + vip_sd,
  VIP_lower      = vip_mean - vip_sd
)

# Plot
ggplot(vip_df, aes(x = Variable_Index, y = VIP_mean)) +
  geom_ribbon(aes(ymin = VIP_lower, ymax = VIP_upper),
              alpha = 0.3, fill = "olivedrab") +
  geom_line(color = "darkgreen", linewidth = 0.8) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  labs(x = "Band index",
       y = "VIP score",
       title = "Variable Importance — mean ± SD across 9 LOSO folds") +
  theme_minimal()