library(caret)
library(mixOmics)
library(agricolae)
library(tidyr)
library(dplyr)
library(ggplot2)

stand_ages <- c("All stands", "Young forest","Mid-aged forest","Mature forest")



for(q in 1:4){

t1 <- Sys.time()

# Select age group
sel_stand_age <- stand_ages[q]

dati <- read.csv(here::here("data_folder", "processed_spectra3.csv"))
dati <- dati[, -1]

# Age column
dati$Age[dati$Stand == "C1"] <- "Young forest"
dati$Age[dati$Stand == "C2"] <- "Young forest"
dati$Age[dati$Stand == "C3"] <- "Young forest"
dati$Age[dati$Stand == "C4"] <- "Mid-aged forest"
dati$Age[dati$Stand == "C5"] <- "Mid-aged forest"
dati$Age[dati$Stand == "C6"] <- "Mid-aged forest"
dati$Age[dati$Stand == "C7"] <- "Mature forest"
dati$Age[dati$Stand == "C8"] <- "Mature forest"
dati$Age[dati$Stand == "C9"] <- "Mature forest"

if (sel_stand_age != "All stands") {
  dati <- dati[dati$Age == sel_stand_age, ]
}
if (sel_stand_age == "All stands") {
  dati$Age <- "All stands"
}

## Count tops and define train size
count_tops <- as.data.frame(table(dati$Treatment, dati$Stand, dati$Age))
count_tops <- count_tops[count_tops$Freq > 0, ]
print(count_tops)

train_min_75 <- ceiling(min(count_tops$Freq) * 0.75)
cat("Train per stratum:", train_min_75, "\n")

## Complete cases
spec_complete <- complete.cases(dati[, 7:351])
spec          <- as.matrix(dati[spec_complete, 7:351])
dati_complete <- dati[spec_complete, ]
classi        <- as.factor(dati_complete$Treatment)

dati$statr <- paste(dati$Stand, dati$Treatment)
plots      <- unique(dati$statr)

### Helper: build one stratified 75/25 split
make_split <- function(dati, plots, train_min_75) {
  out_train <- data.frame()
  out_test  <- data.frame()
  for (plot in plots) {
    plot_data     <- dati[dati$statr == plot, ]
    train_indices <- sample(1:nrow(plot_data), train_min_75, replace = FALSE)
    out_train     <- rbind(out_train, plot_data[ train_indices, ])
    out_test      <- rbind(out_test,  plot_data[-train_indices, ])
  }
  list(train = out_train, test = out_test)
}

### Helper: extract clean train/test matrices from a split
clean_split <- function(split) {
  tr_ok <- complete.cases(split$train[, 7:351])
  te_ok <- complete.cases(split$test[,  7:351])
  list(
    train_spec    = split$train[tr_ok, 7:351],
    train_classes = split$train$Treatment[tr_ok],
    test_spec     = split$test[te_ok,  7:351],
    test_classes  = split$test$Treatment[te_ok]
  )
}

### ============================================================
### Step 1: Determine opt_comp via Tukey HSD on BER
### Use nsims_comp splits, fit plsda up to compi components,
### collect BER from perf(), then Tukey selects minimum
### component count not significantly worse than best.
### ============================================================
compi      <- 15
nsims_comp <- 10

ber_mat <- matrix(NA, nrow = compi, ncol = nsims_comp)

cat("\n=== Component Selection ===\n")
for (p in 1:nsims_comp) {
  cat("Component-selection iteration:", p, "\n")
  flush.console()
  
  d      <- clean_split(make_split(dati, plots, train_min_75))
  ncomp_p <- min(compi, nrow(d$train_spec) - 1)
  
  fit    <- mixOmics::plsda(X = d$train_spec, Y = as.factor(d$train_classes),
                            ncomp = ncomp_p)
  cv_res <- mixOmics::perf(fit,
                           validation  = "Mfold",
                           folds       = 5,
                           nrepeat     = 10,
                           progressBar = FALSE)
  
  ber_mat[1:ncomp_p, p] <- cv_res$error.rate$BER[, "max.dist"]
}

 ## Tukey HSD
acc_mat       <- 1 - ber_mat
acc_df        <- data.frame(
  Accuracy = as.numeric(t(acc_mat)),
  ncomps   = as.factor(rep(1:compi, each = nsims_comp))
)

modi      <- lm(Accuracy ~ ncomps, acc_df)
tuk       <- agricolae::HSD.test(modi, "ncomps")
tuk_dat   <- as.data.frame(tuk$groups)
tuk_dat$var <- as.numeric(row.names(tuk_dat))
tuk_dat   <- tuk_dat[order(-tuk_dat$Accuracy), ]
opt_comp <- tuk_dat$var[1]

tuk_dat$is_opt <- tuk_dat$var == opt_comp
print(tuk_dat[order(tuk_dat$var), ])
cat("Optimal components:", opt_comp, "\n")

ggplot(tuk_dat, aes(x = var, y = Accuracy, col = groups, fill = is_opt)) +
  geom_col() +
  labs(x = "N components", title = "Tukey HSD component selection")

### ============================================================
### Step 2: 100 permutations — fresh split each time
### ============================================================
test_sims <- 100
confus    <- vector("list", test_sims)
vip_list  <- vector("list", test_sims)

cat("\n=== 100-Permutation Validation (opt_comp =", opt_comp, ") ===\n")
for (s in 1:test_sims) {
  cat("Permutation:", s, "\n")
  flush.console()
  
  # Fresh independent split for this permutation
  d <- clean_split(make_split(dati, plots, train_min_75))
  
  cat("  Train n =", nrow(d$train_spec), "| Test n =", nrow(d$test_spec), "\n")
  
  lev           <- levels(as.factor(d$train_classes))
  train_classes <- factor(d$train_classes, levels = lev)
  test_classes  <- factor(d$test_classes,  levels = lev)
  
  # Fit with fixed opt_comp
  final_plsda <- mixOmics::plsda(X     = d$train_spec,
                                 Y     = train_classes,
                                 ncomp = opt_comp)
  
  # Predict
  predictions       <- predict(final_plsda, newdata = d$test_spec)
  preds             <- as.data.frame(predictions$class$max.dist)
  predicted_classes <- factor(preds[, opt_comp], levels = lev)
  
  confus[[s]]   <- caret::confusionMatrix(predicted_classes, test_classes)
  
  # Store VIP for this permutation (opt_comp column accumulates across components)
  vip_list[[s]] <- mixOmics::vip(final_plsda)[, opt_comp]
}

### ============================================================
### Step 3: Aggregate performance
### ============================================================
accu_v  <- sapply(confus, function(x) x$overall["Accuracy"])
kappa_v <- sapply(confus, function(x) x$overall["Kappa"])

cat("\n=== VALIDATION PERFORMANCE (100 permutations, 75/25 split) ===\n")
cat("Accuracy:", round(mean(accu_v), 3), "+-", round(sd(accu_v), 3), "\n")
cat("Kappa:",    round(mean(kappa_v), 3), "+-", round(sd(kappa_v), 3), "\n")

acc  <- paste0("Accuracy: ", round(mean(accu_v) * 100, 1), " +- ", round(sd(accu_v) * 100, 1), "%")
kapp <- paste0("Kappa: ",    round(mean(kappa_v), 2),      " +- ", round(sd(kappa_v), 2))

### Average confusion matrix
tabs     <- lapply(confus, function(x) x$table)
tabsi    <- Reduce("+", tabs)
tab_mean <- as.data.frame.matrix(tabsi / test_sims)

cat("\n=== AVERAGE CONFUSION MATRIX ===\n")
print(round(tab_mean, 2))

### ============================================================
### Step 4: Average VIP across 100 permutations
### ============================================================
vip_mat           <- do.call(cbind, vip_list)
rownames(vip_mat) <- as.numeric(sub(".*_", "", names(spec[1, ])))
colnames(vip_mat) <- paste0("perm_", 1:test_sims)

vip_mean_df <- data.frame(
  wavelength = as.numeric(rownames(vip_mat)),
  VIP_mean   = rowMeans(vip_mat),
  VIP_sd     = apply(vip_mat, 1, sd)
)

vip_mean_df

### ============================================================
### Step 5: Confusion matrix plot
### ============================================================
data_long <- tab_mean %>%
  tibble::rownames_to_column("Prediction") %>%
  tidyr::pivot_longer(cols      = -Prediction,
                      names_to  = "Reference",
                      values_to = "Count") %>%
  dplyr::group_by(Reference) %>%
  dplyr::mutate(Proportion = Count / sum(Count) * 100) %>%
  dplyr::ungroup()

conf_prop_data <- data_long[, c("Prediction", "Reference", "Count", "Proportion")]
conf_prop_data$Proportion <- round(conf_prop_data$Proportion, 0)

conf_prop_data$Reference  <- factor(conf_prop_data$Reference,
                                    levels = c("Control", "N", "P", "NP"))
conf_prop_data$Prediction <- factor(conf_prop_data$Prediction,
                                    levels = c("NP", "P", "N", "Control"))

col <- colorRampPalette(c("black", "black", "darkgray", "brown",
                          "gold", "olivedrab", "darkgreen"))

fig_conf_matrix <- ggplot(conf_prop_data,
                          aes(x = Reference, y = Prediction, fill = Proportion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Proportion, 0)),
            color = "white", size = 8, fontface = "bold") +
  scale_fill_gradientn(
    colors = col(20), name = "",
    limits = c(0, 100), breaks = c(0, 25, 50, 75, 100),
    labels = c("0%", "25%", "50%", "75%", "100%")
  ) +
  labs(x        = "Reference class",
       y        = "Predicted class",
       subtitle = paste0(acc, "; ", kapp)) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title   = element_text(size = 12),
    legend.title = element_blank(),
    legend.text  = element_text(size = 12),
    panel.grid   = element_blank()
  ) +
  coord_fixed() +
  ggtitle(paste0("75/25% individual trees ", sel_stand_age) )

fig_conf_matrix

ggsave(paste0("plot indiv tree ", sel_stand_age, " plsda.png"),
       fig_conf_matrix, width = 10, height = 4, dpi = 300, bg = "white")


# Save outputs
plsda_out <- here::here("R_output","PLSDA_output_response_72_25", sel_stand_age)

if(!dir.exists(plsda_out)){ 
  dir.create(plsda_out, recursive = TRUE)
}

# Output
results_summary <- data.frame(
  Metric = c("Test_Accuracy", "Test_Kappa", 
             "Components_Used", "number of simulations",
             "Train_Sample_Size", "Test_Sample_Size"),
  Value = c(acc,
            kapp,
            opt_comp,
            test_sims,
            nrow(d$train_spec),
            nrow(d$test_spec)))

write.csv(conf_prop_data, file.path(plsda_out,"prop_treatment_plsda.csv"))
write.csv(vip_mean_df, file.path(plsda_out,"vip_scores.csv"), row.names = FALSE)
write.csv(results_summary, file.path(plsda_out,"results_summary_plsda.csv"), row.names = FALSE)

print( paste0("finished ", sel_stand_age))

print(Sys.time() - t1)

}
