library(dplyr)
library(caret)

# Select age group

dati <- read.csv(here::here("data_folder","processed_spectra.csv"))
min(table(dati$Treatment, dati$Stand))

# Provide the Age column  
dati$Age[dati$Stand=="C1"]<-"Young forest"
dati$Age[dati$Stand=="C2"]<-"Young forest"
dati$Age[dati$Stand=="C3"]<-"Young forest"
dati$Age[dati$Stand=="C4"]<-"Mid-aged forest"
dati$Age[dati$Stand=="C5"]<-"Mid-aged forest"
dati$Age[dati$Stand=="C6"]<-"Mid-aged forest" 
dati$Age[dati$Stand=="C7"]<-"Mature forest"
dati$Age[dati$Stand=="C8"]<-"Mature forest"
dati$Age[dati$Stand=="C9"]<-"Mature forest"

## Select Stand age
#dati <- dati[dati$Age==sel_stand_age, ]

## Count tops
count_tops <- as.data.frame(table(dati$Treatment, dati$Stand, dati$Age))
count_tops <- count_tops[count_tops$Freq>0,]
print(count_tops)

train_min_75 <- ceiling(min(count_tops$Freq) * .75)

# Create statr column for stratification
dati$statr <- paste(dati$Stand, dati$Treatment)

### Generate 100 random partitions ###
set.seed(1234)
nsims <- 10
rndid <- list()


# specify caret plsda methods
ctrl_cv <- trainControl(method = "repeatedcv", 
                        repeats = 10, number=5,
                        summaryFunction = multiClassSummary)

for (i in 1:nsims){
  # Create random indices for each statr group
  rndid[[i]] <- with(dati, ave(1:nrow(dati), statr, 
                               FUN=function(x) {sample.int(length(x))}))
}

### Component selection using cross-validation ###
compi <- 20  # max number of components to test
ctrl <- trainControl(method = "repeatedcv", repeats = 10, number = 10,
                     summaryFunction = multiClassSummary)

# Get complete spectral data
names(dati)
spec_complete <- complete.cases(dati[, 8:352])
spec <- as.matrix(dati[spec_complete, 8:352])
classi <- as.factor(dati$Treatment[spec_complete])
dati_complete <- dati[spec_complete, ]

cat("\nTotal samples after removing incomplete cases:", nrow(spec), "\n\n")

### Initial model tuning across iterations ###
mods <- list()

for (nsim in 1:nsims){
  cat("Tuning iteration:", nsim, "\n")
  flush.console()
  set.seed(nsim)
  
  # Create train/test split
  inTrain <- rndid[[nsim]][spec_complete] <= train_min_75
  
  traini <- spec[inTrain, ] 
  testi <- spec[!inTrain, ]
  trainclass <- classi[inTrain]
  testclass <- classi[!inTrain]
  
  # Train model with caret for component selection
  plsFit <- train(traini, trainclass, 
                  method = "pls", 
                  tuneLength = compi,
                  trControl = ctrl_cv)
  
  mods[[nsim]] <- plsFit
}

### Select optimal number of components ###
ncomps <- vector(length = nsims)
for (i in 1:nsims){
  ncomps[i] <- mods[[i]]$finalModel$ncomp
}

### Kappa statistics for component selection ###
kappas <- data.frame(ncomps = 1:compi, 
                     matrix(NA, nrow = compi, ncol = length(mods)))

for (i in 1:length(mods)){
  kappas[, i+1] <- mods[[i]]$results$Kappa
}

### Tukey test for optimal components ###
kapp <- as.data.frame(as.numeric(t(kappas[, -1])))
kapp <- cbind(kapp, rep(1:compi, each = length(mods)))
names(kapp) <- c("Kappa", "ncomps")
kapp$ncomps <- as.factor(kapp$ncomps)

modi <- lm(Kappa ~ ncomps, kapp)
tuk <- agricolae::HSD.test(modi, "ncomps")

tuk_dat <- as.data.frame(tuk$groups)
tuk_dat$var <- as.numeric(row.names(tuk_dat))
tuk_dat <- tuk_dat[order(tuk_dat$var, decreasing = F), ]
letters <- as.character(tuk_dat$groups)


# select best number of components
tuk_dat <- tuk_dat[order(tuk_dat$groups),]
top_group <- tuk_dat$groups[1]
components_in_top_group <- tuk_dat$var[tuk_dat$groups == top_group]
opt_comp <- min(components_in_top_group)

opt_comp <- min(tuk_dat$var[tuk_dat$groups == top_group])


### Kappa plot ###
#pdf("./R_output/PLSDA_kappas_treat.pdf", width = 5, height = 4)
par(bty = "l")
boxplot(kapp$Kappa ~ kapp$ncomps, 
        ylim = c(0, max(kapp$Kappa) + 0.1),
        xlab = "Number of components", 
        ylab = "Kappa")
text(x = 1:compi, y = rep(max(kapp$Kappa) + 0.05, compi), letters)

#dev.off()

#########################################################################
#  Above is step 1: determine the number of components.

####### Step 2

### Determine final number of components ###


opt_comp <- as.numeric(names(sort(table(ncomps), decreasing = TRUE)[1]))

cat("\nUsing", opt_comp, "components for final models\n\n")

finmods <- list()


for (nsim in 1:nsims){
  cat("Final model iteration:", nsim, "\n")
  flush.console()
  set.seed(1234)
  
  inTrain <- rndid[[nsim]][spec_complete] <= train_min_75
  
  training <- spec[!inTrain, ]
  nrow(training)
  testing <- spec[inTrain, ]
  trainclass <- as.factor(classi[!inTrain]) 
  testclass <- as.factor(classi[inTrain])
  
  # Build final PLSDA model using caret with LOOCV
  finalModel <- train(training, trainclass,
                      method = "pls",
                      tuneGrid = data.frame(ncomp = opt_comp),
                      #  trControl = trainControl(method = "LOOCV"))
                      trControl = ctrl_cv )
  
  finmods[[nsim]] <- finalModel
}

# Save models
#saveRDS(finmods, paste0("./R_output/finmods_treat_", opt_comp, "comps.rds"))

### Model Validation - Predictions on test sets ###
probis <- list()
confus <- list()

for (nsim in 1:nsims){
  cat("Validation iteration:", nsim, "\n")
  flush.console()
  set.seed(nsim)
  
  inTrain <- rndid[[nsim]][spec_complete] <= train_min_75
  
  testing <- spec[inTrain, ]
  nrow(testing)
  
  testclass <- as.factor(classi[inTrain])
  
  # Predictions
  plsProbs <- predict(finmods[[nsim]], newdata = testing, type = "prob")
  plsClasses <- predict(finmods[[nsim]], newdata = testing)
  
  # Confusion matrix
  confus[[nsim]] <- caret::confusionMatrix(data = plsClasses, testclass)
  
  # Store probabilities
  probs <- as.data.frame(plsProbs)
  names(probs) <- sapply(strsplit(names(probs), split = "\\."), "[", 1)
  probs <- cbind(testclass, probs)
  probis[[nsim]] <- probs 
}

### Performance Statistics ###

# ## Calibration Performance (Training) ##
# accu_cal <- numeric(length = nsims)
# kappa_cal <- numeric(length = nsims)
# 
# for (i in 1:nsims){
#   accu_cal[i] <- mods[[i]]$results$Accuracy[opt_comp]
#   kappa_cal[i] <- mods[[i]]$results$Kappa[opt_comp]
# }
# 
# cat("\n=== CALIBRATION PERFORMANCE (Training) ===\n")
# cat("Accuracy:", round(mean(accu_cal), 3), "±", round(sd(accu_cal), 3), "\n")
# cat("Kappa:", round(mean(kappa_cal), 3), "±", round(sd(kappa_cal), 3), "\n")

# ## Validation Performance (Testing) ##
# accu_val <- numeric(length = nsims)
# kappa_val <- numeric(length = nsims)
# 
# for (i in 1:nsims){
#   accu_val[i] <- confus[[i]]$overall["Accuracy"]
#   kappa_val[i] <- confus[[i]]$overall["Kappa"]
# }
# 
# cat("\n=== VALIDATION PERFORMANCE (Testing) ===\n")
# cat("Accuracy:", round(mean(accu_val), 3), "±", round(sd(accu_val), 3), "\n")
# cat("Kappa:", round(mean(kappa_val), 3), "±", round(sd(kappa_val), 3), "\n")
# 
# ### Average Confusion Matrix ###
# tabs <- list()
# for(i in 1:length(confus)){
#   tabs[[i]] <- confus[[i]]$table
# }
# 
# tabsi <- Reduce('+', tabs)
# tab_mean <- as.data.frame.matrix(tabsi / length(confus))
# 
# cat("\n=== AVERAGE CONFUSION MATRIX ===\n")
# print(round(tab_mean, 2))

### Accuracy Validation

accu_v <- numeric(length=nsims)

kappa_v <- numeric(length=nsims)

for (i in 1:nsims){
  
  accu_v[i] <- confus[[i]]$overall[1] 
  
  kappa_v[i] <- confus[[i]]$overall[2]
  
}


### the right way to calculate average accuracy and sd for mean
(accu_val_mean <- mean(accu_v))

(accu_val_sd <- sd(accu_v))



(kappa_val_mean <- mean(kappa_v))

(kappa_val_sd <- sd(kappa_v))


### Confusion table plot 

tabs <- list()

for(i in 1:length(confus)){
  
  tabs[[i]] <- confus[[i]]$table
  
}



tabsi <- Reduce('+', tabs)

tab_mean <- as.data.frame.matrix(tabsi/length(confus))

####
### Calculate VIP scores across all 100 models ###



############################################################################

### Calculate VIP scores using mixOmics across all 100 iterations ###
vip_mixo_list <- list()

for (i in 1:nsims){
  cat("Calculating VIP scores - iteration:", i, "\n")
  flush.console()
  set.seed(i)
  
  inTrain <- rndid[[i]][spec_complete] <= train_min_75
  training <- spec[inTrain, ]
  trainclass <- as.factor(classi[inTrain])
  
  # Build mixOmics model with same parameters as caret
  mixo_model <- mixOmics::plsda(training, trainclass, ncomp = opt_comp)
  
  # Get VIP scores
  vip_scores <- mixOmics::vip(mixo_model)
  vip_mixo_list[[i]] <- vip_scores[, 1]  # Extract first column (VIP values)
}

# Average VIP scores across all iterations
vip_mixo_matrix <- do.call(cbind, vip_mixo_list)
vip_mixo_mean <- rowMeans(vip_mixo_matrix)
vip_mixo_sd <- apply(vip_mixo_matrix, 1, sd)

# Create VIP results data frame
vip_results_mixo <- data.frame(
  variable = names(vip_mixo_mean),
  vip_mean = vip_mixo_mean,
  vip_sd = vip_mixo_sd
)





################

### Save results ###

validation = data.frame(
#  Age = sel_stand_age,
  Age =  "All ages",
  accuracy_mean = accu_val_mean,
  accuracy_sd = accu_val_sd,
  kappa_mean = kappa_val_mean,
  kappa_sd = kappa_val_sd,
  nsims = nsims,
  ncomp_opt = opt_comp,
  train_min_75 = train_min_75,
  test_pixels= nrow(testing),
  train_pixels = nrow(training))


#plsda_out <- here::here("R_output","PLSDA_output", sel_stand_age)

plsda_out <- here::here("R_output","PLSDA_output", "All_ages")

if(!dir.exists(plsda_out)){ 
  dir.create(plsda_out, recursive = TRUE)
}


write.csv(tab_mean, file.path(plsda_out,"prop_treatment_plsda.csv"))
write.csv(tabsi, file.path(plsda_out,"count_treatment_plsda.csv"))
write.csv(validation, file.path(plsda_out,"results_summary_plsda.csv"), row.names = FALSE)
write.csv(vip_results_mixo, file.path(plsda_out,"vip_scores.csv"), row.names = FALSE)
write.csv(tuk_dat, file.path(plsda_out,"tukey_component_letters.csv"), row.names = FALSE)



##################################################

# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV


# Read proportion data
count_data <- read.csv(here::here("R_output","PLSDA_output","All_ages","count_treatment_plsda.csv"))


# Reshape to long format and compute row-wise proportions
count_long <- melt(count_data, id.vars = "X", variable.name = "Reference", value.name = "Count")
colnames(count_long)[1] <- "Prediction"

# Compute row totals and proportions
row_totals <- aggregate(Count ~ Prediction, data = count_long, sum)
count_long <- merge(count_long, row_totals, by = "Prediction", suffixes = c("", "_total"))
count_long$Proportion <- (count_long$Count / count_long$Count_total) * 100

conf_prop_data <- count_long[, c("Prediction", "Reference", "Count", "Proportion")]
conf_prop_data$Proportion <- round(conf_prop_data$Proportion, 1)

# Set factor levels
conf_prop_data$Reference  <- factor(conf_prop_data$Reference,  levels = c("Control", "N", "P", "NP"))
conf_prop_data$Prediction <- factor(conf_prop_data$Prediction, levels = c("Control", "N", "P", "NP"))

# Color palette
col <- colorRampPalette(c("black", "brown", "orange", "olivedrab", "darkgreen"))

# Plot
fig3 <- ggplot(conf_prop_data, aes(x = Reference, y = Prediction, fill = Proportion)) +
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
    y = "Predicted class"
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
  coord_fixed()

fig3

ggsave("figure_3.png", fig3, 
       width = 6, height = 4, dpi = 300, bg = "white")


