library(dplyr)
library(caret)

# Select age group
sel_stand_age <- "Mid-aged forest"
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
dati <- dati[dati$Age==sel_stand_age, ]

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
                  trControl = trainControl(method="LOOCV"))
  
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

### Kappa plot ###
pdf("./R_output/PLSDA_kappas_treat.pdf", width = 5, height = 4)
par(bty = "l")
boxplot(kapp$Kappa ~ kapp$ncomps, 
        ylim = c(0, max(kapp$Kappa) + 0.1),
        xlab = "Number of components", 
        ylab = "Kappa")
text(x = 1:compi, y = rep(max(kapp$Kappa) + 0.05, compi), letters)
dev.off()

### Determine final number of components ###
opt_comp <- as.numeric(names(sort(table(ncomps), decreasing = TRUE)[1]))

cat("\nUsing", opt_comp, "components for final models\n\n")

finmods <- list()

for (nsim in 1:nsims){
  cat("Final model iteration:", nsim, "\n")
  flush.console()
  set.seed(nsim)
  
  inTrain <- rndid[[nsim]][spec_complete] <= train_min_75
  
  training <- spec[inTrain, ]
  testing <- spec[!inTrain, ]
  trainclass <- as.factor(classi[inTrain]) 
  testclass <- as.factor(classi[!inTrain])
  
  # Build final PLSDA model using caret with LOOCV
  finalModel <- train(training, trainclass,
                      method = "pls",
                      tuneGrid = data.frame(ncomp = opt_comp),
                      trControl = trainControl(method = "LOOCV"))
  
  finmods[[nsim]] <- finalModel
}

# Save models
saveRDS(finmods, paste0("./R_output/finmods_treat_", opt_comp, "comps.rds"))

### Model Validation - Predictions on test sets ###
probis <- list()
confus <- list()

for (nsim in 1:nsims){
  cat("Validation iteration:", nsim, "\n")
  flush.console()
  set.seed(nsim)
  
  inTrain <- rndid[[nsim]][spec_complete] <= train_min_75
  
  testing <- spec[!inTrain, ]
  testclass <- as.factor(classi[!inTrain])
  
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

## Calibration Performance (Training) ##
accu_cal <- numeric(length = nsims)
kappa_cal <- numeric(length = nsims)

for (i in 1:nsims){
  accu_cal[i] <- mods[[i]]$results$Accuracy[opt_comp]
  kappa_cal[i] <- mods[[i]]$results$Kappa[opt_comp]
}

cat("\n=== CALIBRATION PERFORMANCE (Training) ===\n")
cat("Accuracy:", round(mean(accu_cal), 3), "±", round(sd(accu_cal), 3), "\n")
cat("Kappa:", round(mean(kappa_cal), 3), "±", round(sd(kappa_cal), 3), "\n")

## Validation Performance (Testing) ##
accu_val <- numeric(length = nsims)
kappa_val <- numeric(length = nsims)

for (i in 1:nsims){
  accu_val[i] <- confus[[i]]$overall["Accuracy"]
  kappa_val[i] <- confus[[i]]$overall["Kappa"]
}

cat("\n=== VALIDATION PERFORMANCE (Testing) ===\n")
cat("Accuracy:", round(mean(accu_val), 3), "±", round(sd(accu_val), 3), "\n")
cat("Kappa:", round(mean(kappa_val), 3), "±", round(sd(kappa_val), 3), "\n")

### Average Confusion Matrix ###
tabs <- list()
for(i in 1:length(confus)){
  tabs[[i]] <- confus[[i]]$table
}

tabsi <- Reduce('+', tabs)
tab_mean <- as.data.frame.matrix(tabsi / length(confus))

cat("\n=== AVERAGE CONFUSION MATRIX ===\n")
print(round(tab_mean, 2))


####
### Calculate VIP scores across all 100 models ###

# Extract VIP scores from each model
vip_list <- list()

for (i in 1:nsims){
  # Get variable importance from caret model
  vip_list[[i]] <- varImp(finmods[[i]])$importance
}

# Average VIP scores across all iterations
vip_matrix <- do.call(cbind, vip_list)
vip_mean <- rowMeans(vip_matrix)
vip_sd <- apply(vip_matrix, 1, sd)

# Create VIP results data frame
vip_results <- data.frame(
  variable = names(vip_mean),
  vip_mean = vip_mean,
  vip_sd = vip_sd
)

plot(vip_results[, 3], type = "l",
     xlab = "Variable Index",
     ylab = "VIP Score",
     main = "Variable Importance in Projection (VIP)")
abline(h = 1, col = "red", lty = 2)


### Save results ###

  validation = data.frame(
    accuracy_mean = mean(accu_val),
    accuracy_sd = sd(accu_val),
    kappa_mean = mean(kappa_val),
    kappa_sd = sd(kappa_val))
  
  
  plsda_out <- here::here("R_output","PLSDA_output", sel_stand_age)
  
  if(!dir.exists(plsda_out)){ 
    dir.create(plsda_out, recursive = TRUE)
  }

  
  write.csv(tab_mean, file.path(plsda_out,"prop_treatment_plsda.csv"))
  write.csv(tabsi, file.path(plsda_out,"count_treatment_plsda.csv"))
  write.csv(validation, file.path(plsda_out,"results_summary_plsda.csv"), row.names = FALSE)
  write.csv(vip_results, file.path(plsda_out,"vip_scores.csv"), row.names = FALSE)
  