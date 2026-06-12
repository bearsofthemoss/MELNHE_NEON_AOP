library(dplyr)
library(caret)

# Select age group

dati <- read.csv(here::here("data_folder","processed_spectra2.csv"))
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

## Count tops
count_tops <- as.data.frame(table(dati$Treatment, dati$Stand))
count_tops <- count_tops[count_tops$Freq>0,]
count_tops$Stand <- count_tops$Var2
print(count_tops)


# ============================================================
# LEAVE ONE STAND OUT (LOSO) CROSS-VALIDATION
# Replaces the 75/25 random partition approach
# ============================================================

# --- Step 0: Verify pixel counts per stand ---
# (run this first to confirm sample sizes before modeling)

stand_counts <- aggregate(Freq ~ Stand, data = count_tops, FUN = sum)
print(stand_counts)

cat("\nUnique stands:\n")
print(unique(dati$Stand))

names(dati)

cat("\nPer-stand tree counts in dati_complete (after removing NA spectra):\n")
spec_complete <- complete.cases(dati[, 8:352])
dati_complete <- dati[spec_complete, ]
print(table(dati_complete$Stand))


cat("\nPer stand x treatment counts:\n")
print(table(dati_complete$Stand, dati_complete$Treatment))


# ============================================================
# Setup (unchanged from your original)
# ============================================================

stands <- c("C1","C2","C3","C4","C5","C6","C7","C8","C9")

dati$statr <- paste(dati$Stand, dati$Treatment)

ctrl_cv <- trainControl(method = "repeatedcv",
                        repeats = 10, number = 5,
                        summaryFunction = multiClassSummary)

compi  <- 20   # max components to test

spec_complete <- complete.cases(dati[, 8:352])
spec          <- as.matrix(dati[spec_complete, 8:352])
classi        <- as.factor(dati$Treatment[spec_complete])
dati_complete <- dati[spec_complete, ]

cat("\nTotal samples after removing incomplete cases:", nrow(spec), "\n\n")


# ============================================================
# STEP 1 — Component selection via LOSO
# For each fold, train on 8 stands, evaluate components
# ============================================================

mods  <- list()   # one caret 'train' object per stand fold

for (s in seq_along(stands)){
  
  cat("Component tuning — leaving out stand:", stands[s], "\n")
  flush.console()
  
  # Logical index: TRUE = training (all stands except the held-out one)
  inTrain <- dati_complete$Stand != stands[s]
  
  traini     <- spec[inTrain, ]
  testi      <- spec[!inTrain, ]
  trainclass <- classi[inTrain]
  testclass  <- classi[!inTrain]
  
  cat("  Train n =", nrow(traini), "| Test n =", nrow(testi), "\n")
  
  set.seed(1234)
  plsFit <- train(traini, trainclass,
                  method     = "pls",
                  tuneLength = compi,
                  trControl  = ctrl_cv)
  
  mods[[s]] <- plsFit
}
names(mods) <- stands


# ============================================================
# Select optimal number of components (Tukey HSD, same logic)
# ============================================================

ncomps <- vapply(mods, function(m) m$finalModel$ncomp, numeric(1))

kappas <- data.frame(ncomps = 1:compi,
                     matrix(NA, nrow = compi, ncol = length(mods)))

for (i in seq_along(mods)){
  kappas[, i + 1] <- mods[[i]]$results$Kappa
}

kapp        <- as.data.frame(as.numeric(t(kappas[, -1])))
kapp        <- cbind(kapp, rep(1:compi, each = length(mods)))
names(kapp) <- c("Kappa", "ncomps")
kapp$ncomps <- as.factor(kapp$ncomps)

modi    <- lm(Kappa ~ ncomps, kapp)
tuk     <- agricolae::HSD.test(modi, "ncomps")
tuk_dat <- as.data.frame(tuk$groups)
tuk_dat$var <- as.numeric(row.names(tuk_dat))
tuk_dat <- tuk_dat[order(tuk_dat$var), ]


# select best number of components
tuk_dat <- tuk_dat[order(tuk_dat$groups),]
top_group <- tuk_dat$groups[1]
components_in_top_group <- tuk_dat$var[tuk_dat$groups == top_group]
opt_comp <- min(components_in_top_group)

opt_comp <- min(tuk_dat$var[tuk_dat$groups == top_group])


####

par(bty = "l")
boxplot(kapp$Kappa ~ kapp$ncomps, 
        ylim = c(0, max(kapp$Kappa) + 0.1),
        xlab = "Number of components", 
        ylab = "Kappa")
text(x = 1:compi, y = rep(max(kapp$Kappa) + 0.05, compi), letters)


cat("\nOptimal components (Tukey):", opt_comp, "\n")

# Cross-check with modal value
opt_comp_modal <- as.numeric(names(sort(table(ncomps), decreasing = TRUE)[1]))
cat("Optimal components (modal):", opt_comp_modal, "\n")
cat("Using:", opt_comp, "components for final LOSO models\n\n")


# ============================================================
# STEP 2 — Final LOSO models at opt_comp
# ============================================================

finmods <- list()   # final model per held-out stand

for (s in seq_along(stands)){
  
  cat("Final model — leaving out stand:", stands[s], "\n")
  flush.console()
  
  inTrain <- dati_complete$Stand != stands[s]
  
  training   <- spec[inTrain, ]
  trainclass <- as.factor(classi[inTrain])
  
  cat("  Train n =", nrow(training), "\n")
  
  set.seed(1234)
  finalModel <- train(training, trainclass,
                      method    = "pls",
                      tuneGrid  = data.frame(ncomp = opt_comp),
                      trControl = ctrl_cv)
  
  finmods[[s]] <- finalModel
}
names(finmods) <- stands


# ============================================================
# STEP 3 — Predictions & confusion matrices per held-out stand
# ============================================================

probis <- list()
confus <- list()

for (s in seq_along(stands)){
  
  cat("Validating on held-out stand:", stands[s], "\n")
  flush.console()
  
  inTrain   <- dati_complete$Stand != stands[s]
  testing   <- spec[!inTrain, ]
  testclass <- as.factor(classi[!inTrain])
  
  cat("  Test n =", nrow(testing), "\n")
  
  plsClasses <- predict(finmods[[s]], newdata = testing)
  plsProbs   <- predict(finmods[[s]], newdata = testing, type = "prob")
  
  confus[[s]] <- caret::confusionMatrix(data = plsClasses, testclass)
  
  probs        <- as.data.frame(plsProbs)
  names(probs) <- sapply(strsplit(names(probs), "\\."), "[", 1)
  probs        <- cbind(testclass, probs)
  probis[[s]]  <- probs
}
names(confus) <- stands
names(probis) <- stands


# ============================================================
# STEP 4 — Overall LOSO accuracy & kappa
# ============================================================

accu_v  <- vapply(confus, function(cm) cm$overall["Accuracy"], numeric(1))
kappa_v <- vapply(confus, function(cm) cm$overall["Kappa"],    numeric(1))

# Per-stand summary table
loso_results <- data.frame(
  Stand    = stands,
  Accuracy = round(accu_v, 4),
  Kappa    = round(kappa_v, 4)
)
loso_results$tree_count <- stand_counts$Freq[match(loso_results$Stand, stand_counts$Stand)]


print(loso_results)


# Overall statistics
cat("\n--- Overall LOSO Performance ---\n")
cat("Mean Accuracy:", round(mean(accu_v), 4),
    " SD:", round(sd(accu_v), 4), "\n")
cat("Mean Kappa:   ", round(mean(kappa_v), 4),
    " SD:", round(sd(kappa_v), 4), "\n")


# ============================================================
# STEP 5 — Pooled confusion table (sum across all 9 folds)
# ============================================================

tabs    <- lapply(confus, function(cm) cm$table)
tabsi   <- Reduce('+', tabs)
tab_mean <- as.data.frame.matrix(tabsi / length(confus))

cat("\nPooled confusion table (averaged over 9 LOSO folds):\n")
print(tab_mean)



### Calculate VIP scores using mixOmics - Leave One Stand Out ###
vip_mixo_list <- list()

for (s in seq_along(stands)){
  cat("Calculating VIP scores - leaving out stand:", stands[s], "\n")
  flush.console()
  
  inTrain    <- dati_complete$Stand != stands[s]
  training   <- spec[inTrain, ]
  trainclass <- as.factor(classi[inTrain])
  
  # Build mixOmics model with same parameters as caret
  mixo_model <- mixOmics::plsda(training, trainclass, ncomp = opt_comp)
  
  # Get VIP scores
  vip_scores        <- mixOmics::vip(mixo_model)
  vip_mixo_list[[s]] <- vip_scores[, 1]  # Extract first column (VIP values)
}
names(vip_mixo_list) <- stands

# Average VIP scores across all 9 LOSO folds
vip_mixo_matrix <- do.call(cbind, vip_mixo_list)
vip_mixo_mean   <- rowMeans(vip_mixo_matrix)
vip_mixo_sd     <- apply(vip_mixo_matrix, 1, sd)

# Create VIP results data frame
vip_results_mixo <- data.frame(
  variable = names(vip_mixo_mean),
  vip_mean = vip_mixo_mean,
  vip_sd   = vip_mixo_sd
)

validation = data.frame(
  Age =  "LOSO",
  accuracy_mean = round(mean(accu_v), 4),
  accuracy_sd = round(sd(accu_v), 4),
  kappa_mean = round(mean(kappa_v), 4),
  kappa_sd = round(sd(kappa_v), 4),
  nsims = 9,  # leave one stand out, 9 folds
  ncomp_opt = opt_comp)

validation


plsda_out <- here::here("R_output","PLSDA_output", "LOSO all age")

if(!dir.exists(plsda_out)){ 
  dir.create(plsda_out, recursive = TRUE)
}


write.csv(loso_results, file.path(plsda_out,"loso_results_by_stand.csv"))

write.csv(tab_mean, file.path(plsda_out,"prop_treatment_plsda_loso.csv"))
write.csv(tabsi, file.path(plsda_out,"count_treatment_plsda_loso.csv"))
write.csv(validation, file.path(plsda_out,"results_summary_plsda_loso.csv"), row.names = FALSE)
write.csv(vip_results_mixo, file.path(plsda_out,"vip_scores_loso.csv"), row.names = FALSE)
write.csv(tuk_dat, file.path(plsda_out,"tukey_component_letters_loso.csv"), row.names = FALSE)


######





#################



avg <-  read.csv(here::here("R_output","PLSDA_output","LOSO all age","results_summary_plsda_loso.csv"))


los <-  read.csv(here::here("R_output","PLSDA_output","LOSO all age","loso_results_by_stand.csv"))

los$Age[los$Stand=="C1"]<-"Young forest"
los$Age[los$Stand=="C2"]<-"Young forest"
los$Age[los$Stand=="C3"]<-"Young forest"
los$Age[los$Stand=="C4"]<-"Mid-aged forest"
los$Age[los$Stand=="C5"]<-"Mid-aged forest"
los$Age[los$Stand=="C6"]<-"Mid-aged forest" 
los$Age[los$Stand=="C7"]<-"Mature forest"
los$Age[los$Stand=="C8"]<-"Mature forest"
los$Age[los$Stand=="C9"]<-"Mature forest"

los$Age <- factor(los$Age, levels=c("Young forest", "Mid-aged forest","Mature forest"))

coloso <- colorRampPalette(c("black","black","brown","gold","olivedrab"))
                                    
stlo <- ggplot(los, aes(x=Stand, y=Accuracy*100, fill=Kappa))+
  geom_col(name="Kappa")+
  facet_wrap(~Age, scales="free_x")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_fill_gradientn(
    colors = coloso(20),
    name = "Kappa value",
    limits = c(0, 0.5),
    breaks = c(0, .1, .2, .3, .4),
    labels = c("0", ".1", ".2", ".3", ".4"))+
  labs(x="Stand",y="Accuracy (%)")

stlo
# ggplot2 Confusion Matrix Heatmap from CSV
library(ggplot2)
library(reshape2)
library(dplyr)

# Read the confusion matrix data from CSV


# Read proportion data
count_data <- read.csv(here::here("R_output","PLSDA_output","LOSO all age","count_treatment_plsda_loso.csv"))


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
col <- colorRampPalette(c("black","black","brown","gold","olivedrab","darkgreen"))

conf_prop_data$Prediction <- factor(conf_prop_data$Prediction, levels=c("NP","P","N","Control"))

# Plot
fig3_loso <- ggplot(conf_prop_data, aes(x = Reference, y = Prediction, fill = Proportion)) +
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
  coord_fixed()+
  ggtitle("Leave one stand out using all 9 stands")

fig3_loso

library(patchwork)
fig3_loso + stlo




ggsave("figure_3_loso.png", fig3_loso, 
       width = 6, height = 4, dpi = 300, bg = "white")
###########


#  impo bands
vip<-  read.csv(here::here("R_output","PLSDA_output","LOSO all age","vip_scores_loso.csv"))


library(ggplot2)
# Variable importance for each age class
wvl <- colnames(read.csv(here::here("data_folder","processed_spectra.csv")))

# select wvl columns
wvl <- wvl[8:352]
wvl_nm <- round(as.numeric(sapply(strsplit(wvl, '_'), `[`, 2)),0)
vip$wvl <- wvl_nm



#one thing to add here is the actual wavelengths. Maybe also the avg spectral profile value
vip$Age <- paste0( "All forests")

# Create a grouping variable to break lines at grey regions
vip$line_group <- NA
vip$line_group[vip$wvl < 1340] <- "1"
vip$line_group[vip$wvl > 1450 & vip$wvl < 1780] <-"2"
vip$line_group[vip$wvl > 1960] <- "3"


#############

vip$is_important_1 <-   vip$vip_mean > 1 

vip

vip <- vip[!is.na(vip$line_group),]


head(vip)
g1 <- ggplot(vip, aes(x= wvl, y= vip_mean, group = line_group))+
  geom_line()+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_point(data=vip[vip$is_important_1=="TRUE",], col="forestgreen", size=1)+
  geom_hline(yintercept=1, linetype = "dashed")+
  labs(x= "Wavelength (nm)", y="Variable Importance Value")+
  annotate('rect', xmin=1340, xmax=1455, ymin=0, ymax=2.5, alpha=.2, fill='gray')+
  annotate('rect', xmin=1790, xmax=1960, ymin=0, ymax=2.5, alpha=.2, fill='gray')+
  facet_wrap(~Age, nrow=3)+
  scale_x_continuous(breaks=seq(400, 2500, 200))+
  theme(strip.text = element_text(size = 12))
g1

