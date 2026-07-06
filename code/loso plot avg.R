library(dplyr)
library(caret)
library(mixOmics)

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("mixOmics")

set.seed(12345)

dati <- read.csv(here::here("data_folder","processed_spectra3.csv"))
dati <- dati[,-1]

min(table(dati$Treatment, dati$Stand))
max(table(dati$Treatment, dati$Stand))
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
dati <- dati_wide

  #dati #  just use dati if you want individual trees

##############################################

stands <- unique(dati$Stand)

compi  <- 20   # max components to test; cap lower given small n

ber_mat <- matrix(NA, nrow = compi, ncol = length(stands))
colnames(ber_mat) <- stands

names(dati)

# indiv tree here 
# spec_complete <- complete.cases(dati[, 7:351])
# spec          <- as.matrix(dati[spec_complete, 7:351])

## plot average here
spec_complete <- complete.cases(dati[, 4:348])
spec          <- as.matrix(dati[spec_complete, 4:348])

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





# 
#############################
# 
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


 fig_loso_trees
ggsave("Figure_3.png", fig_loso_trees,
        width = 10, height = 4, dpi = 300, bg = "white")



library(patchwork)



############################

# # Extract VIP from each LOSO model
# # finmods should be your list of 9 plsda fits from the validation loop
vip_list <- lapply(finmods, function(m) vip(m)[, 1])

vip_mat           <- do.call(cbind, vip_list)
rownames(vip_mat) <- as.numeric(sub(".*_", "", names(spec[1, ])))


vip_mean_df <- data.frame(
  wavelength = as.numeric(rownames(vip_mat)),
  VIP_mean   = rowMeans(vip_mat),
  VIP_sd     = apply(vip_mat, 1, sd),
  VIP_upper      = vip_mean + vip_sd,
  VIP_lower      = vip_mean - vip_sd
)

vip_mean_df$line_group <- NA
vip_mean_df$line_group[vip_mean_df$wavelength < 1340] <- "1"
vip_mean_df$line_group[vip_mean_df$wavelength > 1450 & vip_mean_df$wavelength < 1780] <- "2"
vip_mean_df$line_group[vip_mean_df$wavelength > 1960] <- "3"

#############
vip_mean_df$is_important_1 <-   vip_mean_df$VIP_mean > 1 


vip_mean_df <- vip_mean_df[!is.na(vip_mean_df$line_group),]



g1 <- ggplot(vip_mean_df, aes(x= wavelength, y= VIP_mean, group = line_group))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  #  geom_point(data=vip[vip$is_important_1=="TRUE",], col="forestgreen", size=1)+
  geom_hline(yintercept=1, linetype = "dashed")+
  labs(x= "Wavelength (nm)", y="Variable Importance Value")+
  annotate('rect', xmin=1340, xmax=1455, ymin=0, ymax=3, alpha=.2, fill='gray')+
  annotate('rect', xmin=1790, xmax=1960, ymin=0, ymax=3, alpha=.2, fill='gray')+
  #  facet_wrap(~Age, nrow=4)+
  geom_vline(xintercept=535, linetype="solid", col="green",linewidth=2)+
  geom_vline(xintercept=734, linetype="solid", col="green",linewidth=2)+
  geom_vline(xintercept=985, linetype="solid", col="green",linewidth=2)+
  scale_x_continuous(breaks=seq(400, 2500, 200))+
  geom_line()+
  annotate(geom = "text", x = 535, y = 2.5, label = "Xanthophyll", 
           angle = 90,        # Rotates text 90 degrees counter-clockwise
           vjust = -0.5,      # Adjusts spacing to prevent overlap
           color = "black",  size = 5)+
  annotate(geom = "text", x = 735, y = 2.5, label = "Red edge", 
           angle = 90,        # Rotates text 90 degrees counter-clockwise
           vjust = -0.5,      # Adjusts spacing to prevent overlap
           color = "black",  size = 5)+
  annotate(geom = "text", x = 985, y = 2.5, label = "Starch", 
           angle = 90,        # Rotates text 90 degrees counter-clockwise
           vjust = -0.5,      # Adjusts spacing to prevent overlap
           color = "black",  size = 5)
  
  g1

write.csv(vip_mean_df, file="plot avg LOSO VIP.csv")

ggsave("Figure_4.png", g1,
       width = 7, height = 4, dpi = 300, bg = "white")



##########

# Each confus[[s]] corresponds to stands[s]
# Assign age to each stand's result
stand_ages <- c(C1="Young forest", C2="Young forest", C3="Young forest",
                C4="Mid-aged forest", C5="Mid-aged forest", C6="Mid-aged forest",
                C7="Mature forest", C8="Mature forest", C9="Mature forest")


# # 1. Score plot — 36 points positioned by comp1/comp2

# Collect held-out stand scores across all 9 folds
scores_list <- list()

for (s in seq_along(stands)) {
  inTrain   <- dati_complete$Stand != stands[s]
  testi     <- spec[!inTrain, ]
  testclass <- classi[!inTrain]
  test_stand <- dati_complete$Stand[!inTrain]
  
  # Project test data onto training model's components
  preds <- predict(finmods[[s]], newdata = testi)
  
  scores_list[[s]] <- data.frame(
    comp1     = preds$variates[, 1],
    comp2     = preds$variates[, 2],
    Treatment = testclass,
    Stand     = test_stand
  )
}

scores_df <- do.call(rbind, scores_list)
scores_df$Age <- stand_ages[scores_df$Stand]



### ANOVA on LD1

scores_df$Treatment<-factor(scores_df$Treatment, levels=c("Control","N","P","NP"))
scores_df$Ntrmt <- factor(  ifelse(scores_df$Treatment == "N" | scores_df$Treatment == "NP", "N", "NoN"))
scores_df$Ptrmt <- factor(  ifelse(scores_df$Treatment %in% c("P", "NP"), "P", "NoP"))


ld1_mod <-  lmer( comp1 ~ Ntrmt*Ptrmt*Age + (1| Stand), data = scores_df)
anova(ld1_mod)

ld2_mod <-  lmer( comp2 ~ Ntrmt*Ptrmt*Age + (1| Stand), data = scores_df)
anova(ld2_mod)


####

# Collect held-out stand scores across all 9 folds
scores_list <- list()

for (s in seq_along(stands)) {
  inTrain   <- dati_complete$Stand != stands[s]
  testi     <- spec[!inTrain, ]
  testclass <- classi[!inTrain]
  test_stand <- dati_complete$Stand[!inTrain]
  
  # Project test data onto training model's components
  preds <- predict(finmods[[s]], newdata = testi)
  
  scores_list[[s]] <- data.frame(
    comp1     = preds$variates[, 1],
    comp2     = preds$variates[, 2],
    Treatment = testclass,
    Stand     = test_stand
  )
}

scores_df <- do.call(rbind, scores_list)
scores_df$Age <- stand_ages[scores_df$Stand]

###

scores_df$Treatment <- factor(scores_df$Treatment, levels=c("Control","N","P","NP"))

scores_df$Age <- factor(scores_df$Age, levels=c("Young forest","Mid-aged forest", "Mature forest"))
p_scores <- ggplot(scores_df, aes(x = comp1, y = comp2,
                                  color = Treatment)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Treatment), linewidth = 0.6) +
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(x = "Component 1", y = "Component 2") +
  theme_bw()+
  coord_fixed()+
  geom_vline(xintercept = 0, linetype="dashed")+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme(legend.position = "right",
        panel.grid = element_blank())+
  facet_wrap(~Age, nrow=1)

p_scores
# 2. Loading spectrum — comp1 and comp2 as lines, colored by sign
loadings_long <- loadings_df %>%
  tidyr::pivot_longer(cols      = c(comp1, comp2),
                      names_to  = "Component",
                      values_to = "Loading")

#####
table(loadings_long$wavelength)
loadings_long$line_group <- NA
loadings_long$line_group[loadings_long$wavelength < 1340] <- "1"
loadings_long$line_group[loadings_long$wavelength > 1451 & loadings_long$wavelength < 1781] <- "2"
loadings_long$line_group[loadings_long$wavelength > 1965] <- "3"


table(loadings_long$wavelength)
sum(table(loadings_long$line_group))
dim(loadings_long)
#############

loadings_long <- loadings_long[!is.na(loadings_long$line_group),]

loadings_long$comp_line <- paste(loadings_long$Component, loadings_long$line_group)

########

loadings_long[loadings_long$Component=="comp1","Component"] <- "Component 1"
loadings_long[loadings_long$Component=="comp2","Component"] <- "Component 2"

p_loadings <- ggplot(loadings_long,
                     aes(x = wavelength, y = Loading, group=comp_line)) +
  geom_line(linewidth = 0.7, aes(linetype = Component), col="black") +
 geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  # scale_color_manual(values = c(comp1 = "darkgreen", comp2 = "steelblue")) +
  labs(x = "Wavelength (nm)", y = "PLSDA Loading") +
  theme_bw() +
  ylim(-.15, .2)+
  facet_wrap(~Component, nrow=2)+
  theme(panel.grid=element_blank())+
  theme(legend.position = "right")
p_loadings


# 3. Stack them# 3. Stack them# 3. Stack them
library(patchwork)
p_scores / p_loadings


ggsave("loadings plot avg loso.png", 
       width = 10, height = 4, dpi = 300, bg = "white")


