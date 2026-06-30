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


vip_long$line_group <- NA
vip_long$line_group[vip_long$wavelength< 1340] <- "1"
vip_long$line_group[vip_long$wavelength > 1450 & vip_long$wavelength < 1780] <- "2"
vip_long$line_group[vip_long$wavelength > 1960] <- "3"



vip_long <- vip_long[!is.na(vip_long$line_group),]

vip_long$lgs <- paste(vip_long$Stand, vip_long$line_group)

# ### Plot
# ggplot()+
# #vip_df, aes(x = wavelength, y = VIP_mean)) +
#   geom_line(data = vip_long,
#             aes(x = wavelength, y = VIP, group = lgs),
#              linewidth = 0.5) +
#   # geom_ribbon(aes(ymin = VIP_lower, ymax = VIP_upper),
#   #             alpha = 0.3, fill = "olivedrab") +
#   geom_line(color = "darkgreen", linewidth = 0.9) +
#   geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 0.6) +
#   labs(x     = "Wavelength (nm)",
#        y     = "VIP score",
#        title = "Variable Importance — mean ± SD across 9 LOSO folds") +
#   theme_bw()+
#   facet_wrap(~Age, nrow=3)+
#   theme(panel.grid=element_blank())
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
  ggtitle("LOSO all stands- plot level averages")


 fig_loso_trees
ggsave("plot avg loso fig.png", fig_loso_trees,
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


# Plot
fig_VIP <- ggplot(vip_mean_df, aes(x = wavelength  , y = VIP_mean)) +
#  geom_ribbon(aes(ymin = VIP_lower, ymax = VIP_upper),
 #             alpha = 0.3, fill = "olivedrab") +
  geom_line(color = "black", linewidth = 0.8,
            aes(group=line_group)) +
  geom_point(data=vip_mean_df[vip_mean_df$is_important_1==TRUE,],
             aes(x=wavelength, y=VIP_mean), col="darkgreen")+
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  labs(x = "Band index",
       y = "VIP score") +
  theme_bw()+
  theme(panel.grid = element_blank())

fig_VIP

ggsave("vip_loso_fig.png", fig_VIP,
       width = 10, height = 4, dpi = 300, bg = "white")



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


