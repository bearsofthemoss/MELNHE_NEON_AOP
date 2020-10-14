###########################################
### PLSDA: Bartlett Tree tops #############
### Anna Schweiger, April 17 2020 ########
library(caret)
library(reshape)
library(ggplot2)
library(corrplot)
library(agricolae)
library(tidyverse)

dat <- read.csv("./data_folder/actual_tops_10_04_greater_0.1.csv", row.names = 1)
dat <- dat[complete.cases(dat),] ### remove NAs

age <- read.csv("./data_folder/age_classes.csv")

dati <- dat %>% left_join(age, by="Stand") %>%
  mutate(treat_age=paste(Treatment, Age, sep = "_")) %>%
  select(1:6,352:353,7:351) 

### Prepare data
wv <- colnames(dati)[9:ncol(dati)] ### define wvl range for spectral matrix, check your column names
wvl <- substr(wv,6,nchar(wv))
spec <- dati[,9:ncol(dati)] ### make spectral matrix

#### PLSDA differentiating AGE CLASSES #######
classi <- as.factor(dati$Age) ### define classes 

rndid <- list() ### list of random ID's for partitioning data into cal and val
set.seed(1840)
for (i in 1:100){ 
  rndid[[i]] <- with(dati, ave(1:nrow(dati), treat_age, FUN=function(x) {sample.int(length(x))}))
}

nsims <- 100 ### number of iterations, try 50, or 100?
compi <- 15 ### max number of components, something to play with, too many and the model crashes
ctrl <- trainControl(method = "repeatedcv", repeats = 10, number=10,
                     summaryFunction = multiClassSummary)
mods <- list()

for (nsim in seq(nsims)){ ### not sure how to avoid the row names warning, shouldn't matter though
  print(nsim)
  flush.console()
  set.seed(nsim)
  inTrain <- rndid[[nsim]]<= round((min(table(dati$treat_age))/100)*75,
                                   digits = 0) ### Select number of samples per class for training, e.g. 75% of smallest class
  # inTrain <- createDataPartition(y = classi, p = .75, list = FALSE) ## Classes of equal size: 75% for training
  traini <- spec[inTrain,] 
  testi <- spec[!(inTrain),]
  trainclass <- classi[inTrain]
  testclass <- classi[!(inTrain)]
  plsFit <- train(traini, trainclass, method = "pls", tuneLength = compi,
                  trControl = ctrl)
  # trControl = trainControl(method="LOOCV")  ### use leaf-one-out CV for small sample sizes
   mods[[nsim]] <- plsFit
}

### Sample overview
obs_cal <- dati[inTrain,] 
table(obs_cal$treat_age)

obs_val <- dati[!inTrain,] 
table(obs_val$treat_age)


### Select number of components 
ncomps <- vector(length = nsims)
for (i in 1:nsims){
  ncomps[i]<-mods[[i]]$finalModel$ncomp
}
table(ncomps) ### how often the indicated number of comps was selected by the model

### Kappa statistics ### more detailed approach for deciding ncomp
kappas <- data.frame(ncomps= 1:compi,matrix(NA, nrow = compi, ncol = length(mods)))
for (i in 1:length(mods)){
  kappas[,i+1] <- mods[[i]]$results$Kappa
}

### Tukey test
kapp <- as.data.frame(as.numeric(t(kappas[,-1])))
kapp <- cbind(kapp, rep(1:compi, each=length(mods)))
names(kapp) <- c("Kappa", "ncomps")

kapp$ncomps <- as.factor(kapp$ncomps)

modi <- lm (Kappa~ncomps, kapp)
tuk <- HSD.test (modi,"ncomps")

tuk_dat <- as.data.frame(tuk$groups)
tuk_dat$var <- as.numeric(row.names(tuk_dat))
tuk_dat <- tuk_dat[order(tuk_dat$var,decreasing = F),]
letters <- as.character(tuk_dat$groups)

#### Kappa plot
pdf("./R_output/PLSDA_kappas_age.pdf",width = 5,height = 4)
par(bty="l")
boxplot(kapp$Kappa~kapp$ncomps,ylim=c(0,1.1),
        xlab="Number of components",ylab="Kappa")
text(x=1:20, y=rep(1,20),letters)
dev.off()

#####################
#### Final model ###
compi <- 11 ### select number of components
finmods <- list()
nsims=100

for (nsim in 1:nsims){
  print(nsim)
  flush.console()
  set.seed(nsim)
  # inTrain <- rndid[[nsim]]<= 25
  # inTrain <- createDataPartition(y =classi, p = .75, list = FALSE)
  inTrain <- rndid[[nsim]]<= round((min(table(dati$treat_age))/100)*75,
                                   digits = 0)
  training <- spec[inTrain,]
  testing <- spec[!(inTrain),]
  trainclass <- as.factor(classi[inTrain]) 
  testclass <- as.factor(classi[!(inTrain)])
  finalModel <- plsda(training,trainclass, ncomp=compi, 
                      method = "simpls")
  finmods[[nsim]] <- finalModel
}
saveRDS(finmods, "./R_output/finmods_age_11comps.rds")

### Probabilities and confusion matrix
probis <- list()
confus <- list()

### Predictions Model Validation
for (nsim in seq(nsims)){
  print(nsim)
  flush.console()
  set.seed(nsim)
  # inTrain <- rndid[[nsim]]<= nsims
  # inTrain <- createDataPartition(y = classi, p = .80, list = FALSE)
  inTrain <- rndid[[nsim]]<= round((min(table(dati$treat_age))/100)*75,
                                   digits = 0)
  testing <- spec[!(inTrain),]
  testclass <- as.factor(classi[!(inTrain)])
  
  plsProbs <- predict(finmods[[nsim]], newdata = testing, type = "prob")
  plsClasses <- predict(finmods[[nsim]], newdata = testing)
  confus[[nsim]] <- confusionMatrix(data = plsClasses, testclass)
  
  probs <- as.data.frame(plsProbs)
  names(probs) <- sapply(strsplit(names(probs),split = "\\."),"[",1)
  probs <- cbind(testclass, probs)
  probis[[nsim]] <- probs 
}


### Model stats ##
### Accuracy Calibration (model fit) ... but we are more interested in predictive accuracy (validation)
accu <- numeric(length=nsims)
kappa <- numeric(length=nsims)
for (i in 1:nsims){
  accu[i] <- mods[[i]]$results$Accuracy[compi]
  kappa[i] <- mods[[i]]$results$Kappa[compi]
}

(accmean <- mean(accu))
(accsd <- sd(accu))

(kappamean <- mean(kappa))
(kappasd <- sd(kappa))

### Accuracy Validation
accu_v <- numeric(length=nsims)
kappa_v <- numeric(length=nsims)
for (i in 1:nsims){
  accu_v[i] <- confus[[i]]$overall[1] 
  kappa_v[i] <- confus[[i]]$overall[2]
}

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
write.csv(tab_mean,"./R_output/PLSDA_confumean_age_11comps.csv")

sums <- colSums(tab_mean)
tabs_perc <- matrix(NA, length(sums),length(sums))
for (i in 1:length(sums)){
  tabs_perc[,i] <- tab_mean[,i]/sums[i]
}

colnames(tabs_perc) <- colnames(confus[[1]]$table)
rownames(tabs_perc) <- rownames(confus[[1]]$table)
write.csv(tabs_perc,"./R_output/PLSDA_confuperc_age_11comps.csv")

col <- colorRampPalette(c("black","black","brown","gold","forestgreen")) 

pdf("./R_output/PLSDA_corrplot_age_11comps.pdf",width = 7,height = 6,pointsize = 13)
corrplot::corrplot(tabs_perc, p.mat = tabs_perc, insig = "p-value", sig.level = -1, addCoef.col = 1,
         tl.srt = 70,col = col(20),cl.lim = c(0, 1),tl.col = 1, tl.offset =1.5, 
         cl.ratio = 0.2, cl.align.text = "l", cl.cex = 0.9, 
         mar=c(1,3,3,3))
mtext("Prediction",2,at=3, line=-3, cex=1.3)
mtext("Reference",at = 2, line = 0, cex=1.3)
dev.off()

#### Importance of bands, loadings
lls <- list()
for(i in 1:length(finmods)){
  lls[[i]] <- abs(loadings(finmods[[i]])[1:dim(loadings(finmods[[1]]))[1],1:compi])
  sumis <- lapply(lls,rowSums)
}

mm <- apply(simplify2array(sumis), 1, mean)
ss <- apply(simplify2array(sumis), 1, sd)

mm <- as.data.frame(mm)
mm <- cbind(mm,ss)

mm$ww <- as.numeric(substr(row.names(mm), 6, nchar(row.names(mm))))
row.names(mm)<- NULL

names(mm) <- c("mean_abs_loading", "sd_abs_loading", "wavelength")
write.csv(mm, "./R_output/PLSDA_abs_loadings_age_11comps.csv", row.names = F)


################################
##### TREATMENT ###############
### Prepare data
classi <- as.factor(dati$Treatment) ### define classes (age classes, treatments)

#### PLSDA differentiating TREATMENT CLASSES #######
rndid <- list() ### list of random ID's for partitioning data into cal and val
set.seed(1840)
for (i in 1:100){ 
  rndid[[i]] <- with(dati, ave(1:nrow(dati), treat_age, FUN=function(x) {sample.int(length(x))}))
}

nsims <- 100 ### number of iterations, try 50, or 100?
compi <- 20 ### max number of components, something to play with, too many and the model crashes
ctrl <- trainControl(method = "repeatedcv", repeats = 10, number=10,
                     summaryFunction = multiClassSummary)
mods <- list()

for (nsim in seq(nsims)){ ### not sure how to avoid the row names warning, shouldn't matter though
  print(nsim)
  flush.console()
  set.seed(nsim)
  inTrain <- rndid[[nsim]]<= round((min(table(dati$treat_age))/100)*75,
                                   digits = 0)
  # inTrain <- createDataPartition(y = classi, p = .75, list = FALSE) ## Classes of equal size: 75% for training
  traini <- spec[inTrain,] 
  testi <- spec[!(inTrain),]
  trainclass <- classi[inTrain]
  testclass <- classi[!(inTrain)]
  plsFit <- train(traini, trainclass, method = "pls", tuneLength = compi,
                  trControl = ctrl)
  # trControl = trainControl(method="LOOCV")  ### use leaf-one-out CV for small sample sizes
  mods[[nsim]] <- plsFit
}

obs_cal <- dati[inTrain,] 
table(obs_cal$treat_age)

obs_val <- dati[!(inTrain),] 
table(obs_val$treat_age)


### Select number of components 
ncomps <- vector(length = nsims)
for (i in 1:nsims){
  ncomps[i]<-mods[[i]]$finalModel$ncomp
}
table(ncomps) ### how often the indicated number of comps was selected by the model

### Kappa statistics ### more detailed approach for deciding ncomp
kappas <- data.frame(ncomps= 1:compi,matrix(NA, nrow = compi, ncol = length(mods)))
for (i in 1:length(mods)){
  kappas[,i+1] <- mods[[i]]$results$Kappa
}

### Tukey test
kapp <- as.data.frame(as.numeric(t(kappas[,-1])))
kapp <- cbind(kapp, rep(1:compi, each=length(mods)))
names(kapp) <- c("Kappa", "ncomps")

kapp$ncomps <- as.factor(kapp$ncomps)

modi <- lm (Kappa~ncomps, kapp)
tuk <- HSD.test (modi,"ncomps")

tuk_dat <- as.data.frame(tuk$groups)
tuk_dat$var <- as.numeric(row.names(tuk_dat))
tuk_dat <- tuk_dat[order(tuk_dat$var,decreasing = F),]
letters <- as.character(tuk_dat$groups)

#### Kappa plot
pdf("./R_output/PLSDA_kappas_treat.pdf",width = 5,height = 4)
par(bty="l")
boxplot(kapp$Kappa~kapp$ncomps,ylim=c(0,0.6),
        xlab="Number of components",ylab="Kappa")
text(x=1:20, y=rep(0.65,20),letters)
dev.off()

#####################
#### Final model ###
compi <- 10 ### select number of components
finmods <- list()
nsims=100

for (nsim in 1:nsims){
  print(nsim)
  flush.console()
  set.seed(nsim)
  # inTrain <- rndid[[nsim]]<= 25
  # inTrain <- createDataPartition(y =classi, p = .75, list = FALSE)
  inTrain <- rndid[[nsim]]<= round((min(table(dati$treat_age))/100)*75,
                                   digits = 0)
  training <- spec[inTrain,]
  testing <- spec[!(inTrain),]
  trainclass <- as.factor(classi[inTrain]) 
  testclass <- as.factor(classi[!(inTrain)])
  finalModel <- plsda(training,trainclass, ncomp=compi, 
                      method = "simpls")
  finmods[[nsim]] <- finalModel
}
saveRDS(finmods, "./R_output/finmods_treat_10comps.rds")

### Probabilities and confusion matrix
probis <- list()
confus <- list()

### Predictions Model Validation
for (nsim in seq(nsims)){
  print(nsim)
  flush.console()
  set.seed(nsim)
  # inTrain <- rndid[[nsim]]<= nsims
  # inTrain <- createDataPartition(y = classi, p = .80, list = FALSE)
  inTrain <- rndid[[nsim]]<= round((min(table(dati$treat_age))/100)*75,
                                   digits = 0)
  testing <- spec[!(inTrain),]
  testclass <- as.factor(classi[!(inTrain)])
  
  plsProbs <- predict(finmods[[nsim]], newdata = testing, type = "prob")
  plsClasses <- predict(finmods[[nsim]], newdata = testing)
  confus[[nsim]] <- confusionMatrix(data = plsClasses, testclass)
  
  probs <- as.data.frame(plsProbs)
  names(probs) <- sapply(strsplit(names(probs),split = "\\."),"[",1)
  probs <- cbind(testclass, probs)
  probis[[nsim]] <- probs 
}


### Model stats ##
### Accuracy Calibration (model fit) ... but we are more interested in predictive accuracy (validation)
accu <- numeric(length=nsims)
kappa <- numeric(length=nsims)
for (i in 1:nsims){
  accu[i] <- mods[[i]]$results$Accuracy[compi]
  kappa[i] <- mods[[i]]$results$Kappa[compi]
}

(accmean <- mean(accu))
(accsd <- sd(accu))

(kappamean <- mean(kappa))
(kappasd <- sd(kappa))

### Accuracy Validation
accu_v <- numeric(length=nsims)
kappa_v <- numeric(length=nsims)
for (i in 1:nsims){
  accu_v[i] <- confus[[i]]$overall[1] 
  kappa_v[i] <- confus[[i]]$overall[2]
}

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
write.csv(tab_mean,"./R_output/PLSDA_confumean_treat_10comps.csv")

sums <- colSums(tab_mean)
tabs_perc <- matrix(NA, length(sums),length(sums))
for (i in 1:length(sums)){
  tabs_perc[,i] <- tab_mean[,i]/sums[i]
}

colnames(tabs_perc) <- colnames(confus[[1]]$table)
rownames(tabs_perc) <- rownames(confus[[1]]$table)
write.csv(tabs_perc,"./R_output/PLSDA_confuperc_treat_10comps.csv")

col <- colorRampPalette(c("black","black","brown","gold","forestgreen")) 

pdf("./R_output/PLSDA_corrplot_treat_10comps.pdf",width = 7,height = 6,pointsize = 13)
corrplot::corrplot(tabs_perc, p.mat = tabs_perc, insig = "p-value", sig.level = -1, addCoef.col = 1,
                   tl.srt = 70,col = col(20),cl.lim = c(0, 1),tl.col = 1, tl.offset =1.5, 
                   cl.ratio = 0.2, cl.align.text = "l", cl.cex = 0.9, 
                   mar=c(1,3,3,3))
mtext("Prediction",2,at=3, line=-3, cex=1.3)
mtext("Reference",at = 2, line = 0, cex=1.3)
dev.off()

#### Importance of bands, loadings plot
lls <- list()
for(i in 1:length(finmods)){
  lls[[i]] <- abs(loadings(finmods[[i]])[1:dim(loadings(finmods[[1]]))[1],1:compi])
  sumis <- lapply(lls,rowSums)
}

mm <- apply(simplify2array(sumis), 1, mean)
ss <- apply(simplify2array(sumis), 1, sd)

mm <- as.data.frame(mm)
mm <- cbind(mm,ss)

mm$ww <- as.numeric(substr(row.names(mm), 6, nchar(row.names(mm))))
row.names(mm)<- NULL

names(mm) <- c("mean_abs_loading", "sd_abs_loading", "wvl")
write.csv(mmx, "./R_output/PLSDA_abs_loadings_treat_10comps.csv", row.names = F)

#### END ##########
