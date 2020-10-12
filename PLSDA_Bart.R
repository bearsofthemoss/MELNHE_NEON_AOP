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
# dati$Age <- factor(dati$Age, levels = c("~30 years old", "~60 years old", "~100 years old"))

dat <- dat %>% mutate(stand_treat=paste(Stand, Treatment, sep="_"))
age <- age %>% mutate(stand_treat=paste(Stand, Treatment, sep="_"))  
  
dat <- left_join(dat, age[,c(2,4)], by="stand_treat")%>% 
  select(1:6,352:353,7:351)

### subset
dati <- dati %>% group_by(stand_treat) %>% sample_n(15)
dati <- ungroup(dati)

### Prepare data
classi <- as.factor(dati$Age) ### define classes (age classes, treatments)

wv <- colnames(dati)[9:ncol(dati)] ### define wvl range for spectral matrix, check your column names
wvl <- substr(wv,6,nchar(wv))
spec <- dati[,9:ncol(dati)] ### make spectral matrix


#### PLSDA differentiating AGE CLASSES #######
rndid <- list() ### list of random ID's for partitioning data into cal and val
set.seed(1840)
for (i in 1:100){ 
  rndid[[i]] <- with(dati, ave(1:nrow(dati), Age, FUN=function(x) {sample.int(length(x))}))
}

nsims <- 100 ### number of iterations, try 50, or 100?
compi <- 15 ### max number of components, something to play with, too many and the model crashes
ctrl <- trainControl(method = "repeatedcv", repeats = 10, number=10,
                     summaryFunction = multiClassSummary)
mods <- list()

for (nsim in seq(nsims)){ ### not sure how to avoid the row names warning, shouldn't matter though
  print(nsim)
  # inTrain <- rndid[[nsim]]<= 25 ### Alternative to % partitioning,select nnumber of samples per class for training, e.g. 25 samples 
  flush.console()
  set.seed(nsim)
  inTrain <- createDataPartition(y = classi, p = .75, list = FALSE) ## 75% for training
  traini <- spec[inTrain,] 
  testi <- spec[-(inTrain),]
  trainclass <- classi[inTrain]
  testclass <- classi[-(inTrain)]
  plsFit <- train(traini, trainclass, method = "pls", tuneLength = compi,
                  trControl = ctrl)
  # trControl = trainControl(method="LOOCV")  ### use leaf-one-out CV for small sample sizes
   mods[[nsim]] <- plsFit
}

mods[[100]]

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
compi <- 10 ### select number of components
finmods <- list()
winnsims=100

for (nsim in 1:nsims){
  print(nsim)
  flush.console()
  set.seed(nsim)
  # inTrain <- rndid[[nsim]]<= 25
  inTrain <- createDataPartition(y =classi, p = .75, list = FALSE)
  training <- spec[inTrain,]
  testing <- spec[!(inTrain),]
  trainclass <- as.factor(classi[inTrain]) 
  testclass <- as.factor(classi[!(inTrain)])
  finalModel <- plsda(training,trainclass, ncomp=compi, 
                      method = "pls")
  finmods[[nsim]] <- finalModel
}
saveRDS(finmods, "./R_output/finmods_age_4comps.rds")

### Probabilities and confusion matrix
probis <- list()
confus <- list()

### Predictions Model Validation
for (nsim in seq(nsims)){
  print(nsim)
  flush.console()
  set.seed(nsim)
  inTrain <- rndid[[nsim]]<= nsims
  # inTrain <- createDataPartition(y = classi, p = .80, list = FALSE)
  testing <- spec[-(inTrain),]
  testclass <- as.factor(classi[-(inTrain)])
  
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

### for more and classwise accuracies see
# confus[[i]]

### Probability plot
arr <- array(unlist(probis), dim = c(dim(probis[[1]]),nsims))
prob_mean <- apply(arr, 1:2, mean)
prob_mean <- as.data.frame(prob_mean)
prob_mean$V1 <- probis[[1]]$testclass
colnames(prob_mean) <- colnames(probis[[1]])
write.csv(prob_mean,"./R_output/PLSDA_probmean_age_4comps.csv")


pp <- melt(prob_mean, id="testclass")
pp$position <- ifelse (pp$testclass == pp$variable, 2,1) 
pp$testclass <- factor(pp$testclass, levels=rev(levels(pp$testclass)))

coli <- c("grey60","blue3","darkviolet","red2") ### 
coli <- c("orange","green3", "darkgreen")

pdf("./R_output/probability_plot_age_4comps.pdf",width = 6,height = 4)
ggplot(pp, aes(x=testclass, y=value, fill=variable, group=position))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values= alpha(coli,1))+
  theme(legend.title=element_blank())+
  labs(x = "Species", y= "")+
  coord_flip()
dev.off()

### Confusion table plot 
tabs <- list()
for(i in 1:length(confus)){
  tabs[[i]] <- confus[[i]]$table
}

tabsi <- Reduce('+', tabs)
tab_mean <- as.data.frame.matrix(tabsi/length(confus))
write.csv(tab_mean,"./R_output/PLSDA_confumean_age_4comps.csv")

sums <- colSums(tab_mean)
tabs_perc <- matrix(NA, length(sums),length(sums))
for (i in 1:length(sums)){
  tabs_perc[,i] <- tab_mean[,i]/sums[i]
}

colnames(tabs_perc) <- colnames(confus[[1]]$table)
rownames(tabs_perc) <- rownames(confus[[1]]$table)
write.csv(tabs_perc,"./R_output/PLSDA_confuperc_age_4comps.csv")

col <- colorRampPalette(c("black","black","brown","gold","forestgreen")) 

pdf("./R_output/PLSDA_corrplot_age_4comps.pdf",width = 7,height = 6,pointsize = 13)
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

### add NAs for masked wvls
mm$ww <- as.numeric(substr(row.names(mm), 2, nchar(row.names(mm))))
row.names(mm)<- NULL

xx <- data.frame(ww=seq(400,2400,by=5)[-c(1,400,401)])
mmx <- merge(xx,mm, all.x = T)
mmx[is.na(mmx)] <- 0  ### needs to be a value otherwise plot doesn't work
### try finding a smarter solution, otherwise line can be pasted over manually

pdf("./R_output/PLSDA_loadi_age_4comps.pdf",width = 6,height = 4)
plot(1:nrow(mmx), mmx$mm, type="n", bty="l", ylab="abs (loadings)", 
    xaxt="n", xlab="Wavelength (nm)")
polygon(x=c(1:nrow(mmx),nrow(mmx):1), y=c(mmx$mm-(mmx$ss), rev(mmx$mm+(mmx$ss))),col = "grey", border = "grey")
lines(1:nrow(mmx), mmx$mm, type="l")
axis(1, at=seq(0,400,80), labels=seq(400,2400,400))
# abline(v=(680-400)/10) ## for highlighting specific wvls
dev.off()

names(mmx) <- c("wvl", "mean_abs_loading", "sd_abs_loading")
write.csv(mmx, "./R_output/PLSDA_abs_loadings_age_4comps.csv", row.names = F)


################################
##### TREATMENT ###############

### Prepare data
dati$Treatment <- factor(dati$Treatment, 
                         levels = c("Control", "N", "P","NP"))

classi <- as.factor(dati$Treatment) ### define classes (age classes, treatments)

table(classi)

rndid <- list() ### list of random ID's for partitioning data into cal and val
set.seed(1840)
for (i in 1:100){ 
  rndid[[i]] <- with(dati, ave(1:nrow(dati), Treatment, FUN=function(x) {sample.int(length(x))}))
}

nsims <- 100 ### number of iterations, try 50, or 100?
compi <- 20 ### max number of components, something to play with, too many and the model crashes
mods <- list()
for (nsim in seq(nsims)){ ### not sure how to avoid the row names warning, shouldn't matter though
  print(nsim)
  # inTrain <- rndid[[nsim]]<= 25 ### Alternative to % partitioning,select nnumber of samples per class for training, e.g. 25 samples 
  flush.console()
  set.seed(nsim)
  inTrain <- createDataPartition(y = classi, p = .75, list = FALSE) ## 75% for training
  traini <- spec[inTrain,] 
  testi <- spec[-(inTrain),]
  trainclass <- classi[inTrain]
  testclass <- classi[-(inTrain)]
  plsFit <- train(traini, trainclass, method = "simpls", tuneLength = compi,
                  probMethod="Bayes", trControl = trainControl(method="LOOCV")) ### use leaf-one-out CV for small sample sizes
  mods[[nsim]] <- plsFit
}

mods[[1]]

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
boxplot(kapp$Kappa~kapp$ncomps,ylim=c(-0.2, 0.3),
        xlab="Number of components",ylab="Kappa")
text(x=1:20, y=rep(0.2,20),letters)
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
  inTrain <- createDataPartition(y =classi, p = .75, list = FALSE)
  training <- spec[inTrain,]
  testing <- spec[!(inTrain),]
  trainclass <- as.factor(classi[inTrain]) 
  testclass <- as.factor(classi[!(inTrain)])
  finalModel <- plsda(training,trainclass, ncomp=compi, probMethod = "Bayes", 
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
  inTrain <- rndid[[nsim]]<= nsims
  # inTrain <- createDataPartition(y = classi, p = .80, list = FALSE)
  testing <- spec[-(inTrain),]
  testclass <- as.factor(classi[-(inTrain)])
  
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

### for more and classwise accuracies see
# confus[[i]]

### Probability plot
arr <- array(unlist(probis), dim = c(dim(probis[[1]]),nsims))
prob_mean <- apply(arr, 1:2, mean)
prob_mean <- as.data.frame(prob_mean)
prob_mean$V1 <- probis[[1]]$testclass
colnames(prob_mean) <- colnames(probis[[1]])
write.csv(prob_mean,"./R_output/PLSDA_probmean_treat_10comps.csv")

pp <- melt(prob_mean, id="testclass")
pp$position <- ifelse (pp$testclass == pp$variable, 2,1) 
pp$testclass <- factor(pp$testclass, levels=rev(levels(pp$testclass)))

coli <- c("grey60","blue3","darkviolet","red2") ### 
# coli <- c("orange","green3", "darkgreen")

pdf("./R_output/probability_plot_treat_10comps.pdf",width = 6,height = 4)
ggplot(pp, aes(x=testclass, y=value, fill=variable, group=position))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values= alpha(coli,1))+
  theme(legend.title=element_blank())+
  labs(x = "Species", y= "")+
  coord_flip()
dev.off()

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

### add NAs for masked wvls
mm$ww <- as.numeric(substr(row.names(mm), 2, nchar(row.names(mm))))
row.names(mm)<- NULL

xx <- data.frame(ww=seq(400,2400,by=5)[-c(1,400,401)])
mmx <- merge(xx,mm, all.x = T)
mmx[is.na(mmx)] <- 0  ### needs to be a value otherwise plot doesn't work
### try finding a smarter solution, otherwise line can be pasted over manually

pdf("./R_output/PLSDA_loadi_treat_10comps.pdf",width = 6,height = 4)
plot(1:nrow(mmx), mmx$mm, type="n", bty="l", ylab="abs (loadings)", 
     xaxt="n", xlab="Wavelength (nm)")
polygon(x=c(1:nrow(mmx),nrow(mmx):1), y=c(mmx$mm-(mmx$ss), rev(mmx$mm+(mmx$ss))),col = "grey", border = "grey")
lines(1:nrow(mmx), mmx$mm, type="l")
axis(1, at=seq(0,400,80), labels=seq(400,2400,400))
# abline(v=(680-400)/10) ## for highlighting specific wvls
dev.off()

names(mmx) <- c("wvl", "mean_abs_loading", "sd_abs_loading")
write.csv(mmx, "./R_output/PLSDA_abs_loadings_treat_10comps.csv", row.names = F)

######## END ##############

