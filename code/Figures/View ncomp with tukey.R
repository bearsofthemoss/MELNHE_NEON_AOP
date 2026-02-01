

## supplemental figure S2, selection of number of components

m_vip <- read.csv(here::here("R_output","PLSDA_output","Mid-aged forest","tukey_component_letters.csv"))


par(bty = "l")
boxplot(kapp$Kappa ~ kapp$ncomps, 
        ylim = c(0, max(kapp$Kappa) + 0.1),
        xlab = "Number of components", 
        ylab = "Kappa")

text(x = 1:compi, y = rep(max(kapp$Kappa) + 0.05, compi), letters)


