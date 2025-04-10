# 15 oct

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
#                             Partitionnement de la variation                           #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# NOTES : 


#-=-=-=-=-=-=-=-=-=-=-=-=-
# Libraries ----
#-=-=-=-=-=-=-=-=-=-=-=-=-

library(dplyr)
library(ggplot2)
library(tidyr)
library(spectrolab)
library(tibble)
library(vegan)

#-=-=-=-=-=-=-=-=-=-=-=-=-
# Données ----
#-=-=-=-=-=-=-=-=-=-=-=-=-

# setwd("~/Documents/Maîtrise/DonnéesAnalyses/spectres")

# load("~/Documents/Maîtrise/DonnéesAnalyses/spectres/rt_sg_large.RData") # rt_sg_large
# refl <- rt_sg_large %>% 
#   dplyr::filter(propriete == "reflectance")
# obtenu via manips sur script "nettoyage_spectres_lissage_correction.R" dans le document "scripts-NETTOYAGE"

dada<- read.csv(here::here("data_folder","melnhe_input_files","actual_tops_10_26_greater_0.1.csv"))
dim(dada)
head(dada)


cwm <- read.csv(here::here("data_folder","melnhe_input_files","CWM_2021-22.csv"))
dim(cwm)




#-=-=-=-=-=-=-=-=-=-=-=-=-
# Manipulations préalables  ----
#-=-=-=-=-=-=-=-=-=-=-=-=-

## distance spectrale
wvl <- refl_norm_large %>%
  within(x <- paste(sample_id, scientific_name, site_class, sep = '-')) %>%
  dplyr::select(sample_id = "x", "400":"2400") %>%
  column_to_rownames(var = "sample_id")

sp.angl_prov <- hsdar::speclib(spectra = as.matrix(wvl), wavelength = as.numeric(colnames(wvl)) )
id <- hsdar::idSpeclib(sp.angl_prov)
sp.angl <- as.dist(hsdar::sam_distance(sp.angl_prov)) # objet utilisé dans la PCoA
# save(sp.angl, file = "sp.angl.RData")

## définir les variables explicatives en variables binaires, enlever col.redondantes
sc.prov <- as.vector.factor(refl_norm_large$scientific_name)
sc.prov.2 <- as.matrix(model.matrix(~ sc.prov))
scientific_name <- sc.prov.2[, 2:4]

st.clss.prov <- as.vector.factor(refl_norm_large$site_class)
st.clss.prov.2 <- as.matrix(model.matrix(~ st.clss.prov))
site_class <- st.clss.prov.2[, 2:4]

# distance entre les samples -> TF
## centrer-réduire + distance de Hellinger
ft <- vegan::decostand(ft_all[, 9:26], method = "standardize", na.rm = T) %>% 
  vegan::vegdist("euclidean", diag = T, upper = T) 

## définir les variables explicatives en variables binaires, enlever col.redondantes
sc.prov.ft <- as.vector(ft_all$scientific_name)
sc.prov.ft.2 <- as.matrix(model.matrix(~ sc.prov.ft))
sc.ft <- sc.prov.ft.2[, 2:4]

site.ft.prov <- as.vector(ft_all$site_class)
site.ft.prov.2 <- as.matrix(model.matrix(~ site.ft.prov))
site.ft <- site.ft.prov.2[, 2:4]

#-=-=-=-=-=-=-=-=-=-=-=-=-
# Partitionnement de la variance -- SPECTRES
#-=-=-=-=-=-=-=-=-=-=-=-=-

var.part.sp <- varpart(sp.angl, site_class, scientific_name)
# 
# Partition of squared  distance in dbRDA 
# 
# Call: varpart(Y = sp.angl, X = site_class, scientific_name)
# 
# Explanatory tables:
#   X1:  site_class
# X2:  scientific_name 
# 
# No. of explanatory tables: 2 
# Total variation (SS): 0.2998 
# No. of observations: 94 
# 
# Partition table:
#                       Df  R.squared   Adj.R.squared Testable
# [a+b] = X1            3   0.00961      -0.02340     TRUE
# [b+c] = X2            3   0.82542       0.81960     TRUE
# [a+b+c] = X1+X2       6   0.83588       0.82456     TRUE
# Individual fractions                                    
# [a] = X1|X2           3                 0.00496     TRUE
# [b]                   0                -0.02836    FALSE
# [c] = X2|X1           3                 0.84797     TRUE
# [d] = Residuals                         0.17544    FALSE
# ---
#   Use function ‘dbrda’ to test significance of fractions of interest
plot(var.part.sp, bg = 1:4, digits = 2, cutoff = 0.0001)

## vérifier résultats avec RDA (devrait donner la même chose)
rda.sp <- capscale(sp.angl ~ site_class + scientific_name) # Inertia is variance, proportion is r.squared (in vegan)
RsquareAdj(rda.sp) # un peu différent... pas d'intération dans ma formule ?
anova(rda.sp)

## Use function ‘dbrda’ to test significance of fractions of interest. Semi-Partial correlation
cpsc.site <- dbrda(sp.angl ~ site_class) # condition pas disponible dans capscale !
RsquareAdj(cpsc.site) #  pas significatif
anova(cpsc.site) # PERMUTATION ?

cpsc.sc <- dbrda(sp.angl ~ scientific_name)
RsquareAdj(cpsc.sc)
anova(cpsc.sc)


#-=-=-=-=-=-=-=-=-=-=-=-=-
# Partitionnement de la variance -- TF
#-=-=-=-=-=-=-=-=-=-=-=-=-
var.part.tf <- varpart(ft, site.ft, sc.ft)
# Partition of squared Euclidean distance in dbRDA 
# 
# Call: varpart(Y = ft, X = site.ft, sc.ft)
# 
# Explanatory tables:
#   X1:  site.ft
# X2:  sc.ft 
# 
# No. of explanatory tables: 2 
# Total variation (SS): 1660.1 
# No. of observations: 94 
# 
# Partition table:
#   Df R.squared Adj.R.squared Testable
# [a+b] = X1            3   0.01827      -0.01446     TRUE
# [b+c] = X2            3   0.69920       0.68918     TRUE
# [a+b+c] = X1+X2       6   0.72300       0.70390     TRUE
# Individual fractions                                    
# [a] = X1|X2           3                 0.01472     TRUE
# [b]                   0                -0.02918    FALSE
# [c] = X2|X1           3                 0.71835     TRUE
# [d] = Residuals                         0.29610    FALSE
# ---
#   Use function ‘dbrda’ to test significance of fractions of interest
plot(var.part.tf, bg = 1:4, digits = 2, cutoff = 0.0001)

## Use function ‘dbrda’ to test significance of fractions of interest. Semi-Partial correlation
cpsc.site.tf <- dbrda(ft ~ site.ft) 
RsquareAdj(cpsc.site.tf) 
anova(cpsc.site.tf) #  pas significatif

cpsc.sc.tf <- dbrda(ft ~ sc.ft)
RsquareAdj(cpsc.sc.tf)
anova(cpsc.sc.tf) # sign