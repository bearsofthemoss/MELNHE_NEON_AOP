# 15 oct

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
#                             Partitionnement de la variation                           #
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# NOTES : 


#-=-=-=-=-=-=-=-=-=-=-=-=-
# Libraries ----
#-=-=-=-=-=-=-=-=-=-=-=-=-

# # Install remotes if needed
# if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
# library(remotes)
# 
# # Feb 21 2022
# 
# # find archived version of raster
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/raster/raster_3.4-10.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# # rgdal
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.5-23.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# # recipes
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/recipes/recipes_0.1.16.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# # colorspace
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/colorspace/colorspace_2.0-1.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# 
# # ggplot
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.3.3.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# # plyr
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/plyr/plyr_1.8.6.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# #reshape2
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/reshape2/reshape2_1.4.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# #ModelMetrics
# install.packages("ModelMetrics")
# 
# # pROC
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/pROC/pROC_1.18.0.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# 
# # caret is persnickety
# # caret
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/caret/caret_6.0-86.tar.gz"  # Adjust version as needed
# install.packages(packageurl, repos=NULL, type="source")
# 
# 
# # hsdar at the end
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/hsdar/hsdar_1.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# 
# 
# # vegan at the end
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/vegan/vegan_2.5-7.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")


####################


library(dplyr)

library(ggplot2)
library(tidyr)
library(spectrolab)
library(tibble)
library(vegan)
library(hsdar)


dada<- read.csv(here::here("data_folder","melnhe_input_files","actual_tops_10_26_greater_0.1.csv"))
dim(dada)
in_stands <- c("C1","C2","C4","C6","C8","C9")
dada <- dada[dada$Stand %in% in_stands,]
dim(dada)

dada <- na.omit(dada)

cwm <- read.csv(here::here("data_folder","melnhe_input_files","CWM_2021-22.csv"))
dim(cwm)
cwm <- cwm[cwm$Stand %in% in_stands,]
cwm$plot_id <- paste(cwm$Stand, cwm$Treatment)

cwm <- cwm[ , c("plot_id","Ncwm","Pcwm","Cacwm","Kcwm","Mgcwm","Mncwm","Scwm")]

names(cwm)


dada$pixel_id <- dada$X
dada$plot_id <- paste(dada$Stand, dada$Treatment)

dim(dada)
dim(cwm)
length(unique(dada$plot_id))


# make a df of just the spectral data
start_col <- which(names(dada) == "Band_403.57")
end_col <- which(names(dada) == "Band_2396.71")
spectral_cols <- start_col:end_col

# Create data frame with just spectral data
wvl <- as.data.frame(dada[, spectral_cols])
rownames(wvl) <- paste(dada$plot_id, dada$pixel_id, sep = '_')

# Replace NA values with zeros
wvl_filled <- wvl
wvl_filled[is.na(wvl_filled)] <- 0  # Replace NAs with zeros

# Create wavelength values
wavelengths <- seq(405, 2395, length=345)  # Adjust as needed for your wavelength range

# Create spectral library using hsdar
sp_lib <- hsdar::speclib(spectra = as.matrix(wvl_filled), wavelength = wavelengths)

# Calculate spectral angle mapper distance
sp_dist <- as.dist(hsdar::sam_distance(sp_lib))

# Step 3: Prepare the foliar chemistry explanatory variables
pixel_plot_data <- data.frame(
  pixel_id = dada$pixel_id,
  plot_id = dada$plot_id
)

# Perform a manual join operation (equivalent to left_join)
pixel_chem <- merge(pixel_plot_data, cwm, by = "plot_id", all.x = TRUE)

pixel_chem$Stand <- dada$Stand[match(pixel_chem$plot_id, dada$plot_id)]

# Join the chemistry data to ensure matching order
chem_data_ordered <- merge(order_match, pixel_chem, by = c("pixel_id", "plot_id"))
chem_data_ordered <- chem_data_ordered[, 3:9, drop = FALSE]

# Standardize if needed
chem_data <- vegan::decostand(chem_data_ordered, method = "standardize", na.rm = TRUE)

# add in stand
chem_data$Stand <- pixel_chem$Stand

# Check the matching dimensions
dim(chem_data_ordered)
dim(wvl_filled)

###################

# Run the RDA
spec_rda <- rda(wvl ~ Ncwm + Pcwm + Cacwm + Kcwm + Mgcwm + Mncwm + Scwm + Condition(Stand), data = chem_data)

# Step 6: Examine the RDA results
# Get a summary of the RDA
rda_summary <- summary(spec_rda)
print(rda_summary)


# Step 7: Test statistical significance
# Test overall model significance
anova_model <- anova(spec_rda)
print("Overall model significance:")
print(anova_model)

# Test significance of individual axes
anova_axes <- anova(spec_rda, by = "axis")
print("Significance of individual axes:")
print(anova_axes)


# Test significance of individual chemistry variables
anova_terms <- anova(spec_rda, by = "terms")
print("Significance of individual chemistry variables:")
print(anova_terms)

# Step 8: Visualize the RDA results
# Basic RDA plot
par(mfrow = c(1, 1))
plot(spec_rda, scaling = 2)  # scaling=2 emphasizes relationships among variables

# More detailed plot with better labeling
plot(spec_rda, scaling = 2, type = "none", main = "RDA of Spectral Data ~ Foliar Chemistry")
points(spec_rda, display = "sites", pch = 21, col = "black", bg = "lightblue", cex = 1.2)
text(spec_rda, display = "species", col = "red", cex = 0.8)
arrows(0, 0, 
       scores(spec_rda, choices = 1, display = "bp")[,1] * 0.9, 
       scores(spec_rda, choices = 2, display = "bp")[,1] * 0.9, 
       col = "blue", lwd = 2)
text(scores(spec_rda, choices = 1, display = "bp")[,1] * 0.95, 
     scores(spec_rda, choices = 2, display = "bp")[,1] * 0.95, 
     labels = rownames(scores(spec_rda, display = "bp")), 
     col = "blue", cex = 1.2)

##################



#-=-=-=-=-=-=-=-=-=-=-=-=-
# Variance Partitioning -- SPECTRAL DATA WITH FOLIAR CHEMISTRY
#-=-=-=-=-=-=-=-=-=-=-=-=-


#-=-=-=-=-=-=-=-=-=-=-=-=-
# Individual Element Contributions to Spectral Variance
#-=-=-=-=-=-=-=-=-=-=-=-=-

# List of all foliar chemistry elements to analyze
elements <- c("Ncwm", "Pcwm", "Cacwm", "Kcwm", "Mgcwm", "Mncwm", "Scwm","Stand")

# Create a data frame to store the results
element_results <- data.frame(
  Element = elements,
  R2 = numeric(length(elements)),
  Adj_R2 = numeric(length(elements)),
  F_value = numeric(length(elements)),
  p_value = numeric(length(elements)),
  stringsAsFactors = FALSE
)

# Run analysis for each element individually
for (i in 1:length(elements)) {
  # Create formula with just this element
  formula_str <- paste("sp_dist ~", elements[i])
  
  # Run dbRDA model
  element_model <- dbrda(formula(formula_str), data = chem_data)
  
  # Extract adjusted R² 
  adj_r2 <- RsquareAdj(element_model)$adj.r.squared
  
  # Run permutation test for significance
  element_anova <- anova(element_model)
  
  # Store results
  element_results$R2[i] <- element_model$CCA$tot.chi / element_model$tot.chi
  element_results$Adj_R2[i] <- adj_r2
  element_results$F_value[i] <- element_anova$F[1]
  element_results$p_value[i] <- element_anova$Pr[1]
}

# Sort results by adjusted R² to see most important elements
element_results_sorted <- element_results[order(element_results$Adj_R2, decreasing = TRUE),]
print(element_results_sorted)

# Create a bar plot of adjusted R² values
barplot(element_results_sorted$Adj_R2, 
        names.arg = element_results_sorted$Element,
        main = "Contribution of Each Element to Spectral Variance",
        ylab = "Adjusted R²",
        col = "steelblue",
        las = 2) # Rotates the x-axis labels
abline(h = 0, lty = 2) # Add a reference line at 0

# Find the element with highest contribution
best_element <- element_results_sorted$Element[1]
cat("Element explaining most variance:", best_element, 
    "(Adj. R² =", round(max(element_results_sorted$Adj_R2), 3), ")\n")

#-=-=-=-=-=-=-=-=-=-=-=-=-
# Forward Selection of Elements
#-=-=-=-=-=-=-=-=-=-=-=-=-
# 
# # This shows which combination of elements is most parsimonious
# # Start with null model
# null_model <- dbrda(sp_dist ~ 1, data = chem_data)
# 
# # Full model with all elements
# all_elements_formula <- paste("sp_dist ~", paste(elements, collapse = " + "))
# full_model <- dbrda(formula(all_elements_formula), data = chem_data)
# 
# # Perform forward selection
# fwd_model <- ordistep(null_model, scope = formula(full_model), direction = "forward")
# 
# # Get the final adjusted R²
# final_adj_r2 <- RsquareAdj(fwd_model)$adj.r.squared
# cat("Optimal model explains", round(final_adj_r2, 3), "of variance\n")
# 
# # Show the selected variables
# cat("Selected elements (in order):", paste(attr(terms(fwd_model), "term.labels"), collapse = ", "), "\n")
# 
# #-=-=-=-=-=-=-=-=-=-=-=-=-
# # Variance Partitioning Between Best Elements
# #-=-=-=-=-=-=-=-=-=-=-=-=-
# 
# # If forward selection identified 2+ elements, we can partition variance between them
# if (length(attr(terms(fwd_model), "term.labels")) >= 2) {
#   top_elements <- attr(terms(fwd_model), "term.labels")[1:min(2, length(attr(terms(fwd_model), "term.labels")))]
# 
#   # Create separate matrices for top elements
#   X1 <- chem_data[, top_elements[1], drop=FALSE]
#   X2 <- chem_data[, top_elements[2], drop=FALSE]
# 
#   # Perform variance partitioning between top 2 elements
#   var_part_top <- varpart(sp_dist, X1, X2)
# 
#   # Display results
#   var_part_top
# 
#   # Plot the Venn diagram
#   plot(var_part_top, bg = 1:4, digits = 2, cutoff = 0.0001,
#        Xnames = top_elements)
# }