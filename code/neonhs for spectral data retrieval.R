


# https://cu-esiil.github.io/data-library/remote_sensing/neon_hyperspectral/neon_hyperspectral/
library(devtools)  
# install.packages('devtools')
devtools::install_github('earthlab/neonhs')


library(neonhs)
library(raster)
library(viridis)
library(sp)
library(tidyverse)

path_to_file <- system.file('extdata', 'ex.h5', package = 'neonhs')
r <- hs_read(path_to_file, bands = c(1, 50, 100, 400))
r

plot(r, col = cividis(100), axes = FALSE, box = FALSE)
