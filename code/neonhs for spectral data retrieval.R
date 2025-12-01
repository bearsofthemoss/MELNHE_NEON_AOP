


# https://cu-esiil.github.io/data-library/remote_sensing/neon_hyperspectral/neon_hyperspectral/
library(devtools)  
# install.packages('devtools')
devtools::install_github('earthlab/neonhs')


library(neonhs)
#library(raster)
library(viridis)
#library(sp)
library(tidyverse)

ff <- list.files("data_folder/DP3.30006.001/neon-aop-products/2019/",pattern = ".h5", recursive = T, full.names = T)

f <- ff[1]

# path_to_file <- system.file('extdata', 'ex.h5', package = 'neonhs')
r <- hs_read( f , bands = c(1:426))
r

plot(r, col = cividis(100), axes = FALSE, box = FALSE)

pts <- SpatialPointsDataFrame(coords = data.frame(x = c(257025, 257011),
                                                  y = c(4111982, 4111991)), 
                              data = data.frame(id = 1:2),
                              proj4string = CRS(hs_proj4string(path_to_file)))
plot(r[[1]], axes = FALSE, box = FALSE)
plot(pts, add = TRUE)




