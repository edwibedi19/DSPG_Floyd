library(tiff)
library(raster)

file <- readTIFF("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/Domestic_Well_BGM_map_2010 (1)/BGM_map_2010.tif")
floyd_tif <- readTIFF("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/Domestic_Well_BGM_map_2010 (1)/BGM_map_2010_Floyd.tif")


r <- raster(file)
rF <- raster(floyd_tif)
R <- rasterToPoints(r)

## plotting either is not helpful 
plot(rF)
plot(r)
