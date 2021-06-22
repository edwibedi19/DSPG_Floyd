library(tiff)
library(raster)

file <- readTIFF("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/BGM_map_2010_Floyd.tif", 
         native = FALSE, all = FALSE, convert = TRUE,
         info = FALSE, indexed = FALSE, as.is = FALSE,
         payload = TRUE)


r <- raster(file)
R <- rasterToPoints(r)
