setwd("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_FLoyd/data")
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)


water <- readOGR( 
  dsn= "virginia_water/" , 
  layer="virginia_water"
)

natural <- readOGR( 
  dsn= "virginia_natural/" , 
  layer="virginia_natural"
)
coastline <- readOGR( 
  dsn= "virginia_coastline/" , 
  layer="virginia_coastline"
)

#coastline <- st_read(
  #"/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_FLoyd/data/virginia_coastline/virginia_coastline.shp")

water_df <- fortify(water)
flo_w <- water_df %>% 
  filter(water_df$long > -81 & water_df$long < -79.9 & water_df$lat < 37 & water_df$lat > 36.4) 

natural_df <- fortify(natural)
flo_n <- natural_df %>% 
  filter(natural_df$long > -81 & natural_df$long < -79.9 & natural_df$lat < 37 & natural_df$lat > 36.4) 

coastline_df <- fortify(coastline)
flo_c <- coastline_df %>% 
  filter(coastline_df$long > -81 & coastline_df$long < -79.9 & coastline_df$lat < 37 & coastline_df$lat > 35.5) 



ggplot(water_df, aes(x = long, y = lat)) +
  geom_polygon() +
  geom_point() + 
  coord_equal()  





