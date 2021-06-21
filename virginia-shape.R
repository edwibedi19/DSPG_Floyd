setwd("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_FLoyd/data")
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)

#devtools::install_github("UrbanInstitute/urbnmapr")


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

## Water bodies, lakes 
water_df <- fortify(water)
flo_w <- water_df %>% 
  filter(water_df$long > -80.6 & water_df$long < -80.1
         & water_df$lat < 37.2 & water_df$lat > 36.7) 

## Forests, lakes, wetlands 
natural_df <- fortify(natural)
flo_n <- natural_df %>% 
  filter(natural_df$long > -80.6 & natural_df$long < -80.1 & 
           natural_df$lat < 37.2 & natural_df$lat > 36.7) 

## nothing beecause Floyd is not on the coast 
# coastline_df <- fortify(coastline)
# flo_c <- coastline_df %>% 
#   filter(coastline_df$long >-80.7 & coastline_df$long < -80.1 & coastline_df$lat < 37 & coastline_df$lat > 36.8)



ggplot(flo_n, aes(x = long, y = lat)) +
  geom_point() + 
  ggtitle("Floyd County Forests, Lakes, Wetlands ")
  

ggplot(flo_w, aes(x = long, y = lat)) +
  geom_point() + 
  ggtitle("Floyd County Water bodies, Lakes, Ponds")


## creating maps 
library(tidyverse)
library(urbnmapr)
library(rworldmap)
## getting county data for just floyd 
floyd <- left_join(countydata, counties, by = "county_fips") %>% 
  filter(state_name %in% c("Virginia"), county_name %in% c("Floyd County")) 


floyd %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "white", size = 0.05, fill = "light blue") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.position = "right",
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 11),
        legend.key.height = unit(.25, "in")) +
  geom_point(data=flo_w, aes(long, lat), inherit.aes = FALSE, alpha = 0.5, size = 0.5) + 
  geom_label()
  ggtitle("Floyd County Water bodies, Lakes, Ponds") 



map <- floyd %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon( size = 0.05, fill = "light blue") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.position = "right",
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 11),
        legend.key.height = unit(.25, "in")) +  ggtitle("Floyd County") 

