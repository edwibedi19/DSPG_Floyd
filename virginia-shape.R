setwd("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_FLoyd/data")
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate) 
library(tidyverse)
library(urbnmapr)
library(rworldmap)
library(sp)
library(sf)
library(spatialEco)
library(FRK)


water <- readOGR( 
  dsn= "virginia_water/" , 
  layer="virginia_water"
)

natural <- readOGR( 
  dsn= "virginia_natural/" , 
  layer="virginia_natural"
)

## Water bodies, lakes 
water_df <- fortify(water)
flo_w <- water_df %>% 
  filter(water_df$long > -80.6 & water_df$long < -80.1
         & water_df$lat < 37.2 & water_df$lat > 36.7) 
flo_w$group <- 1

## Forests, lakes, wetlands 
natural_df <- fortify(natural)
flo_n <- natural_df %>% 
  filter(natural_df$long > -80.6 & natural_df$long < -80.1 & 
           natural_df$lat < 37.2 & natural_df$lat > 36.7) 
flo_n$group <- 2 


## simple point plots 
ggplot(flo_n, aes(x = long, y = lat)) +
  geom_point() + 
  ggtitle("Floyd County Forests, Lakes, Wetlands ")


ggplot(flo_w, aes(x = long, y = lat)) +
  geom_point() + 
  ggtitle("Floyd County Water bodies, Lakes, Ponds")


## getting county data for just floyd 
floyd <- left_join(countydata, counties, by = "county_fips") %>% 
  filter(state_name %in% c("Virginia"), county_name %in% c("Floyd County")) 

# turning into SpatialPointsDF 
pts_w <- SpatialPointsDataFrame(flo_w, coords = flo_w[,1:2]) 
pts_n <- SpatialPointsDataFrame(flo_n, coords = flo_n[,1:2]) 

floyd_df <- as.data.frame(floyd) 

## turning floyd_df into spatialPolygon 
points_poly <- df_to_SpatialPolygons(floyd_df, key = "group", coords = c("long","lat"), proj = CRS()) 


# this is filtering the points 
new_shape_w <- pts_w[points_poly,]
new_shape_df_w <- as.data.frame(new_shape_w)

# this is filtering the points 
new_shape_n <- pts_n[points_poly,]
new_shape_df_n <- as.data.frame(new_shape_n)

map <- ggplot(floyd, aes(long, lat)) +
  geom_polygon(color = "white", size = 0.05, fill = "grey") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.position = "right",
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 7),
        legend.key.height = unit(.25, "in")) +
  scale_colour_manual(labels = c("Water bodies", "Forests"), values=c("dark blue","green")) + 
  geom_point(data=new_df, aes(long, lat, color = as.factor(group)), inherit.aes = FALSE, alpha = 0.1, size = 3)  +  
  labs(title = "Floyd County Features",
       color = "Features",
       x = "Longitude",
       y = "Latitude")

map


## just the county outline 
floyd %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon( size = 0.05, fill = "light blue") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.position = "right",
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 11),
        legend.key.height = unit(.25, "in")) +  ggtitle("Floyd County") 


nhdp <- readOGR( 
  dsn= "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/Shape" , 
  layer="NHDPoint"
)

nhdp_df <- as(nhdp, "data.frame")
pts_p <- SpatialPointsDataFrame(nhdp_df, coords = nhdp_df[,10:11]) 

#Springs only 1 in Floyd 
new_shape_p <- pts_p[points_poly,]
new_shape_df_p <- as.data.frame(new_shape_p)

new_shape_df_p$group <- 3 

new_df <- rbind(new_shape_df_n, new_shape_df_w) 


