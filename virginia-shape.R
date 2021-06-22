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


## simple point plots 
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
library(sp)
library(sf)
library(spatialEco)
library(FRK)


## getting county data for just floyd 
floyd <- left_join(countydata, counties, by = "county_fips") %>% 
  filter(state_name %in% c("Virginia"), county_name %in% c("Floyd County")) 

pts <- SpatialPointsDataFrame(flo_w, coords = flo_w[,1:2]) 

floyd_df1 <- as.data.frame(floyd) 

points_poly <- df_to_SpatialPolygons(floyd_df1, key = "group", coords = c("long","lat"), proj = CRS()) 

# didnt work 
#new_shape <- over( pts , points_poly , fn = NULL) 
#new_shape <- point.in.poly(pts, points_poly) 

# this is filtering the points 
new_shape <- pts[points_poly,]
new_shape_df <- as.data.frame(new_shape)

## only points within the polygon 
floyd %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "white", size = 0.05, fill = "light blue") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.position = "right",
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 11),
        legend.key.height = unit(.25, "in")) +
  geom_point(data=new_shape_df, aes(long, lat), inherit.aes = FALSE, alpha = 0.5, size = .5) + 
  geom_point(data=nhdp_df_f, aes(x=coords.x1, y=coords.x2), inherit.aes = FALSE, alpha = 0.5, size = 1, colour = "red") + 
  ggtitle("Floyd County Water bodies, Lakes, Ponds, Springs")


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
## point map of towns in Virginia  
nhdp_df_f <- filter(nhdp_df, nhdp_df$coords.x1>-80.8, nhdp_df$coords.x2<37.1, !is.na(nhdp_df$GNIS_Name) )

nhdp_df_f <- filter(nhdp_df_f, nhdp_df_f$coords.x1 < -79, nhdp_df_f$coords.x2 > 36.7)
