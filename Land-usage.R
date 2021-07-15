library(tidyverse)
library(readxl)
library(raster)
library(rgdal)
library(maps) 
library(dplyr) 
library(ggplot2)
library(sf)

## pulling in data for land parcel in Floyd 

shape=readOGR(dsn="C:/Users/E DWIBEDI/Desktop/Interships/DGSP/DSPG_Floyd/data/parcels_with_class", layer="Parcels_with_Class")

aoi_boundary_HARV <- st_read(
  "C:/Users/E DWIBEDI/Desktop/Interships/DGSP/DSPG_Floyd/data/parcels_with_class/Parcels_with_Class.shp")

#View geometry type of our shape file
st_geometry_type(aoi_boundary_HARV)

#Check the CRS the data file is in
st_crs(aoi_boundary_HARV)

#check extent of AOI
st_bbox(aoi_boundary_HARV)


#Inspect all the values of categorical variable StateClass
aoi_boundary_HARV$PropClass
levels(aoi_boundary_HARV$PropClass)

state_class_colors <- c("blue", "green", "navy", "purple","yellow", "red", "pink", "orange","brown","cyan1","cyan", "magenta", "springgreen")

ggplot() +
  geom_sf(data = aoi_boundary_HARV, aes(fill=PropClass)) + 
  scale_color_manual(values = state_class_colors) +
  labs(color = 'Property Class') +
  ggtitle("Land Usage Classification", subtitle = "All Parcel Type") + 
  coord_sf()

#Filter out parcels based on property type and map seperately for agricultural/residential/industrial land usage

lu_interest_HARV <- aoi_boundary_HARV %>% 
  filter(PropClass == "Agricultural/Undeveloped (20 – 99 acres)")
nrow(lu_interest_HARV)

ggplot() + 
  geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
  scale_color_manual(values = state_class_colors) +
  ggtitle("Land Usage Classification", subtitle = "Agricultural/Undeveloped (20 – 99 acres)") + 
  coord_sf()

lu_interest_HARV <- aoi_boundary_HARV %>% 
  filter(PropClass == "Agricultural/Undeveloped (100 acres and up)")
nrow(lu_interest_HARV)

ggplot() + 
  geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
  scale_color_manual(values = state_class_colors) +
  ggtitle("Land Usage Classification", subtitle = "Agricultural/Undeveloped (100 acres and up)") + 
  coord_sf()

lu_interest_HARV <- aoi_boundary_HARV %>% 
  filter(PropClass == "Single-Family Residential(Suburban 0-19.99 acres)")
nrow(lu_interest_HARV)

ggplot() + 
  geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
  scale_color_manual(values = state_class_colors) +
  ggtitle("Land Usage Classification", subtitle = "Single-Family Residential(Suburban 0-19.99 acres)") + 
  coord_sf()

lu_interest_HARV <- aoi_boundary_HARV %>% 
  filter(PropClass == "Single-Family Residential(Urban)")
nrow(lu_interest_HARV)

lu_interest_HARV <- aoi_boundary_HARV %>% 
  filter(PropClass == "Multi-Family")
nrow(lu_interest_HARV)

ggplot() + 
  geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
  scale_color_manual(values = state_class_colors) +
  ggtitle("Land Usage Classification", subtitle = "Multi-Family") + 
  coord_sf()

lu_interest_HARV <- aoi_boundary_HARV %>% 
  filter(PropClass == "Commercial/Industrial")
nrow(lu_interest_HARV)

ggplot() + 
  geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
  scale_color_manual(values = state_class_colors) +
  ggtitle("Land Usage Classification", subtitle = "Commercial/Industrial") + 
  coord_sf()