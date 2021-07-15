library(tidyverse)
library(readxl)
library(raster)
library(rgdal)
library(maps) 
library(dplyr) 
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(tidycensus)

## pulling in data for land parcel in Floyd 

shape=readOGR(dsn="/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/parcels_with_class", layer="Parcels_with_Class")

aoi_boundary_HARV <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/parcels_with_class/Parcels_with_Class.shp")

#View geometry type of our shape file
st_geometry_type(aoi_boundary_HARV)

#Check the CRS the data file is in
st_crs(aoi_boundary_HARV)

#check extent of AOI
st_bbox(aoi_boundary_HARV)


#Inspect all the values of categorical variable StateClass
aoi_boundary_HARV$PropClass
levels(aoi_boundary_HARV$PropClass)

state_class_colors <- c("blue", "green", "red", "cyan", "magenta", "yellow")
class_levels <- c("Agricultural/Undeveloped (20 – 99 acres)", "Agricultural/Undeveloped (100 acres and up)","Single-Family Residential(Suburban 0-19.99 acres)" ,
                   "Single Family Residential(Urban)", "Commercial/Industrial", "Multi-Family")
class_pal <- colorFactor(pal = state_class_colors, 
                          levels = class_levels)


# ggplot() +
#   geom_sf(data = aoi_boundary_HARV, aes(fill=PropClass)) + 
#   scale_color_manual(values = state_class_colors) +
#   labs(color = 'Property Class') + 
#   ggtitle("Land Usage Classification", subtitle = "All Parcel Type") + 
#   coord_sf()


#Filter out parcels based on property type and map seperately for agricultural/residential/industrial land usage

agr <- aoi_boundary_HARV %>% 
  filter(PropClass == "Agricultural/Undeveloped (20 – 99 acres)")%>%
  st_transform(crs = "+init=epsg:4326")
#nrow(lu_interest_HARV)

# ggplot() + 
#   geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
#   scale_color_manual(values = state_class_colors) +
#   ggtitle("Land Usage Classification", subtitle = "Agricultural/Undeveloped (20 – 99 acres)") + 
#   coord_sf()

agr_large <- aoi_boundary_HARV %>% 
  filter(PropClass == "Agricultural/Undeveloped (100 acres and up)")%>%
  st_transform(crs = "+init=epsg:4326")
  
#nrow(lu_interest_HARV)

# ggplot() + 
#   geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
#   scale_color_manual(values = state_class_colors) +
#   ggtitle("Land Usage Classification", subtitle = "Agricultural/Undeveloped (100 acres and up)") + 
#   coord_sf()

single <- aoi_boundary_HARV %>% 
  filter(PropClass == "Single-Family Residential(Suburban 0-19.99 acres)")%>%
  st_transform(crs = "+init=epsg:4326")
nrow(lu_interest_HARV)

#ggplot() + 
  # geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
  # scale_color_manual(values = state_class_colors) +
  # ggtitle("Land Usage Classification", subtitle = "Single-Family Residential(Suburban 0-19.99 acres)") + 
  # coord_sf()

single_urban <- aoi_boundary_HARV %>% 
  filter(PropClass == "Single Family Residential(Urban)")%>%
  st_transform(crs = "+init=epsg:4326")
nrow(lu_interest_HARV)

mult <- aoi_boundary_HARV %>% 
  filter(PropClass == "Multi-Family")%>%
  st_transform(crs = "+init=epsg:4326")
nrow(mult)

# ggplot() + 
#   geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
#   scale_color_manual(values = state_class_colors) +
#   ggtitle("Land Usage Classification", subtitle = "Multi-Family") + 
#   coord_sf()
com <- aoi_boundary_HARV %>% 
  filter(PropClass == "Commercial/Industrial")%>%
  st_transform(crs = "+init=epsg:4326")
nrow(lu_interest_HARV)

total_block <-  get_acs(geography = "block group",
                        variables = "B01003_001",
                        state = "VA",
                        county = "Floyd County",
                        geometry = TRUE)



total_block %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(data = com, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~class_pal("Commercial/Industrial"),
              group = com$PropClass) %>%
  addPolygons(data = agr, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~class_pal("Agricultural/Undeveloped (20 – 99 acres)"),
              group = agr$PropClass) %>%
  addPolygons(data = agr_large, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~class_pal("Agricultural/Undeveloped (100 acres and up)"),
              group = agr_large$PropClass) %>%
  addPolygons(data = single, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~class_pal("Single-Family Residential(Suburban 0-19.99 acres)"),
              group = single$PropClass) %>%
  addPolygons(data = single_urban, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~class_pal("Single Family Residential(Urban)"),
              group = single_urban$PropClass) %>%
  addPolygons(data = mult, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~class_pal("Multi-Family"),
              group = mult$PropClass) %>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Agricultural/Undeveloped (20 – 99 acres)",
                      "Agricultural/Undeveloped (100 acres and up)",
                      "Single-Family Residential(Suburban 0-19.99 acres)",
                      "Single Family Residential(Urban)",
                      "Multi-Family",
                      "Commercial/Industrial"), 
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend(title = "Land Parcel", position = "topleft", pal = class_pal, values = class_levels)

