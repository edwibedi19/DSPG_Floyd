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

#shape=readOGR(dsn="/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/parcels_with_class", layer="Parcels_with_Class")
aoi_boundary_HARV2020 <- st_read( "/Users/julierebstock/Downloads/parcels_with_class/parcels_with_class.shp")

aoi_boundary_HARV2017 <- st_read( "/Users/julierebstock/Downloads/parcels_2017/parcels_2017.shp")

aoi_boundary_HARV2013 <- st_read("/Users/julierebstock/Downloads/parcels_2013/parcels_2013.shp")


agr20 <- aoi_boundary_HARV2020 %>% 
  filter(PropClass == "Agricultural/Undeveloped (20 – 99 acres)")
agr17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Agricultural/Undeveloped (20 – 99 acres)")
agr13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Agricultural/Undeveloped (20 – 99 acres)")

years <- c(2020, 2017,2013)
agr <- c(2994, 2961, 0)
percent <- c(0, 1.11, 0) 
agrAll <-data.frame(years, agr, percent)
agrAll$Land <- "Agricultural/Undeveloped (20 – 99 acres)"
colnames(agrAll) <- c("Year", "Amouunt", "Percent Change", "Land")


agr_large20 <- aoi_boundary_HARV2020 %>% 
  filter(PropClass == "Agricultural/Undeveloped (100 acres and up)")
agr_large17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Agricultural/Undeveloped (100 acres and up)")
agr_large13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Agricultural/Undeveloped (100 acres and up)")

agr_large <- c(514, 536, 543)
percent <- c(0, -4.1, -1.29)
agr_largeAll <-data.frame(years, agr_large, percent)
agr_largeAll$Land <- "Agricultural/Undeveloped (100 acres and up)"
colnames(agr_largeAll) <- c("Year", "Amouunt", "Percent Change", "Land")

#single household 
single20 <- aoi_boundary_HARV2020 %>% 
  filter(PropClass == "Single-Family Residential(Suburban 0-19.99 acres)")
single17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Single-Family Residential(Suburban 0-19.99 acres)")
single13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Single-Family Residential(Suburban 0-19.99 acres)")

single <- c(10307, 10120, 9749)
percent <- c(0, 1.85, 3.81)
single_all <-data.frame(years, single, percent)

single_all$Land <- "Single-Family Residential(Suburban 0-19.99 acres)"
colnames(single_all) <- c("Year", "Amouunt", "Percent Change", "Land")
# single urban household 
single_urban20 <- aoi_boundary_HARV2020 %>% 
  filter(PropClass == "Single Family Residential(Urban)")
single_urban17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Single Family Residential(Urban)")
single_urban13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Single Family Residential(Urban)")

single_urban <- c(237, 243, 244)
percent <- c(0, -2.47, -.41)
single_urban_all <-data.frame(years, single_urban, percent)

single_urban_all$Land <- "Single-Family Residential(Urban)"
colnames(single_urban_all) <- c("Year", "Amouunt", "Percent Change", "Land")
# multi
mult20 <- aoi_boundary_HARV2020 %>% 
  filter(PropClass == "Multi-Family") 
mult17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Multi-Family") 
mult13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Multi-Family") 

mult <- c(2, 2, 2)
percent <- c(0, 0, 0)
multAll <-data.frame(years, mult, percent)

multAll$Land <- "Multi-Family"
colnames(multAll) <- c("Year", "Amouunt", "Percent Change", "Land")

# commerical
com20 <- aoi_boundary_HARV2020 %>% 
  filter(PropClass == "Commercial/Industrial")
com17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Commercial/Industrial")
com13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Commercial/Industrial")

com <- c(210, 208, 200)
percent <- c(0, .96, 4)
comAll <-data.frame(years, com, percent)

comAll$Land <- "Commercial/Industrial"
colnames(comAll) <- c("Year", "Amouunt", "Percent Change", "Land")

percent_change <- rbind(agrAll, agr_largeAll, single_all, single_urban_all, multAll, comAll)
write.csv(percent_change, "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/percent_change.csv" )
percent_change<-read.csv("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/land_parcel/percent_change.csv")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

percent_change$A
ggplot(aes(Year, Amount, group = Land, color = Land), data = percent_change) + 
  geom_line(size = 2, linetype = "dashed") + scale_color_manual(values=cbPalette)+ 
  labs(title = "Percent Change of Land Parcels from 2013- 2020", 
       x= "")
  
  
  

#View geometry type of our shape file
st_geometry_type(aoi_boundary_HARV)

#Check the CRS the data file is in
st_crs(aoi_boundary_HARV)

#check extent of AOI
st_bbox(aoi_boundary_HARV)

library(base)
#Inspect all the values of categorical variable StateClass
unique(aoi_boundary_HARV2017$PopClass)


state_class_colors <- c("blue", "green", "red", "cyan", "magenta", "yellow")
class_levels <- c("Agricultural/Undeveloped (20 – 99 acres)", "Agricultural/Undeveloped (100 acres and up)","Single-Family Residential(Suburban 0-19.99 acres)" ,
                   "Single Family Residential(Urban)", "Commercial/Industrial", "Multi-Family")
class_pal <- colorFactor(pal = state_class_colors, 
                          levels = class_levels)

library(rmapshaper)
library(leaflet)
library(shinycssloaders)
land <- ms_simplify(readOGR(dsn = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/Parcels_with_Class/", 
                            layer="Parcels_with_Class"))

land_t <- ms_simplify(st_read("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/Parcels_with_Class/Parcels_with_Class.shp")) 

state_class_colors <- c("#9dfef8","#00cc00","#2621ff","#ffa500","#ffff19","#ff0000" )
land@data$PropClass


land%>% leaflet(
  options = leafletOptions(
    minZoom = 0, maxZoom= 18,
    drag = FALSE)) %>% addTiles() %>%
  addPolygons(MAP_2017_91_T_o, pal, M0, M0_labels, "M0") %>%
  clearControls() %>%
  htmlwidgets::prependContent(html_fix)
  
  leaflet(options = leafletOptions(minZoom = 10)) %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(data = land, 
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7) 
    # addPolygons(data = agr, 
    #             stroke = FALSE,
    #             smoothFactor = 0,
    #             fillOpacity = 0.7,
    #             fillColor = state_class_colors[2] ,
    #             group = "Agricultural/Undeveloped (20 – 99 acres)") %>%
    # addPolygons(data = agr_large, 
    #             stroke = FALSE,
    #             smoothFactor = 0,
    #             fillOpacity = 0.7,
    #             fillColor = state_class_colors[3],
    #             group = "Agricultural/Undeveloped (100 acres and up)") %>%
    # addPolygons(data = single, 
    #             stroke = FALSE,
    #             smoothFactor = 0,
    #             fillOpacity = 0.7,
    #             fillColor = state_class_colors[4],
    #             group = "Single-Family Residential(Suburban 0-19.99 acres)") %>%
    # addPolygons(data = single_urban, 
    #             stroke = FALSE,
    #             smoothFactor = 0,
    #             fillOpacity = 0.7,
    #             fillColor = state_class_colors[5],
    #             group = "Single Family Residential(Urban)") %>%
    # addPolygons(data = mult, 
    #             stroke = FALSE,
    #             smoothFactor = 0,
    #             fillOpacity = 0.7,
    #             fillColor = state_class_colors[6],
    #             group = "Multi-Family") %>%
    # addLayersControl(
    #   position = "bottomright",
    #   overlayGroups = c("Agricultural/Undeveloped (20 – 99 acres)",
    #                     "Agricultural/Undeveloped (100 acres and up)",
    #                     "Single-Family Residential(Suburban 0-19.99 acres)",
    #                     "Single Family Residential(Urban)",
    #                     "Multi-Family",
    #                     "Commercial/Industrial"), 
    #   options = layersControlOptions(collapsed = FALSE)) 


agr <- land %>%
  filter(land@data$PropClass == "Agricultural/Undeveloped (20 – 99 acres)")

agr_large <- land_t %>% 
  filter(PropClass == "Agricultural/Undeveloped (100 acres and up)")%>%
  st_transform(crs = "+init=epsg:4326")

#single household 
single <- land_t %>% 
  filter(PropClass == "Single-Family Residential(Suburban 0-19.99 acres)")%>%
  st_transform(crs = "+init=epsg:4326")
# single urban household 
single_urban <- land_t %>% 
  filter(PropClass == "Single Family Residential(Urban)")%>%
  st_transform(crs = "+init=epsg:4326")
# multi
mult <- land_t %>% 
  filter(PropClass == "Multi-Family") %>%
  st_transform(crs = "+init=epsg:4326")
# commerical
com <- land_t %>% 
  filter(PropClass == "Commercial/Industrial")%>%
  st_transform(crs = "+init=epsg:4326")


land%>%
leaflet(options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(provider = "CartoDB.Positron")%>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Agricultural/Undeveloped (20 – 99 acres)",
                      "Agricultural/Undeveloped (100 acres and up)",
                      "Single-Family Residential(Suburban 0-19.99 acres)",
                      "Single Family Residential(Urban)",
                      "Multi-Family",
                      "Commercial/Industrial"), 
    options = layersControlOptions(collapsed = FALSE)) 






leaflet(options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(data = com, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = state_class_colors[1],
              group = "Commercial/Industrial") %>%
  addPolygons(data = agr, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = state_class_colors[2] ,
              group = "Agricultural/Undeveloped (20 – 99 acres)") %>%
  addPolygons(data = agr_large, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = state_class_colors[3],
              group = "Agricultural/Undeveloped (100 acres and up)") %>%
  addPolygons(data = single, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = state_class_colors[4],
              group = "Single-Family Residential(Suburban 0-19.99 acres)") %>%
  addPolygons(data = single_urban, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = state_class_colors[5],
              group = "Single Family Residential(Urban)") %>%
  addPolygons(data = mult, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = state_class_colors[6],
              group = "Multi-Family")


#Filter out parcels based on property type and map seperately for agricultural/residential/industrial land usage
#2017
agr17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Agricultural/Undeveloped (20 – 99 acres)")%>%
  st_set_crs(st_crs(aoi_boundary_HARV))%>%
  st_transform(crs="+init=epsg:4326") 

agr_large17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Agricultural/Undeveloped (100 acres and up)")%>%
  st_set_crs(st_crs(aoi_boundary_HARV))%>%
  st_transform(crs="+init=epsg:4326") 

#single household 
single17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Single-Family Residential(Suburban 0-19.99 acres)")%>%
  st_set_crs(st_crs(aoi_boundary_HARV))%>%
  st_transform(crs="+init=epsg:4326") 
# single urban household 
single_urban17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Single Family Residential(Urban)")%>%
  st_set_crs(st_crs(aoi_boundary_HARV))%>%
  st_transform(crs="+init=epsg:4326") 
# multi
mult17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Multi-Family") %>%
  st_set_crs(st_crs(aoi_boundary_HARV))%>%
  st_transform(crs="+init=epsg:4326") 
# commerical
com17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Commercial/Industrial")%>%
  st_set_crs(st_crs(aoi_boundary_HARV))%>%
  st_transform(crs="+init=epsg:4326") 


write_rds(agr17, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr17.rds") 
agr17 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr17.rds")
write_rds(agr_large17, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr_large17.rds") 
agr_large17 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr_large17.rds")

write_rds(single17, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/single17.rds") 
single17 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/single17.rds")

write_rds(single_urban17, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/single_urban17.rds") 
single_urban17 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/single_urban17.rds")

write_rds(mult17, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/mult17.rds") 
mult17 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/mult17.rds")

write_rds(com17, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/com17.rds") 
com17 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/com17.rds")


leaflet(options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(data = com17, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7) 


agr_large13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Agricultural/Undeveloped (100 acres and up)")%>%
  st_transform(crs = "+init=epsg:4326")


#single household 
single13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Single-Family Residential(Suburban 0-19.99 acres)")%>%
  st_transform(crs = "+init=epsg:4326")
# single urban household 
single_urban13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Single Family Residential(Urban)")%>%
  st_transform(crs = "+init=epsg:4326")
# multi
mult13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Multi-Family") %>%
  st_transform(crs = "+init=epsg:4326")
# commerical
com13 <- aoi_boundary_HARV2013 %>% 
  filter(PropClass == "Commercial/Industrial")%>%
  st_transform(crs = "+init=epsg:4326")




write_rds(agr_large13, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr_large13.rds") 
write_rds(single13, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/single13.rds") 
write_rds(single_urban13, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/single_urban13.rds") 
write_rds(mult13, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/mult13.rds") 
write_rds(com13, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/com13.rds") 
write_rds(agr_large13, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr_large13.rds") 
agr_large13 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr_large13.rds")
single13 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/single13.rds")
single_urban13 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/single_urban13.rds")
mult13 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/mult13.rds")
com13 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/com13.rds")




#nrow(lu_interest_HARV)

# ggplot() + 
#   geom_sf(data = lu_interest_HARV, aes(fill=PropClass)) + 
#   scale_color_manual(values = state_class_colors) +
#   ggtitle("Land Usage Classification", subtitle = "Agricultural/Undeveloped (20 – 99 acres)") + 
#   coord_sf()

agr_large17 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Agricultural/Undeveloped (100 acres and up)")
agr_large13 <- aoi_boundary_HARV2017 %>% 
  filter(PopClass == "Agricultural/Undeveloped (100 acres and up)")

leaflet(options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(data = agr_large17, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7) 

write_rds(agr_large13, file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr_large13.rds") 
agr_large13 <- readRDS("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/agr_large13.rds")
  
#nrow(lu_interest_HARV)

ggplot() +
  geom_sf(data = agr_large17, aes(fill=PopClass)) +
  scale_color_manual(values = state_class_colors) +
  ggtitle("Land Usage Classification", subtitle = "Agricultural/Undeveloped (100 acres and up)") +
  coord_sf()

leaflet(options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(data = agr_large17, 
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~class_pal(PropClass),
              group = "Agricultural/Undeveloped (100 acres and up)") 

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

