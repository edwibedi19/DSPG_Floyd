
setwd("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/")

virginiaCounty <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/VirginiaAdministrativeBoundary.shp/VirginiaCounty.shp")
f <- virginiaCounty[5,] 


areawater2 <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/tl_2020_51063_areawater/tl_2020_51063_areawater.shp")
edges2 <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/tl_2020_51063_edges/tl_2020_51063_edges.shp")

# shows 6 polygons of FLoyd 
tract20 <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/DSPG-Floyd/data/tl_2020_51063_tract20/tl_2020_51063_tract20.shp")

faces <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/tl_2020_51063_faces/tl_2020_51063_faces.shp")

ggplot()+ 
  geom_sf(mapping = aes(geometry = geometry), data = f) + 
  geom_sf(mapping = aes(geometry = geometry), data = areawater2,  color = "blue") + 
  labs(title = "Streams and Water bodies in Floyd")


ggplot()+ 
  geom_sf(mapping = aes(geometry = geometry), data = f) + 
  geom_sf(mapping = aes(geometry = geometry), data = edges2,  color = "red")

plot(tract20)
plot(faces)

library(leaflet)

colors <- c("dodgerblue4", "steelblue2","lightblue2", "cyan2", "skyblue1" )
options(sf_max.plot=1)
ggplot() + geom_sf(mapping = aes(geometry = geometry), data = tract20, fill = colors)+
  labs(title = "Subdivisions of Floyd County",
       caption = "Data Source: US Census Bureau",
       subtitle = "To protect the privacy of private well owners, we may locate the wells according to these subdivisions instead of exact coordinates. ") + 
  theme(plot.subtitle = element_text(size = 7))




  leaflet()%>%
    addPolygons(data=tract20$geometry ,color =colors) 
  
  leaflet(data = tract20, options = leafletOptions(minZoom = 10))%>%
    addPolygons(fillColor = ~pal(tract20$geometry),
                fillOpacity = 0.7,
                stroke = TRUE, weight = 0.5, color = "#202020",
                label = labels,
                labelOptions = labelOptions(direction = "bottom",
                                            style = list(
                                              "font-size" = "12px",
                                              "border-color" = "rgba(0,0,0,0.5)",
                                              direction = "auto"
                                            )))
                
  
  
  
  
  
  
  
  
  
  
  
  
  
  