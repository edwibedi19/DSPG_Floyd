setwd("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/")

virginiaCounty <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/VirginiaAdministrativeBoundary.shp/VirginiaCounty.shp")
f <- virginiaCounty[5,] 


areawater2 <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/tl_2020_51063_areawater/tl_2020_51063_areawater.shp")
edges2 <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/tl_2020_51063_edges/tl_2020_51063_edges.shp")

# shows 6 polygons of FLoyd 
tract20 <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/tl_2020_51063_tract20/tl_2020_51063_tract20.shp")

faces <- st_read(
  "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/tl_2020_51063_faces/tl_2020_51063_faces.shp")

ggplot()+ 
  geom_sf(mapping = aes(geometry = geometry), data = f) + 
  geom_sf(mapping = aes(geometry = geometry), data = areawater2,  color = "blue") + 
  labs(title = "Streams and Water bodies in Floyd")


ggplot()+ 
  geom_sf(mapping = aes(geometry = geometry), data = f) + 
  geom_sf(mapping = aes(geometry = geometry), data = edges2,  color = "red")

plot(tract20)
plot(faces)

colors <- c("dodgerblue4", "steelblue2","lightblue2", "cyan2", "skyblue1" )
options(sf_max.plot=1)
ggplot() + geom_sf(mapping = aes(geometry = geometry), data = tract20, fill = colors)+
  labs(title = "Subdivisions of Floyd County",
       caption = "Data Source: US Census Bureau",
       subtitle = "To protect the privacy of private well owners, we may locate the wells according to these subdivisions instead of exact coordinates. ") + 
  theme(plot.subtitle = element_text(size = 7))