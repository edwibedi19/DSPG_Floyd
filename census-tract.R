
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
  
  
  
  
  
  
  ## Economics Data 
  capita_income <- read_excel("shinyApp/data/capita_income.xlsx")[,1:3]

  ggplot(capita_income, aes(fill = Area, x = Year, y = Amount)) + 
    geom_bar(position="dodge", stat="identity") + 
    labs(title = "Income per Capita", 
         caption = "Data Source: U.S Census Bureau",
         y="Dollar ($) ")
  
  
  
  
  retail <- read_excel("shinyApp/data/retail-sales.xlsx")[,1:3]
  retail$Year <- as.character(retail$Year)
  
  ggplot(retail, aes(fill = Year, x = Retail, y = Sales/100000)) + 
    geom_bar(position="dodge", stat="identity") + 
    labs(title = "Retail Sales by Group", 
         caption = "Data Source: Virginia Department of Taxation and Weldon Cooper Center" ,
         y="Sales (100,000) ", x= "Retail Group")+ coord_flip() + 
    theme( plot.subtitle = element_text(size = 9, color = "blue"))
  
  
  
  
  unempl <- read_excel("shinyApp/data/unemployment.xlsx")[,1:3] 
  unempl$Year <- as.character(unempl$Year)

  ggplot(unempl, aes(group = Area, x = Year, y = Rate*100, color = Area)) + 
    geom_line(linetype = "dotted", size = 2) + 
    labs(title = "Unemployment Rate", 
         caption = "Data Source: Virginia Employment Commission" ,
         y="Sales (100,000) ", x= "Retail Group")+ 
    theme( plot.subtitle = element_text(size = 9, color = "blue"))
  
  
  library(readxl)
  library(ggplot2)
  industry_overtime <- read_excel("shinyApp/data/economics/industry-overtime.xlsx")
  industry_overtime$Estimate <- as.numeric(industry_overtime$Estimate)
  industry_overtime$Year <- as.character(industry_overtime$Year)
  
  ggplot(industry_overtime, aes(group = Label, x = Year, y = Estimate, color = Label)) + 
    geom_line() + 
    labs(title = "Industry Sector ", 
         caption = "Data Source: ACS 5-year Estimates" ,
         y="Persons ", x= "Sector")+ theme(legend.position="bottom")
  
  
  
  
  
  
  
  
  
  