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
library(shiny)


water <- readOGR( 
  dsn= "virginia_water/" , 
  layer="virginia_water"
)

water <- readOGR( 
  dsn= "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/DSPG-Floyd/data/virginia_water" , 
  layer="virginia_water"
)


## Water bodies, lakes 
water_df <- fortify(water)
flo_w <- water_df %>% 
  filter(water_df$long > -80.6 & water_df$long < -80.1
         & water_df$lat < 37.2 & water_df$lat > 36.7) 
flo_w$group <- 1 

## Forests, lakes, wetlands 
# natural_df <- fortify(natural)
# flo_n <- natural_df %>% 
#   filter(natural_df$long > -80.6 & natural_df$long < -80.1 & 
#            natural_df$lat < 37.2 & natural_df$lat > 36.7) 
# flo_n$group <- 2 


## simple point plots 
# ggplot(flo_n, aes(x = long, y = lat)) +
#   geom_point() + 
#   ggtitle("Floyd County Forests, Lakes, Wetlands ")
# 
# 
# ggplot(flo_w, aes(x = long, y = lat)) +
#   geom_point() + 
#   ggtitle("Floyd County Water bodies, Lakes, Ponds")


## getting county data for just floyd 
va_counties <- counties(state = "VA", cb = TRUE)
floyd <- va_counties %>%
  filter(NAME == "Floyd")

floyd <- left_join(countydata, counties, by = "county_fips") %>% 
  filter(state_name %in% c("Virginia"), county_name %in% c("Floyd County")) 

# turning into SpatialPointsDF 
pts_w <- SpatialPointsDataFrame(flo_w, coords = flo_w[,1:2]) 
# pts_n <- SpatialPointsDataFrame(flo_n, coords = flo_n[,1:2]) 

floyd_df <- as.data.frame(floyd) 

## turning floyd_df into spatialPolygon 
points_poly <- df_to_SpatialPolygons(floyd_df, key = "group", coords = c("long","lat"), proj = CRS()) 


# this is filtering the points 
new_shape_w <- pts_w[points_poly,]
new_shape_df_w <- as.data.frame(new_shape_w)

# this is filtering the points 
# new_shape_n <- pts_n[points_poly,]
# new_shape_df_n <- as.data.frame(new_shape_n)

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


map <- ggplot(floyd, aes(long, lat)) +
  geom_polygon(color = "white", size = 0.05, fill = "grey") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.position = "right",
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 7),
        legend.key.height = unit(.25, "in"),
        plot.caption = element_text(size = 4),
        plot.subtitle = element_text(color = "blue", size = 8)) +
  scale_colour_manual(labels = c("Water bodies", "Forests", "Springs"), values=c("dark blue","green", "red")) + 
  geom_point(data=new_df, aes(long, lat, group = group, color = as.factor(group)), inherit.aes = FALSE, alpha = 0.1, size = 3)  +  
  labs(title = "Floyd County Features",
       subtitle = "*Land and water features",
       caption = "Data Source: https://mapcruzin.com/free-united-states-shapefiles/free-virginia-arcgis-maps-shapefiles.htm", 
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

names <- c("long", "lat", "order", "hole", "piece", "id", "group", "long.1", "lat.1")
data <- c(-80.26592, 37.08154, 1, FALSE, 1, 300, 3, -80.26592, 37.08154)
springs <- t(data.frame(data)) 
colnames(springs) <- names
water_springs <- rbind(new_shape_df_w, springs) %>%
  select(long, lat, group)

water_springs <- water_springs %>%
  mutate(feature = case_when(group == 1 ~ 'Water Body', 
                             group == 3 ~ 'Spring'
  ))


library(leaflet)
library(leaflet.extras)


features <- unique(water_springs$feature)
Pillar_pal <- colorFactor(pal = c('blue', 'red'), 
                          levels = features)
## interavtive map of springs and streams in Floyd with two points for Town of Floyd and Floyd Quarry 

  leaflet(options = leafletOptions(minzoom = 19)) %>% 
  setView(lng = -80.3, lat = 36.91, zoom = 9.5) %>% 
  addProviderTiles("CartoDB") %>% 
  addPolygons(data = f,
              stroke = FALSE) 
floyd_map



## outline of Floyd
virginiaCounty <- st_read("/Users/julierebstock/Desktop/VirginiaAdministrativeBoundary.shp/VirginiaCounty.shp")
f <- virginiaCounty[5,]%>%st_transform(crs = "+init=epsg:4326") %>%

library(dplyr)
library(tidyverse)
library(tidycensus)

## try to get block level 

home <- get_acs(geography = "block group", 
                     variables = "B25077_001", 
                     state = "VA",
                     county = "Floyd County",
                     geometry = TRUE)

income <- get_acs(geography = "tract",
                  variables = "B19013_001",
                  state = "VA",
                  county = "Floyd County",
                  geometry = T) 

age <- get_acs(geography = "tract", 
                variables = "B01002_001", 
                state = "VA",
                county = "Floyd County",
                geometry = TRUE)


pal <- colorNumeric(palette = "viridis", 
                    domain = home$estimate)
pal <- colorNumeric(palette = "viridis", 
                    domain = income$estimate)
pal <- colorNumeric(palette = "viridis", 
                           domain = age$estimate)
pal_unemployed <- colorNumeric(palette = "viridis", 
                            domain = unemployed$estimate)

labels <- lapply(
  paste("<strong>Area: </strong>",
        home$NAME,
        "<br />",
        "<strong>Home Value: </strong>",
        formatC(home$estimate, format = "f", big.mark =",", digits = 0)),
  htmltools::HTML
)


home %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              label = labels,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Median Home Value",
            labFormat = labelFormat(prefix = "$"),
            opacity = .7)


labels <- lapply(
  paste("<strong>Area: </strong>",
        income$NAME,
        "<br />",
        "<strong>Home Value: </strong>",
        formatC(income$estimate, format = "f", big.mark =",", digits = 0)),
  htmltools::HTML
)

income %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              label = labels,
              color = ~ pal_income(estimate)) %>%
  addLegend("bottomright", 
            pal = pal_income, 
            values = ~ estimate,
            title = "Household Income",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)


age %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal_age(estimate)) %>%
  addLegend("bottomright", 
            pal = pal_age, 
            values = ~ estimate,
            title = "Median Age",
            labFormat = labelFormat(suffix = ""),
            opacity = 1)

labels <- lapply(
  paste("<strong>Area: </strong>",
        unemployed$NAME,
        "<br />",
        "<strong>Home Value: </strong>",
        formatC(unemployed$estimate, format = "f", big.mark =",", digits = 0)),
  htmltools::HTML
)



unemployed %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              label = labels,
              color = ~ pal_unemployed(estimate)) %>%
  addLegend("bottomright", 
            pal = pal_unemployed, 
            values = ~ estimate,
            title = "Total Unemployed",
            labFormat = labelFormat(prefix = ""),
            opacity = .7)





employment_status <- get_acs(geography = "block group",
                          variables = "B23025_005" ,
                          state = "VA",
                          county = "Floyd County",
                          geometry = TRUE, 
                          summary_var = "B23025_003")

employment_status <-employment_status %>%
  mutate(rate = as.numeric(estimate)/as.numeric(summary_est)*100) 
pal <- colorNumeric(palette = "viridis", 
                               domain = employment_status$rate)
labels <- lapply(
  paste("<strong>Area: </strong>",
        employment_status$NAME,
        "<br />",
        "<strong>Home Value: </strong>",
        formatC(employment_status$rate, format = "f", digits = 3)),
  htmltools::HTML
)
employment_status %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              label = labels,
              color = ~ pal(rate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ rate,
            title = "Unemployment Rate",
            labFormat = labelFormat(suffix = "%"),
            opacity = .7)



education <- get_acs(geography = "block group",
                       variables = c("B15003_017", "B15003_022","B15003_023","B15003_025"),
                       state = "VA",
                       county = "Floyd County",
                       geometry = TRUE)
high <- education%>%
  filter(variable == "B15003_017")
bach <- education%>%
  filter(variable == "B15003_022")
mast <- education%>%
  filter(variable == "B15003_023")
doct <- education%>%
  filter(variable == "B15003_025")


library(tidycensus)
commute <- get_acs(geography = "tract",
                     variables = c("B08135_002", "B08135_003",
                                   "B08135_004","B08135_005",
                                   "B08135_006","B08135_007","B08135_008",
                                   "B08135_009","B08135_010"),
                     state = "VA",
                     county = "Floyd County",
                     geometry = TRUE
                    )

# weighted averages of commute time 
f <- commute %>%
  filter(NAME =="Census Tract 9202, Floyd County, Virginia")
f_average <- (f[1,4]$estimate*5 + f[2,4]$estimate*12+  f[3,4]$estimate*17 + f[4,4]$estimate*22 + 
  f[5,4]$estimate*27 + f[6,4]$estimate*32 + f[7,4]$estimate*40 + f[8,4]$estimate*51+  
    f[9,4]$estimate*60) /sum(f$estimate)

f <- commute %>%
  filter(NAME =="Census Tract 9201.01, Floyd County, Virginia")
f2_average <- (f[1,4]$estimate*5 + f[2,4]$estimate*12+  f[3,4]$estimate*17 + f[4,4]$estimate*22 + 
                f[5,4]$estimate*27 + f[6,4]$estimate*32 + f[7,4]$estimate*40 + f[8,4]$estimate*51+  
                f[9,4]$estimate*60) /sum(f$estimate)

f <- commute %>%
  filter(NAME =="Census Tract 9201.02, Floyd County, Virginia")
f3_average <- (f[1,4]$estimate*5 + f[2,4]$estimate*12+  f[3,4]$estimate*17 + f[4,4]$estimate*22 + 
                 f[5,4]$estimate*27 + f[6,4]$estimate*32 + f[7,4]$estimate*40 + f[8,4]$estimate*51+  
                 f[9,4]$estimate*60) /sum(f$estimate)
new <- commute%>%
  select(NAME, geometry)

new <- new[c(1,10,19),]
new$average <- c(f_average, f2_average, f3_average)
pal <- colorNumeric(palette = "viridis", 
                    domain = new$average)
labels <- lapply(
  paste("<strong>Area: </strong>",
        new$NAME,
        "<br />",
        "<strong>Commute Time: </strong>",
        formatC(new$average, format = "f", digits = 2)),
  htmltools::HTML
)
new %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              label = labels,
              color = ~ pal(average)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ average,
            title = "Weighted Average Commute Time",
            labFormat = labelFormat(suffix = " mins"),
            opacity = .7)




poverty <- get_acs(geography = "tract",
                   variables = "B17020_002",
                   state = "VA",
                   county = "Floyd County",
                   geometry = TRUE)

total_block <-  get_acs(geography = "block group",
                        variables = "B01003_001",
                        state = "VA",
                        county = "Floyd County",
                        geometry = TRUE)

total_census <-  get_acs(geography = "tract",
                        variables = "B01003_001",
                        state = "VA",
                        county = "Floyd County",
                        geometry = TRUE)
food <-  get_acs(geography = "tract",
                         variables = "B22007_002",
                         state = "VA",
                         county = "Floyd County",
                         geometry = TRUE)

# John's Mining Data 
# Collection of the abandoned

library(ggplot2)
library(tidyverse)
library(tigris)
library(readr)
library(tmap)
library(leaflet)
library(sp)


options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
mines <- read_tsv("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/DSPG-Floyd/data/AbandonedMineralMineLands.txt")

# Filter the mine data for only those within Floyd County
mines_Floyd <- mines %>%
  filter(County == "Floyd")
head(mines_Floyd)


# Get Floyd County borders and water maps
va_counties <- counties(state = "VA", cb = TRUE)
floyd <- va_counties %>%
  filter(NAME == "Floyd")


floyd_water <- area_water(state = "VA", county = "Floyd")

head(floyd)
head(floyd_water)



plot(floyd$geometry)
plot(floyd_water$geometry, add = TRUE, color = "blue")


ggplot() +
  geom_sf(data = floyd) +
  geom_sf(data = floyd_water, color = "blue") +
  geom_point(data = mines_Floyd, aes(x = Lon, y = Lat), color = "red", size = 2) +
  coord_sf(datum = NA) +
  theme_minimal() +
  labs(title = "Floyd County Abandon Mines and Water Areas",
       x = "",
       y = "")

total_block <-  get_acs(geography = "block group",
                        variables = "B01003_001",
                        state = "VA",
                        county = "Floyd County",
                        geometry = TRUE)


total_block %>%
leaflet(options = leafletOptions(minzoom = 12)) %>% 
  setView(lng = -80.4, lat = 36.95, zoom = 10) %>% 
  addTiles()%>%
  addPolygons(fillOpacity = 0.01,
              color = "black", opacity = 1.0, weight = 1,
              label = lapply(
                paste("<strong>Area: </strong>",
                      total_block$NAME),
                htmltools::HTML))   %>%
  addMarkers(lng = -80.31891779181245, lat = 36.91313331126569, 
             label = lapply(
               paste("<strong>Town of Floyd</strong>",
                     "<br />"),
               htmltools::HTML)) %>%
  addCircleMarkers(lng = ~mines_Floyd$Lon,
                   lat = ~mines_Floyd$Lat,
                   radius = 5, fillOpacity = .2,
                    color = "red")










