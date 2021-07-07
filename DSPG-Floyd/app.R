library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(readxl)
library(urbnmapr)
library(rworldmap)
library(sp)
library(sf)
library(spatialEco)
library(FRK)
library(rgdal)

# data -----------------------------------------------------------
# County
floyd <- left_join(countydata, counties, by = "county_fips") %>% 
    filter(state_name %in% c("Virginia"), county_name %in% c("Floyd County")) 
floyd_df <- as.data.frame(floyd) 
points_poly <- df_to_SpatialPolygons(floyd_df, key = "group", coords = c("long","lat"), proj = CRS())
# Streams 
water <- readOGR("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/DSPG-Floyd/data/virginia_water/virginia_water.shp")
water_df <- fortify(water)
flo_w <- water_df %>% 
    filter(water_df$long > -80.6 & water_df$long < -80.1
           & water_df$lat < 37.2 & water_df$lat > 36.7) 
flo_w$group <- 1 
pts_w <- SpatialPointsDataFrame(flo_w, coords = flo_w[,1:2]) 

new_shape_w <- pts_w[points_poly,]
new_shape_df_w <- as.data.frame(new_shape_w)
# Springs 
nhdp <- readOGR("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/DSPG-Floyd/data/NHDPoint.shp")
nhdp_df <- as(nhdp, "data.frame")
pts_p <- SpatialPointsDataFrame(nhdp_df, coords = nhdp_df[,10:11]) 
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



climate <- data.frame(read_excel(paste0(getwd(),"/data/climate-floyd-county-usClimateData.xlsx"))) 
wells <- data.frame(t(read_excel(paste0(getwd(),"/data/NRV-wells-floyd-county-2011.xlsx"), col_types = "numeric")))  

colnames(wells) = c("Number_Wells", "Well_Depth", "Casing_Depth" , "Diameter", "Withdrawl_MGD", "Withdrawl_GPD", "Max_MGD", "Max_GPD", "Permitted")
wells <- wells[-1,]
wells$Names <- c("Christie", "Shortt", "Howard", "Rec.Park", "Comm. Cntr")

tract20 <- st_read(paste0(getwd(),"/data/tl_2020_51063_tract20/tl_2020_51063_tract20.shp")) 
st_transform(tract20, crs = st_crs("+proj=longlat +datum=WGS84"))

## Labor Market (Ryan)
industry <- read_excel(paste0(getwd(),"/data/Floyd Labor Market Statistics.xlsx")) 
industry_df <- as.data.frame(industry)

## Floyd County Employment by Industry
emp_df <- industry_df[1:24,1:2]
colnames(emp_df) <- c("Employment_by_Industry","Quantity")
emp_df$Quantity <- as.numeric(emp_df$Quantity)
emp_df[2,2] <- 0
emp_df[9,2] <- 0
## Floyd Population Change 
popch_df <- industry_df[34:38,1:3]
colnames(popch_df) <- c("Year","Population","percent_Change")
popch_df$Population <- as.numeric(popch_df$Population)
popch_df$percent_Change <- as.numeric(popch_df$percent_Change)
## Floyd County Population by Age
popage_df <- industry_df[54:71,1:2]
colnames(popage_df) <- c("Age","Quantity")
popage_df$Quantity <- as.numeric(popage_df$Quantity)
popage_df$Age <- factor(popage_df$Age, levels=unique(popage_df$Age))
## Floyd County Commuting
commute_df <- industry_df[79:81,1:2]
colnames(commute_df) <- c("Type","Quantity")
commute_df$Type <- factor(commute_df$Type, levels=unique(commute_df$Type))
commute_df$Quantity <- as.numeric(commute_df$Quantity)
## New Business Growth in Floyd 
busgrowth_df <- industry_df[103:115,1:2]
colnames(busgrowth_df) <- c("Time","Quantity")
busgrowth_df$Time <- factor(busgrowth_df$Time, levels=unique(busgrowth_df$Time))
busgrowth_df$Quantity <- as.numeric(busgrowth_df$Quantity)


# sidebar -----------------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        menuItem(
            tabName = "overview",
            text = "Project Overview",
            icon = icon("info circle")),
        menuItem(
            tabName = "intro",
            text = "Introduction to Floyd County",
            icon = icon("database")) ,
        menuItem(
            tabName = "data",
            text = "Data and Methodology",
            icon = icon("database")) ,
        menuItem(
            tabName = "economics",
            text = "Economic and Business Trends",
            icon = icon("database")) , 
        menuItem(
            tabName = "wells",
            text = "Well Data",
            icon = icon("database"))
    ) 
) 

# body -----------------------------------------------------------
body <- dashboardBody(
    fluidPage(
        tabItems(
            ## Tab Overview--------------------------------------------
            tabItem(tabName = "overview",
                    fluidRow(
                        box(
                            title = "Project Overview",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            h1("Water Management And Industry And Residential Growth In Floyd County"),
                            h2("Project Description"),
                            p(""),
                            plotlyOutput("census")
                        ) 
                    ) 
            ), 
            ## Tab Introduction--------------------------------------------
            tabItem(tabName = "intro",
                    fluidRow(
                        box(
                            title = "Introduction to Floyd County",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            h2("Demographics of Floyd")
                            # p(), 
                            # plotlyOutput("demos")
                        ) ,
                        box(
                            title = "Geology/Water Features",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            h1("Geology and Water Feautres"), 
                            plotlyOutput("water"), 
                            selectInput("var1", "Select Variable:", width = "100%", choices = c(
                                "Rainfall" = "rainfall",
                                "Minimum Temeprature" = "min", 
                                "Maximum Temeprature" = "max")
                            ),
                            plotlyOutput("precipitation")
                        ) 
                    ) 
            ), 
            ## Tab Data and Methodology--------------------------------------------
            tabItem(tabName = "data",
                    fluidRow(
                        box(
                            title = "Data and Methodology",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            h1("Data and Methodology"),
                            p(),
                            p()
                            
                            
                        ) 
                    ) 
            ),
            ## Tab Economics--------------------------------------------
            tabItem(tabName = "economics",
                    fluidRow(
                        box(
                            title = "Resident and Commerial Development",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE, 
                            p(), 
                            selectInput("econ1", "Select Variable:", width = "100%", choices = c(
                                "Employment by Industry" = "industry",
                                "Projected Population Change" = "pop", 
                                "Population by Age" = "age", 
                                "Number of Commuters" = "commute", 
                                "New Business Growth" = "business")
                            ),
                            plotlyOutput("trend1"),
                            p(tags$small("Data Source: Virginia Employment Commission"))
                            
                        ) 
                    ) 
            ), 
            ## Tab Wells and Water Quality --------------------------------------------
            tabItem(tabName = "wells",
                    fluidRow(
                        box(
                            title = "Private and Public Wells",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE, 
                            h1("Community Wells"), 
                            selectInput("var2", "Select Variable:", width = "100%", choices = c(
                                "Average Daily Withdrawals (GPD)" = "gpd",
                                "Well Depth with Percent of Usage" = "depth", 
                                "Maximum Daily Withdrawals" = "max")
                            ),
                            plotlyOutput("wells"),
                            p(tags$small("Data Source: New River Valley Water SUpply Plan 2011"))
                        ) 
                    ) 
            )
            
            
        ) 
    )
)
 



 
ui <- dashboardPage(
    dashboardHeader(title = "DSPG 2021"), 
    sidebar = sidebar, 
    body = body
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$census <- renderPlotly({
        
        # colors <- c("dodgerblue4", "steelblue2","lightblue2", "cyan2", "skyblue1" )
        # options(sf_max.plot=1)
        # ggplot() + geom_sf(mapping = aes(geometry = geometry), data = tract20, fill = colors)+
        #     labs(title = "Subdivisions of Floyd County",
        #          caption = "Data Source: US Census Bureau",
        #          subtitle = "To protect the privacy of private well owners, we may locate the wells according to these subdivisions instead of exact coordinates. ") + 
        #     theme(plot.subtitle = element_text(size = 7))
        
        
    })
    
    
    var1 <- reactive({
        input$var1
    })
    output$precipitation <- renderPlotly({
        
        if(var1() == "rainfall") {
            
            ggplot(climate, aes(fill = County, x = Month, y = Rainfall)) + 
                geom_bar(position="dodge", stat="identity") + 
                scale_x_discrete(limits = month.abb)+ 
                labs(title = "Average Monthly Ranfall from 3 surrounding counties in Virginia",
                     caption = "Data Source: US Climate Data",
                     subtitle = "*This can be used to see how Floyd County comapres to its surrounding counties based on groundwater.", 
                     y="Rainfall (in)")+ theme( 
                         plot.subtitle = element_text(size = 9, color = "blue"))
        
            
        }else if (var1() == "min") {
            
            ggplot(climate, aes(fill = County, x = Month, y = Min_Temp)) + 
                geom_bar(position="dodge", stat="identity") + 
                scale_x_discrete(limits = month.abb) +
                labs(title = "Minimum Tempature from 3 surrounding counties in Virginia",
                     caption = "Data Source: US Climate Data",
                     subtitle = "*This can be used to see how Floyd County comapres to its surrounding counties based on if the groundwater freezes and melts during the Winter",
                     y = "Temperature (F)") + theme( 
                         plot.subtitle = element_text(size = 6, color = "blue"))
            
            
        }else {
            
            ggplot(climate, aes(fill = County, x = Month, y = Max_Temp)) + 
                geom_bar(position="dodge", stat="identity") + 
                scale_x_discrete(limits = month.abb) + 
                labs(title = "Maximum Tempature from 3 surrounding counties in Virginia",
                     caption = "Data Source: US Climate Data",
                     subtitle = "*This can be used to see how Floyd County comapres to its surrounding counties based on if the groundwater is getting evaporated",
                     y = "Temperature (F) ")+ theme( 
                         plot.subtitle = element_text(size = 7, color = "blue"))
            
            
        }
        
        
        
        
    })
    
    # Census tract plot 
    output$water <- renderLeaflet({
        
        
        features <- unique(water_springs$feature)
        Pillar_pal <- colorFactor(pal = c('deepskyblue3', 'magenta2'), 
                                  levels = features)
        
        ## interavtive map of springs and streams in Floyd with two points for Town of Floyd and Floyd Quarry 
        floyd_map <- water_springs %>% 
            leaflet(options = leafletOptions(minzoom = 19)) %>% 
            setView(lng = -80.3, lat = 36.91, zoom = 9.5) %>% 
            addProviderTiles("CartoDB") %>% 
            addCircleMarkers(lng = ~long, lat = ~lat,  radius = 1, color = ~Pillar_pal(feature)) %>% 
            addMarkers(lng = -80.31891779181245, lat = 36.91313331126569, popup = "Town of Floyd") %>% 
            addMarkers(lng = -80.25908794232855, lat = 36.90665582434524, popup = "Floyd Quarry") %>% 
            addLegend(title = "Feature", position = "bottomleft", pal = Pillar_pal, values = features) %>%
            addPolygons(data = f,
                        fillColor = "black",
                        fillOpacity = .5,
                        stroke = FALSE) 
        floyd_map
        
        
    })
    
    
    
    var2 <- reactive({
        input$var2
    })
    output$wells <- renderPlotly({
        
        if(var2() == "gpd") {
            
            wells %>% 
                ggplot(aes(x = Names, y = Withdrawl_GPD)) + 
                geom_bar(stat = "identity", position = "dodge", fill = rgb(0.9,0.1,0.1,0.9))+ 
                labs(title = "Average Daily Withdrawals of Public Wells", 
                     subtitle = "*Note this data is from 2011 so the average daily withdrawal may be different now. ",
                     x = "Wells",
                     y = "Withdrawal (GPD) ") + 
                theme(legend.position = "none", 
                      plot.subtitle = element_text(color = "blue", size = 9), 
                      plot.caption = element_text(color = "black", face = "italic", size = 7))
            
            
        }else if (var2() == "depth") {
            wells$Percent <- wells$Withdrawl_GPD/wells$Max_GPD
            
            ggplot(wells, aes(x = Names)) + 
                geom_col(aes(y = Percent), fill = "darkseagreen1")  + 
                geom_line(aes(y = Well_Depth/1000), color = "navyblue", size = 1, group = 1) +
                scale_y_continuous("Percent", sec.axis = sec_axis(~.*1000, name = "Depth (ft)"))  + 
                labs(title = "Percent of Usage and Well Depth of Wells in Floyd County",
                     subtitle = "*could be a potential correlation of well depth compared to the average daily withdrawal amount",x = "Wells") + theme(plot.subtitle = element_text(color = "blue", size = 7), 
                                                                                                                                                       plot.caption = element_text(color = "black", face = "italic", size = 7))
            
            
        }else {
            
            wells %>% 
                ggplot(aes(x = Names, y = Max_GPD)) + 
                geom_bar(stat = "identity", position = "dodge",fill = rgb(0.9,0.4,0.4,0.9))+ 
                labs(title = "Maximum Daily Withdrawals of Public Wells", 
                     subtitle = "*Note this data is from 2011 so the average daily withdrawal may be different now",
                     x = "Wells",
                     y = "Withdrawal (GPD)") + 
                theme(legend.position = "none", 
                      plot.subtitle = element_text(color = "blue", size = 9), 
                      plot.caption = element_text(color = "black", face = "italic", size = 7))
            
            
        }
        
        
        
        
    })
    
    
    econ1 <- reactive({
        input$econ1
    })
    
    output$trend1 <- renderPlotly({
        
        if(econ1() == "industry") {
            
            ggplot(data = emp_df,mapping = aes(Employment_by_Industry,Quantity))+geom_bar(stat = "identity",fill="steelblue2")+
                labs(title = "Floyd County Employment by Industry", 
                     subtitle = "* indicates non-disclosable data",
                     caption = "Data Source: Virginia Employment Commission, Economic Information & Analytics,
                        Quarterly Census of Employment and Wages (QCEW), 4th", 
                     x="Industry") + coord_flip()
            
            
        }else if (econ1() == "commute") {
            ggplot(data = commute_df,mapping = aes(Type,Quantity))+geom_bar(stat = "identity",fill="powderblue")+
                labs(title = "Floyd County Commuting",
                     caption = "Data Source: U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination 
                        Employment Statistics, 2014") + 
                coord_flip()
            
        }else if (econ1() == "pop"){ 
            
            ggplot(popch_df, aes(x=Year,y=Population,group=1)) +
                geom_line()+geom_point()+
                labs(title = "Projected Floyd Population Change",
                     caption = "Data Source: U.S. Census Bureau, Weldon Cooper Center for Public Service")
            
            
            
        }else if (econ1() == "age") {
            
            ggplot(data = popage_df,mapping = aes(Age,Quantity))+geom_bar(stat = "identity",fill="dodgerblue2")+
                labs(title = "Floyd County Population by Age",
                     caption = "Data Source: 2010 Census") + coord_flip()
            
            
            
        }else {
            
            ggplot(busgrowth_df, aes(x=Time,y=Quantity,group=1)) +
                geom_line()+geom_point()+
                labs(title = "New Business Growth in Floyd",
                     caption = "Data Source: Virginia Employment Commission, Economic Information & Analytics, 
                        Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")
            
            
        }
        
        
    }) 
    
    

    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
