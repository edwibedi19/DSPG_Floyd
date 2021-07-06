#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(leaflet)
library(plotly)
library(readxl)
library(urbnmapr)
library(rworldmap)
library(sp)
library(sf)
library(spatialEco)
library(FRK)
library(rgdal)

# data -----------------------------------------------------------
climate <- data.frame(read_excel("~/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/climate-floyd-county-usClimateData.xlsx")) 
wells <- data.frame(t(read_excel("~/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/NRV-wells-floyd-county-2011.xlsx", col_types = "numeric"))  ) 

colnames(wells) = c("Number_Wells", "Well_Depth", "Casing_Depth" , "Diameter", "Withdrawl_MGD", "Withdrawl_GPD", "Max_MGD", "Max_GPD", "Permitted")
wells <- wells[-1,]
wells$Names <- c("Christie", "Shortt", "Howard", "Rec.Park", "Comm. Cntr")


tract20 <- st_read(
    "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/tl_2020_51063_tract20/tl_2020_51063_tract20.shp")



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
            tabItem(tabName = "data",
                    fluidRow(
                        box(
                            title = "Data and Methodology",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE
                            
                        ) 
                    ) 
            ),
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
                            plotlyOutput("trend1")
                            
                        ) 
                    ) 
            ), 
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
                            plotlyOutput("wells")
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
        
        colors <- c("dodgerblue4", "steelblue2","lightblue2", "cyan2", "skyblue1" )
        options(sf_max.plot=1)
        ggplot() + geom_sf(mapping = aes(geometry = geometry), data = tract20, fill = colors)+
            labs(title = "Subdivisions of Floyd County",
                 caption = "Data Source: US Census Bureau",
                 subtitle = "To protect the privacy of private well owners, we may locate the wells according to these subdivisions instead of exact coordinates. ") + 
            theme(plot.subtitle = element_text(size = 7))
        
        
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
    output$water <- renderPlotly({
        
        
        features <- unique(water_springs$feature)
        Pillar_pal <- colorFactor(pal = c('blue', 'red'), 
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
                ggplot(aes(x = Names, y = Withdrawl_GPD, fill = Names)) + 
                geom_bar(stat = "identity", position = "dodge")+ 
                labs(title = "Average Daily Withdrawals of Public Wells", 
                     caption = "Data Source: New River Valley Water Supply Plan 2011", 
                     subtitle = "*Note this data is from 2011 so the average daily withdrawal may be different now. ",
                     x = "Wells",
                     y = "Withdrawal (GPD) ") + 
                theme(legend.position = "none", 
                      plot.subtitle = element_text(color = "blue", size = 9), 
                      plot.caption = element_text(color = "black", face = "italic", size = 7))
            
            
        }else if (var2() == "depth") {
            ggplot(wells, aes(x = Names)) + 
                geom_col(aes(y = Percent), fill = "white", color = "red")  + 
                geom_line(aes(y = Well_Depth/1000), color = "blue", size = 1, group = 1) +
                scale_y_continuous("Percent", sec.axis = sec_axis(~.*1000, name = "Depth (ft)"))  + 
                labs(title = "Percent of Usage and Well Depth of Wells in Floyd County",
                     caption = "Data Source: New River Valley Water Supply Plan 2011",
                     subtitle = "*could be a potential correlation of well depth compared to the average daily withdrawal amount",x = "Wells") + theme(plot.subtitle = element_text(color = "blue", size = 7), 
                                                                                                                                                       plot.caption = element_text(color = "black", face = "italic", size = 7))
            
            
        }else {
            
            wells %>% 
                ggplot(aes(x = Names, y = Max_GPD, fill = Names)) + 
                geom_bar(stat = "identity", position = "dodge")+ 
                labs(title = "Maximum Daily Withdrawals of Public Wells", 
                     caption = "Data Source: New River Valley Water Supply Plan 2011", 
                     subtitle = "*Note this data is from 2011 so the average daily withdrawal may be different now",
                     x = "Wells",
                     y = "Withdrawal (GPD)") + 
                theme(legend.position = "none", 
                      plot.subtitle = element_text(color = "blue", size = 9), 
                      plot.caption = element_text(color = "black", face = "italic", size = 7))
            
            
        }
        
        
        
        
    })

    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
