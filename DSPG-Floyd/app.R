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

# data -----------------------------------------------------------
climate <- data.frame(read_excel("~/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/climate-floyd-county-usClimateData.xlsx")) 
wells <- data.frame(t(read_excel("~/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/NRV-wells-floyd-county-2011.xlsx", col_types = "numeric"))  ) 

colnames(wells) = c("Number_Wells", "Well_Depth", "Casing_Depth" , "Diameter", "Withdrawl_MGD", "Withdrawl_GPD", "Max_MGD", "Max_GPD", "Permitted")
wells <- wells[-1,]
wells$Names <- c("Christie", "Shortt", "Howard", "Rec.Park", "Comm. Cntr")




locations_features_floyd_county <- read_excel("~/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/data/locations-features-floyd-county.xlsx")

## chaning column names 
colnames(locations_features_floyd_county) <- locations_features_floyd_county[1,]

## only want certain columns and rows 

locations_features_floyd_county <- data.frame(locations_features_floyd_county[-c(1, 10,15:19),-c(2,4,6,8,10,11)])


## replacing the degree sign and N and W 
locations_features_floyd <- locations_features_floyd_county%>% 
    mutate(Latitude1 = str_replace_all(Latitude, "°", ""), Longitude1 = str_replace_all(Longitude, "°", "")
    ) %>%
    select(Feature.Name,Type,Latitude1, Longitude1)


locations_features_floyd <- locations_features_floyd%>% 
    mutate(Latitude = str_replace_all(Latitude1, "N", ""), Longitude = str_replace_all(Longitude1, "W", "")
    ) %>%
    select(Feature.Name,Type,Latitude, Longitude)

## need to change the Longitude and Latitude columns to numeric 
locations_features_floyd$Latitude <- as.numeric(locations_features_floyd$Latitude) 
locations_features_floyd$Longitude <- as.numeric(locations_features_floyd$Longitude) 

## need to multiply the Longitude by -1 since it is W 
locations_features_floyd <- locations_features_floyd%>% 
    mutate(Longitude1 = -1*Longitude
    ) %>%
    select(Feature.Name,Type, Latitude, Longitude1)

## getting county data for just floyd 
floyd <- left_join(countydata, counties, by = "county_fips") %>% 
    filter(state_name %in% c("Virginia"), county_name %in% c("Floyd County"))




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
                            status = "warning",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            h1("Water Management And Industry And Residential Growth In Floyd County"),
                            h2("Project Description")
                        ) 
                    ) 
            ), 
            tabItem(tabName = "intro",
                    fluidRow(
                        box(
                            title = "Introduction to Floyd County",
                            closable = FALSE,
                            width = NULL,
                            status = "warning",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            h2("Demographics of Floyd"),
                            p(), 
                            plotlyOutput("demos")
                        ) ,
                        box(
                            title = "Geology/Water Features",
                            closable = FALSE,
                            width = NULL,
                            status = "warning",
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
                            status = "warning",
                            solidHeader = TRUE,
                            collapsible = TRUE
                            
                        ) 
                    ) 
            ),
            tabItem(tabName = "wells",
                    fluidRow(
                        box(
                            title = "Private and Public Wells",
                            closable = FALSE,
                            width = NULL,
                            status = "warning",
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
    
    
    var1 <- reactive({
        input$var1
    })
    output$precipitation <- renderPlot({
        
        if(var1() == "rainfall") {
        
            
        }else if (var1() == "min") {
            
            
        }else {
            
            
        }
        
        
        
        
    })
    
    # Census tract plot 
    output$water <- renderPlot({
        
        
        
        
    })
    
    
    
    var2 <- reactive({
        input$var2
    })
    output$wells <- renderPlot({
        
        if(var2() == "gpd") {
            
            
        }else if (var2() == "depth") {
            
            
        }else {
            
            
        }
        
        
        
        
    })

    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
