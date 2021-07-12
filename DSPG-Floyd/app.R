library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)
library(urbnmapr)
library(rworldmap)
library(sp)
library(sf)
library(spatialEco)
library(FRK)
library(rgdal)

# data -----------------------------------------------------------
# Demographics 
total_pop <- 15074 
home <- get_acs(geography = "block group", 
                variables = "B25077_001", 
                state = "VA",
                county = "Floyd County",
                geometry = TRUE)

income <- get_acs(geography = "block group",
                  variables = "B19013_001",
                  state = "VA",
                  county = "Floyd County",
                  geometry = T) 

age <- get_acs(geography = "block group", 
               variables = "B01002_001", 
               state = "VA",
               county = "Floyd County",
               geometry = TRUE)

employment_status <- get_acs(geography = "block group",
                             variables = "B23025_005" ,
                             state = "VA",
                             county = "Floyd County",
                             geometry = TRUE, 
                             summary_var = "B23025_003")

employment_status <-employment_status %>%
    mutate(rate = as.numeric(estimate)/as.numeric(summary_est)*100)

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

food <-  get_acs(geography = "tract",
                 variables = "B22007_002",
                 state = "VA",
                 county = "Floyd County",
                 geometry = TRUE)

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

commute <- get_acs(geography = "tract",
                   variables = c("B08135_002", "B08135_003",
                                 "B08135_004","B08135_005",
                                 "B08135_006","B08135_007","B08135_008",
                                 "B08135_009","B08135_010"),
                   state = "VA",
                   county = "Floyd County",
                   geometry = TRUE
)
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


# County
va_counties <- counties(state = "VA", cb = TRUE)
floyd <- va_counties %>%
    filter(NAME == "Floyd")

virginiaCounty <- st_read(
    "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/DSPG-Floyd/data/VirginiaAdministrativeBoundary.shp/VirginiaCounty.shp")
f <- virginiaCounty[5,] 
areawater2 <- st_read(
    "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/DSPG-Floyd/data/tl_2020_51063_areawater/tl_2020_51063_areawater.shp")

mines <- read_tsv("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/DSPG-Floyd/data/AbandonedMineralMineLands.txt")

# Filter the mine data for only those within Floyd County
mines_Floyd <- mines %>%
    filter(County == "Floyd")


# rainfall and temperatures 
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
            icon = icon("leaf")) ,
        menuItem(
            tabName = "data",
            text = "Data and Methodology",
            icon = icon("server")) ,
        menuItem(
            tabName = "geology",
            text = "Geology and Water Features",
            icon = icon("server")), 
        menuItem(
            tabName = "economics",
            text = "Economic and Business Trends",
            icon = icon("server")) , 
        menuItem(
            tabName = "wells",
            text = "Well Data",
            icon = icon("database")), 
        menuItem(
            tabName = "conclusion",
            text = "Conclusion",
            icon = icon("check-double")),
        menuItem(
            tabName = "team",
            text = "Team",
            icon = icon("users"))
    ) 
) 

# body -----------------------------------------------------------
body <- dashboardBody(
    fluidPage(
        tabItems(
            ## Tab Overview--------------------------------------------
            tabItem(tabName = "overview",
                    fluidRow(style = "margin: 2px;",
                             align = "center",
                             br(""),
                             h1(strong("Water Management And Industry And Residential Growth In Floyd County, VA"),
                                br(""),
                                h4("Data Science for the Public Good Program"),
                                h4("Virginia Tech"),
                                br()
                             ), 
                            column(4,
                                   h2(strong("Project Background")),
                                   p(strong("The problem."), "" ),
                                   p(),
                                   p(strong("The setting."), ""),
                                   p(),
                                   p(strong("The project."), "This University of Virginia", a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "Biocomplexity Institute", target = "_blank"),
                                     "Data Science for Public Good (DSPG) project aimed to build local capacity, leverage social and data science to address current and future resident well-being, and enhance
                                             data-driven decision making about rural health in Patrick County, Virginia.")
                            ),
                            column(4,
                                   h2(strong("Our Work")),
                                   p("Our research team worked closely with Patrick County Extension Office, Virginia Department of Health, and Healthy Patrick County coalition stakeholders
                                            to identify the county’s priority challenges in the area of health. The research team reviewed a prior", a(href = "https://www.vdh.virginia.gov/west-piedmont/2020/05/27/patrick-county-health-needs-improvement-plan-completed/",
                                                                                                                                                       "community health assessment,", target = "blank"), a(href = "https://www.pubs.ext.vt.edu/VCE/VCE-596/VCE-596-75/VCE-1002-75.html", "situation analysis", target = "_blank"),
                                     "relevant funding applications, and held a listening meeting with stakeholders to identify these challenges. Lack of
                                            data on health care access, food access as related to diabetes and heart disease prevalence, older adult health, and digital connectivity that would facilitate
                                            access to telemedicine emerged as key problems where providing actionable insights could address barriers to Patrick County residents’ health."),
                                   p(),
                                   p("We implemented the", a(href = "https://doi.org/10.1162/99608f92.2d83f7f5", "data science framework", target = "_blank"), "and identified, acquired, profiled, and used
                                            publicly available data to provide Patrick County with data-driven resources in each of the four priority areas. We:"),
                                   tags$li("Provided census tract- and census block group-level maps of Patrick County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                   tags$li("Created census tract-level maps on", strong("older adult health"), "to show the geographic distribution of older adults in the county by gender and
                                                  type of disability, identifying areas where providing telehealth or travelling preventive care services may be particularly important."),
                                   tags$li("Mapped residents'", strong("computing device and internet access"), "at census block group level, and constructed 10- and 15-minute isochrones (areas of equal travel time) from households to free
                                                  wifi hotspots to highlight internet gaps that could suggest where new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                   tags$li("Calculated and mapped", strong("emergency medical service (EMS) station coverage"), "of households within 8-, 10-, and 12-minute travel times, identifying areas difficult to reach within
                                                   standard EMS travel thresholds."),
                                   tags$li("Constructed", strong("food access"), "maps by census tract, 10- and 15-minute isochrones from households to grocery stores and farmers markets, and maps of food security resources in the county,
                                                highlighting food deserts and areas that could benefit from programs facilitating access to fresh produce."),
                                   p(),
                                   p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                            ),
                            column(4,
                                   h2(strong("Dashboard Aims")),
                                   p("Our dashboard is aimed at:"),
                                   p(strong("Floyd County extension professionals and the communities they serve."), ""),
                                   p(strong("Local health-related agencies and departments seeking data insights to inform their decision-making."), ""),
                                   p(strong("State government representatives in the Virginia Department of Health and the State Office of Rural Health."), ".")
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
                            br(),
                            column(4, 
                                   h4(strong("Who does Patrick County Serve?")),
                            p("We examined Floyd County population sociodemographic and socioeconomic characteristics to better understand the residents that the county serves. ") ,
                            
                             p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census tract levels. A
                              CS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity, 
                              employment, health insurance coverage, and other relevant characteristics."), 
                            
                              p("Our interactive plots visualize census block-group level sociodemographic characteristics of Floyd County residents.")) ,
                            br(), 
                            column(8,
                                   h4(strong("Map of Resident Socioeconomic Characteristics by Census Tract or Block Group")),
                            selectInput("demo1", "Select Variable:", width = "100%", choices = c(
                                "Population Median Household Income" = "income",
                                "Population Median Home Value" = "home", 
                                "Population Median Age" = "age" ,
                                "Unemployment Rate" = "unemploy",
                                "Population 25 and over with high school diploma" = "high",
                                "Population 25 and over with Bachelor's" = "bach",
                                "Population 25 and over with Master's" = "mast",
                                "Population 25 and over with Doctorate's" = "doct",
                                "Weighted Average Commute Time" = "commute",
                                "Population who received Food Stamps/SNAP within 12 months" = "food",
                                "Population with Income below the Poverty Level" = "poverty", 
                                "Total Population by Census Tract" = "census",
                                "Total Population by Block Group" = "block"
                                )
                            ), 
                            leafletOutput("demo"), 
                            p(tags$small("Data Source: American Community Survey 5-year estimate 2015/2019"))) 
                            
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
            ## Tab Geology--------------------------------------------
            tabItem(tabName = "geology",
                    fluidRow(
                        box(
                            title = "Water Retaintion",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE, 
                            p(), 
                            selectInput("var1", "Select Variable:", width = "100%", choices = c(
                                "Rainfall" = "rainfall",
                                "Minimum Temeprature" = "min", 
                                "Maximum Temeprature" = "max")
                            ),
                            plotlyOutput("precipitation"), 
                            p(tags$small("Data Source: US Climate "))
                            
                        ) ,
                        box(
                            title = "Geology and Water Features",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE, 
                            p(), 
                            plotlyOutput("water")

                        ) ,
                        box(
                            
                            title = "Abandoned and Active Mines",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE, 
                            p(), 
                            
                            
                            leafletOutput("mines")
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
            ),
            ## Tab Conclusion --------------------------------------------
            tabItem(tabName = "conclusion",
                    fluidRow(
                        box(
                            title = "Conclusion",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE, 
                            p(), 
                            p()
                        ) 
                    ) 
            ), 
            ## Tab Team --------------------------------------------
            tabItem(tabName = "team",
                    fluidRow(
                        box(
                            title = "Team",
                            closable = FALSE,
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            h2("Data Science for the Public Good Program"),
                            p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the Biocomplexity Institute’s Social and Decision Analytics Division (SDAD). In its eighth year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply, and our annual symposium, please visit the official Biocomplexity DSPG website."),
                            h2("2021 Loudoun County Summer Project"),
                            p("Our project goal was to identify the gaps in the services available for transitional aged youth in Loudoun County, VA. We visualized the programs by education, employment, housing, transportation, insurance and funding & policy and mapped their locations. Our team is comprised of talented individuals with a broad range of skills and experience."),
                            h2("DSPG Team Members"),
                            # change the images 
                            img(src = '', height = "150", width = "140", align = "center"),
                            img(src = 'team-julie.png', height = "150", width = "150", align = "center"),
                            img(src = '', height = "150", width = "140", align = "center"),
                            img(src = '', height = "150", width = "140", align = "center"),
                            br(),
                            br(),
                            p("Esha Dwibedi, Fellow ()"),
                            p("Julie Rebstock, Intern (Undergraduate Student at Virginia Tech, Computational Modeling and Data Anaylytics and Economics)"),
                            p("Ryan Jacobs, Intern (Undergraduate. Student at Virginia Tech, )"),
                            p("John Wright, Intern ()"),
                            h2("Virginia Tech Faculty Team Members"),
                            img(src = 'Susan.Chen.VT.jpg', height = "150", width = "140", align = "center"),
                            img(src = '', height = "150", width = "140", align = "center"),
                            img(src = '', height = "150", width = "140", align = "center"),
                            img(src = '', height = "150", width = "140", align = "center"),
                            br(),
                            br(),
                            p("Susan Chen (Associate Professor, Food and Health Economics, DSPG Project Co-Lead)"),
                            p("Brianna Posadas ()"),
                            p("Sarah M. Witiak ()"),
                            h2("Project Sponsors"),
                            img(src = '', height = "150", width = "200", align = "center", style="display: block; margin-left: auto; margin-right: auto;"),
                            h2("Acknowledgements"),
                            # Stakeholders 
                            p("We would like to thank:"),
                            p(" (),"),
                            p(" ()"),
                            p(" ()")
                        )
                    ))
            
            
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
    
    demo1 <- reactive({
        input$demo1
    })
    output$demo <- renderLeaflet({
        if(demo1() == "home") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = home$estimate)
            
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
            
            
            
        }else if(demo1() == "income") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = income$estimate)
            
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      income$NAME,
                      "<br />",
                      "<strong>Household Income: </strong>",
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
                            color = ~ pal(estimate)) %>%
                addLegend("bottomright", 
                          pal = pal, 
                          values = ~ estimate,
                          title = "Household Income",
                          labFormat = labelFormat(prefix = "$"),
                          opacity = 1)
            
            
        }else if (demo1() =="unemploy") {
            
            
            pal <- colorNumeric(palette = "viridis", 
                                domain = employment_status$rate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      employment_status$NAME,
                      "<br />",
                      "<strong>Unemployment Rate: </strong>",
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
            
            
        }
        else if (demo1() == "age") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = age$estimate)
            
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      age$NAME,
                      "<br />",
                      "<strong>Median Age: </strong>",
                      formatC(age$estimate, format = "f", digits = 0)),
                htmltools::HTML
            )
            
            
            age %>%
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
                          title = "Median Age",
                          labFormat = labelFormat(),
                          opacity = 1)
            
            
            
        }else if (demo1() =="high") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = high$estimate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      high$NAME,
                      "<br />",
                      "<strong>Population Estimate: </strong>",
                      formatC(high$estimate, format = "f", digits = 0)),
                htmltools::HTML
            )
            high %>%
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
                          title = "High School Diploma",
                          labFormat = labelFormat(suffix = ""),
                          opacity = .7)
        }else if (demo1() =="bach") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = bach$estimate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      bach$NAME,
                      "<br />",
                      "<strong>Population Estimate: </strong>",
                      formatC(bach$estimate, format = "f", digits = 0)),
                htmltools::HTML
            )
            bach %>%
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
                          title = "Bachelor's Degree",
                          labFormat = labelFormat(suffix = ""),
                          opacity = .7)
        }else if (demo1() =="mast") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = mast$estimate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      mast$NAME,
                      "<br />",
                      "<strong>Population Estimate: </strong>",
                      formatC(mast$estimate, format = "f", digits = 0)),
                htmltools::HTML
            )
            mast %>%
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
                          title = "Master's Degree",
                          labFormat = labelFormat(suffix = ""),
                          opacity = .7)
        }else if(demo1() == "doct") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = doct$estimate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      doct$NAME,
                      "<br />",
                      "<strong>Population Estimate: </strong>",
                      formatC(doct$estimate, format = "f", digits = 0)),
                htmltools::HTML
            )
            doct %>%
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
                          title = "Doctorate Degree",
                          labFormat = labelFormat(suffix = ""),
                          opacity = .7)
        }else if (demo1() == "commute"){
            
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
        }
        else if(demo1() == "food") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = food$estimate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      food$NAME,
                      "<br />",
                      "<strong>Population Estimate: </strong>",
                      formatC(food$estimate, format = "f", digits = 0)),
                htmltools::HTML
            )
            food %>%
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
                          title = "Received Food Stamps/SNAP",
                          labFormat = labelFormat(suffix = ""),
                          opacity = .7)
        }else if(demo1() == "poverty") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = poverty$estimate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      poverty$NAME,
                      "<br />",
                      "<strong>Population Estimate: </strong>",
                      formatC(poverty$estimate, format = "f", digits = 0)),
                htmltools::HTML
            )
            poverty %>%
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
                          title = "Population Income below Poverty",
                          labFormat = labelFormat(suffix = ""),
                          opacity = .7)
        }else if (demo1() == "block") {
            pal <- colorNumeric(palette = "viridis", 
                                domain = total_block$estimate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      total_block$NAME,
                      "<br />",
                      "<strong>Total Population: </strong>",
                      formatC(total_block$estimate, format = "f",  big.mark =",",digits = 0)),
                htmltools::HTML
            )
            total_block %>%
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
                          title = "Total Population",
                          labFormat = labelFormat(suffix = ""),
                          opacity = .7)
        }else {
            pal <- colorNumeric(palette = "viridis", 
                                domain = total_census$estimate)
            labels <- lapply(
                paste("<strong>Area: </strong>",
                      total_census$NAME,
                      "<br />",
                      "<strong>Total Population: </strong>",
                      formatC(total_census$estimate, format = "f",  big.mark =",",digits = 0)),
                htmltools::HTML
            )
            total_census %>%
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
                          title = "Total Population",
                          labFormat = labelFormat(suffix = ""),
                          opacity = .7)
        }
        
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


        # features <- unique(water_springs$feature)
        # Pillar_pal <- colorFactor(pal = c('deepskyblue3', 'magenta2'),
        #                           levels = features)

        ## interavtive map of springs and streams in Floyd with two points for Town of Floyd and Floyd Quarry
        # floyd_map <- water_springs %>%
            
        ggplot() + 
            geom_sf(mapping = aes(geometry = geometry), data = f) + 
            geom_sf(mapping = aes(geometry = geometry), data = areawater2,  color = "blue") + 
            labs(title = "Streams and Water bodies in Floyd")
        
      
            
        


    })
    
    output$mines <- renderLeaflet({
        
        
        
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
