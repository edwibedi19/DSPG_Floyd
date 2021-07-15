library(shiny)
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
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(shinyjs)


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


# Water Features 
floyd <- left_join(countydata, counties, by = "county_fips") %>% 
  filter(state_name %in% c("Virginia"), county_name %in% c("Floyd County")) 

pts_w <- SpatialPointsDataFrame(flo_w, coords = flo_w[,1:2]) 
floyd_df <- as.data.frame(floyd) 
points_poly <- df_to_SpatialPolygons(floyd_df, key = "group", coords = c("long","lat"), proj = CRS()) 
new_shape_w <- pts_w[points_poly,]
new_shape_df_w <- as.data.frame(new_shape_w)
# Springs
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


mines <- read_tsv("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/ShinyApp/data/AbandonedMineralMineLands.txt")

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

## Land Parcel Data
aoi_boundary_HARV <- st_read(paste0(getwd(), "/data/parcels_with_class/Parcels_with_Class.shp")) 
state_class_colors <- c("blue", "green", "red", "cyan", "magenta", "yellow")
class_levels <- c("Agricultural/Undeveloped (20 – 99 acres)", "Agricultural/Undeveloped (100 acres and up)","Single-Family Residential(Suburban 0-19.99 acres)" ,
                  "Single Family Residential(Urban)", "Commercial/Industrial", "Multi-Family")
class_pal <- colorFactor(pal = state_class_colors, 
                         levels = class_levels)
# agr 
agr <- aoi_boundary_HARV %>% 
  filter(PropClass == "Agricultural/Undeveloped (20 – 99 acres)")%>%
  st_transform(crs = "+init=epsg:4326")
# agr greater 
agr_large <- aoi_boundary_HARV %>% 
  filter(PropClass == "Agricultural/Undeveloped (100 acres and up)")%>%
  st_transform(crs = "+init=epsg:4326")
#single household 
single <- aoi_boundary_HARV %>% 
  filter(PropClass == "Single-Family Residential(Suburban 0-19.99 acres)")%>%
  st_transform(crs = "+init=epsg:4326")
# single urban household 
single_urban <- aoi_boundary_HARV %>% 
  filter(PropClass == "Single Family Residential(Urban)")%>%
  st_transform(crs = "+init=epsg:4326")
# multi
mult <- aoi_boundary_HARV %>% 
  filter(PropClass == "Multi-Family")%>%
  st_transform(crs = "+init=epsg:4326")
# commerical
com <- aoi_boundary_HARV %>% 
  filter(PropClass == "Commercial/Industrial")%>%
  st_transform(crs = "+init=epsg:4326")


# body -----------------------------------------------------------
ui <- navbarPage(title = "DSPG 2021",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
               
      
            ## Tab Overview--------------------------------------------
            tabPanel("Overview", value = "overview",
                     fluidRow(style = "margin: 2px;",
                              align = "center",
                              # br("", style = "padding-top:2px;"),
                              # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                              br(""),
                              h1(strong("Water Management And Industry And Residential Growth In Floyd County, Virginia"),
                                 br(""),
                                 h4("Data Science for the Public Good Program"),
                                 h4("Virginia Tech"),
                                 br()
                              )
                     ),
                     fluidRow(style = "margin: 6px;",
                              column(4,
                                     h2(strong("Project Background")),
                                     p(strong("The problem."), "Rural counties often face challenges in providing health care access to their residents given limited", a(href = "https://www.ruralhealthinfo.org/topics/hospitals", "health facilities", target = "_blank"),
                                       "available, lack of broadband infrastructure that makes it difficult to provide", a(href = "https://www.ruralhealthinfo.org/topics/telehealth", "telemedicine access", target = "_blank"), "or communicate health information, and individual-level",
                                       a(href = "https://www.ruralhealthinfo.org/topics/social-determinants-of-health", "inequalities", target = "_blank"), "that pose barriers to health care use and health
                                            behaviors. Identifying areas of high need or potential solutions may also be difficult for rural areas without adequate resources to acquire, analyze, and interpret
                                            relevant data."),
                                     p(),
                                     p(strong("The setting."), a(href = "https://www.floydcova.org/", "Floyd County", target = "_blank"), "is a rural area in Virginia’s Central Piedmont, bordering North Carolina,
                                            with a declining population of approximately 17,600 people. Like many other rural areas in the United States, Patrick County is having difficulty meeting its residents’ health and quality of life needs.
                                            The county’s", a(href = "https://www.countyhealthrankings.org/app/virginia/2019/rankings/patrick/county/outcomes/overall/snapshot", "doctor to patient ratios", target = "_blank"),
                                       "of 3,530 to 1 for primary care providers, 8,840 to 1 for dentists, and 2,520 to 1 for mental health providers are 3-
                                            to 8-times higher than statewide, and the county’s only hospital closed in 2017. At the same time, the median income for Patrick County residents is $42,900,
                                            46% of children living in the county are eligible for free or reduced-price school lunch, and 12% of residents are food insecure."),
                                     p(),
                                     p(strong("The project."), "This Virginia Tech", a(href = "https://aaec.vt.edu/index.html", "Department of Argicultural and Applied Economics", target = "_blank"),
                                       "Data Science for Public Good (DSPG) project aimed to build local capacity, leverage social and data science to address current and future resident well-being, and enhance
                                             data-driven decision making about rural health in Floyd County, Virginia.")
                              ),
                              column(4,
                                     h2(strong("Our Work")),
                                     p("Our research team worked closely with Floyd County Extension Office, Virginia Department of Health, and Healthy Floyd County coalition stakeholders
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
                                     p(strong("Floyd County extension professionals and the communities they serve."), "Information available through the interface helps extension
                                            agents identify areas where residents may not have access to internet, or areas with a high smartphone ownership share, suggesting what channels agents may
                                            want to use to disseminate health-related information most effectively. Information on older adult populations and grocery store access can help extension agents
                                            better understand where underserved populations live and how to advocate on their behalf."),
                                     p(strong("Local health-related agencies and departments seeking data insights to inform their decision-making."), "For local stakeholders, identifying broadband
                                            access gaps that limit access to telemedicine, grocery store access gaps, and areas with high proportions of older adults with independent living difficulty can suggest
                                            optimal locations for placing free wifi hotspots, providing grocery delivery services, devising mobile health unit routes, or can inform other solutions that would benefit
                                            a broad population base."),
                                     p(strong("State government representatives in the Virginia Department of Health and the State Office of Rural Health."), "These and similar stakeholders may
                                            need small or rural area-specific insights that Centers for Disease Control and other county-level datasets cannot provide.")
                              )
                     ),
                     fluidRow(align = "center",
                              p(tags$small(em('Last updated: August 2021')))
                        
                        
                    ) 
            ), 
            ## Tab Introduction--------------------------------------------
            tabPanel("Sociodemographics", value = "socio",
                    fluidRow(style = "margin: 6px;",
                             h1(strong("Floyd County Residents' Sociodemographic Characteristics"), align = "center"),
                             p("", style = "padding-top:10px;"),
                             column(4, 
                                   h4(strong("Who does Floyd County Serve?")),
                                    p("We examined Floyd County population sociodemographic and socioeconomic characteristics to better understand the residents that the county serves. ") ,
                                    p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census tract levels. A
                                      CS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. 
                                      We used the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity, 
                                      employment, health insurance coverage, and other relevant characteristics."), 
                                     p("Our interactive plots visualize census block-group level sociodemographic characteristics of Floyd County residents.")
                            ) ,
                             column(8,
                                   h4(strong("Map of Resident Socioeconomic Characteristics by Census Tract or Block Group")),
                                    selectInput("char1", "Select Variable:", width = "100%", choices = c(
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
                                   withSpinner(leafletOutput("demo1")) , 
                                   p(tags$small("Data Source: American Community Survey 5-year estimate 2015/2019"))
                                 ) 
                          
                    ) 
            ), 
            ## Tab Data and Methodology--------------------------------------------
            tabPanel("Data and Methodology", value = "data",
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Data and Methodology"), align = "center"),
                              br(),
                              column(4,
                                     img(src = "data-dmme.png", style = "display: inline; float: left;", width = "200px"),
                                     p(strong("Department of Mines, Minerals and Energy."), ""),
                                     br(""),
                                     img(src = "data-nrv.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("New River Valley Regional Commission."), "New River Valley Regional Commission is ")
                              ),
                              column(4,
                                     img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                     p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census
                                            Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and
                                            socioeconomic characteristics including employment, disability, and health insurance coverage. We used ACS 2014/18 5-year
                                            estimates to obtain census tract and census block group-level to explore Floyd County resident characteristics."),
                                     br(""),
                                     img(src = "data-usClimate.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("US Climate Data."), "US Climate Data "),
                                     br(""),
                                     img(src = "data-usgsnhd.jpeg", style = "display: inline; float: left;", width = "200px"),
                                     p(strong("USGS National Hydrography Dataset. "), "USGS National Hydrography is a... We used 2019 CoreLogic data to obtain the locations of all residential
                                           properties in Patrick County.")
                              ),
                              column(4,
                                     img(src = "data-vce.jpeg", style = "display: inline; float: left;", width = "100px"),
                                     p(strong("Virginia Cooperative Extension. "), "Virginia Cooperative Extension is "),
                                     br(""),
                                     img(src = "data-vec.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("Virginia Employment Commission."), "The United State Department of Agriculture Food Access Research Atlas is a data resource
                                          created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows
                                          individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                                          Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics."),
                                     br(""), 
                                     img(src = "", style = "display: inline; float: left;", width = "120px"),
                                     p(strong("Virginia Employment Commission."), "The United State Department of Agriculture Food Access Research Atlas is a data resource
                                          created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows
                                          individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                                          Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics.")
                              )
                     )
            ),
            ## Tab Geology--------------------------------------------
            tabPanel("Geology and Water Features" , value = "geology",
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Water Retention"), align = "center"),
                              p("", style = "padding-top:10px;"), 
                              column(4, 
                                     h4(strong("Floyd County")),
                                     p("Floyd County's climate is characterized by moderately mild winters and warm summers.Precipitation patterns in Floyd County are determined
                                        generally by prevailing westerly winds which have a southerly component during fall and winter. Most moisture
                                        comes from storms spawned over the Atlantic Ocean. The average annual rainfall is 40.79 inches, though this varies
                                        within the County. Using this information and data from the surrounding towns of Christainsburg and Pulaski, we can try to picture 
                                       the groundwater quantity in Floyd and determine how much more residental or commerical development the county can withstand. "),
                                     br(),
                                     p("Floyd County consists of 382 square miles; 143,873 acres of forest land and 100,108 acres of
                                        non-forest land. Floyd County is situated in the Blue Ridge Uplands, a part of the Blue Ridge Physiographic
                                        Province which extends from New York to northwestern Georgia. Elevations in the County generally range from 2,000 to
                                        3,000 feet, significantly higher neighboring counties to the north, south, and east. The physiography of the County is characterized by gently rolling
                                        land. Most of the land is more suited to grazing and forestry than to large-scale cultivation and urban types of development. Nearly half
                                        of the County's total acreage is forested. "), 
                                     br(), 
                                     p("Almost all of Floyd County is underlain by Pre-Cambrian igneous and metamorphic rocks. They are complex, vary in age, and include the granites, gneisses and schists of the
                                      Leatherwood granite and Wissachickon and Lynchburg gneiss formations. "), 
                                     br(), 
                                     p("A number of streams originate in the County. These include major tributaries of the New River (Big Reed Island Creek and Little River) and headwater streams of the Dan, Smith, Pigg,
                                      Backwater and Roanoke Rivers. Most of the drainage ultimately Snowmelt atop Buffalo Mountain goes to the Gulf of Mexico via the New River, Kanawha and Ohio into
                                      the Mississippi River system. "),
                                     ), 
                              column(8, 
                                    tabsetPanel(
                                        tabPanel("Precipitation",
                                                 selectInput("var1", "Select Variable:", width = "100%", choices = c(
                                                     "Rainfall" = "rainfall",
                                                     "Minimum Temeprature" = "min", 
                                                     "Maximum Temeprature" = "max")
                                                 ),
                                                 p(strong("Precipitation")),
                                                 plotlyOutput("precipitation"),
                                                 p(tags$small("Data Source: US Climate"))
                                        ),
                                        tabPanel("Water Features",
                                                 p(strong("Streams")),
                                                 leafletOutput("water"),
                                                 p(tags$small("Data Source: USGS National Hydrography Dataset"))
                                                 
                                    )
                                  ) 
                              )
                        
                     ) 
            ), 
            ## Tab Water Usage--------------------------------------------
            tabPanel("Water Usage", value = "usage",
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Land Parcels"), align = "center"),
                              p("", style = "padding-top:10px;"),
                              column(4, 
                                     h4(strong("Land and Water Usage")),
                                     p("Because Floyd is located on the west most rural part of Virginia, there was little data on the well water level of the public and private wells that
                                       supports the county's water system. Simiarly, there was little recent data on the water quality issue they have been facing for the past 20 years. 
                                      ")
                                     
                                     
                                     ), 
                              column(8, 
                                     
                                  tabsetPanel(
                                      tabPanel("Land Parcels",
                                               withSpinner(leafletOutput("landParcel")), 
                                               p(tags$small("Data Source: "))
                                               
                                      ),
                                      tabPanel("NDWI"
                                               
                                      )
                                  )
                              ) 
                     ) 
                     
                     
            ), 
            ## Tab Economics--------------------------------------------
            tabPanel("Economics", value = "economics",
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Resident and Commerical Development"), align = "center"),
                              column(4, 
                                     h4(strong("Economic Growth")),
                                     p("The residental and commerial businesses ahve been growing within the past 10 years in Floyd, but there is a different demographic of the new movers. 
                                       Of the recent residents, their household income is significantly higher than those residing in Floyd for the past 10 years and their home values have
                                       almost doubled. Because of the recent pandemic, there was a push on moving to rural areas and working from home and this is part of the reason why home values
                                       have increased so much within the past 2 years. It seems that the new residents are moving into Floyd for its land features, natural beauty, 
                                       and reowned for vibrant culture of music, arts, local foods and wines, and outdoor recreation, but they are working outside and not contributing to the county's economy. 
                                       ")
                                     ), 
                              column(8, 
                                     
                                  p("", style = "padding-top:10px;"), 
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
            tabPanel("Water Quality", tabName = "wells",
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Water Quality"), align = "center"),
                              p("", style = "padding-top:10px;"),
                              column(4, 
                                     h4(strong("Potential Sources of Contamination")),
                                     p("Based on our research within the past 8 weeks and a couple of meetings with stakeholders, we can conclude that contaminants of well water can fall into 3 categories: 
                                       (1) Geologically non point source, runoff from farmland, streams, lawns, driveways, (2) Domestic, from house pipelines or faucets, (3) Surface, from well construction and maintenice. "), 
                                   p("Major sources of potential contamination near the home (within 100 feet of the well) were identified as streams (19%), oil tanks (13%) and
                                      septic systems (3%). Larger, more significant potential pollutant sources were also proximate (within one-half mile) to water supplies, according to participants. 59% of
                                      respondents indicated that their water supply was located within one-half mile of a farm animal operation and 31% indicated that their supply
                                      was within one half-mile of a field crop operation. The type of material used for water distribution in each home was also described by participants on the questionnaire. 
                                      The two most common pipe materials were plastic (63%) and copper (25%).") , 
                                   p(" Old mines as well as abandoned wells pose considerable threats for groundwater contamination, with all drinking water coming from groundwater in the County. Essentially
                                      these sites can provide direct routes for any contaminants to reach groundwater unless they are properly closed off."),
                                   p(" If coliform bacteria are present in a water supply, possible pathways or sources include: (1) improper well location or inadequate
                                      construction or maintenance, (2) contamination of the household plumbing system (e.g. contaminated faucet, water heater),and (3) contamination of the groundwater itself. ") 
                                     ),
                              column(8, 
                                     
                                  tabsetPanel(
                                      tabPanel("Wells",
                                             selectInput("var2", "Select Variable:", width = "100%", choices = c(
                                                 "Average Daily Withdrawals (GPD)" = "gpd",
                                                 "Well Depth with Percent of Usage" = "depth", 
                                                 "Maximum Daily Withdrawals" = "max")
                                             ),
                                             plotlyOutput("wells"), 
                                             p(tags$small("Data Source: New River Valley Water Supply Plan 2011"))
                                      ),
                                      tabPanel("Contamination",
                                             p("", style = "padding-top:10px;"), 
                                             selectInput("contam", "Select Variable:", width = "100%", choices = c(
                                               "Percent Common Contaminants" = "percent",
                                               "Groups of Common Contaminants" = "group")
                                             ),
                                             withSpinner(tableOutput("sources")),
                                             p(tags$small("Data Sources: Virginia Cooperative Extension, Virginia Household Water Quality Program 2010, Town of Christainsburg 2018 Drinking Water Report")), 
                                             p(tags$small("All water testing: The Water Quality Laboratory of the Department of Biological Systems Engineering and Soils Testing Laboratory of the Department of Crop and Soil Environmental Sciences at Virginia Tech")), 
                                             br(), 
                                             p(strong("Map of Abandoned Mines")),
                                             withSpinner(leafletOutput("mines")),
                                             p(tags$small("Data Sources: "))
                                               
                                               
                                      )
                                  ) 
                              )
                     ) 
                     
            ),
            ## Tab Conclusion --------------------------------------------
            tabPanel("Conclusion", value = "conclusion", 
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Water Quality"), align = "center"),
                              p("", style = "padding-top:10px;"),
                              column(4, 
                                     h4("Water Quality")), 
                              column(4, 
                                     h4("Water Quantity")),
                              column(4,
                                     h4("Overall Sustainability")) 
                     ) 
                     
                     
            ), 
            ## Tab Team --------------------------------------------
            tabPanel("Team", value = "team",
                     fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                              h1(strong("Team"), align = "center"),
                              br(),
                              h4(strong("VT Data Science for the Public Good")),
                              p("The", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural'), "and", a(href = 'https://ext.vt.edu/','Applied Economics and the Virginia Cooperative Extension Service.'),
                                "In its eighth year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around
                              critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences
                              to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program
                              highlights, how to apply, and our annual symposium, please visit", 
                                a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                              p("", style = "padding-top:10px;")
                     ),
                     fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                              column(6, align = "center",
                                     h4(strong("DSPG Team Members")),
                                     img(src = "", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "team-julie.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     p(a(href = 'https://www.linkedin.com/in/morgan-stockham/', 'Esha Dwibedi', target = '_blank'), "(Virginia Tech, Applied Microeconomics);",
                                       a(href = 'https://www.linkedin.com/in/julie-rebstock', 'Julie Rebstock', target = '_blank'), "(Virgina Tech, Economics and Computational Modeling and Data Analytics);",
                                       a(href = 'https://www.linkedin.com/in/igomez-3099/', 'Ryan Jacobs', target = '_blank'), "(Virginia Tech, Statistical and Data Science).",
                                        a(href = 'https://www.linkedin.com/in/igomez-3099/', 'John Wright', target = '_blank'), "(Virginia State Univeristy, Statistical and Data Science)."),
                                     p("", style = "padding-top:10px;") 
                              ),
                              column(6, align = "center",
                                     h4(strong("VT Faculty Team Members")),
                                     img(src = "", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     p(a(href = "", 'Susna Chen', target = '_blank'), "(Project Lead, Research Assistant Professor);",
                                       a(href = "", 'Brianna Posadas', target = '_blank'), "(VT, Postdoctoral Research Associate);",
                                       a(href = '', 'Sarah M. Witiak', target = '_blank'), "(VSU, Division Director and Distinguished Professor)."),
                                     p("", style = "padding-top:10px;")
                              )
                     ),
                     fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                              h4(strong("Project Stakeholders")),
                              p(a(href = 'https://www.linkedin.com/in/nancy-bell-aa293810/', 'Dawn Barnes', target = '_blank'), "(Virginia Cooperative Extension, Floyd County at Virginia Tech);",
                                a(href = 'https://www.linkedin.com/in/terri-alt-3138b4101/', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                              p("", style = "padding-top:10px;"),
                              h4(strong("Acknowledgments")),
                              p("We would like to thank Healthy Patrick County, an association of concerned Patrick County residents, and Brandon Kramer for their input to this project.")
                     )
            ),
            inverse = T) 
        

     
 



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    char1 <- reactive({
        input$char1
    })
    output$demo1 <- renderLeaflet({
        if(char1() == "home") {
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
            
            
            
        }else if(char1() == "income") {
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
            
            
        }else if (char1() == "unemploy") {
            
            
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
        else if (char1() == "age") {
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
            
            
            
        }else if (char1() =="high") {
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
        }else if (char1() =="bach") {
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
        }else if (char1() =="mast") {
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
        }else if(char1() == "doct") {
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
        }else if (char1() == "commute"){
            
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
        else if(char1() == "food") {
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
        }else if(char1() == "poverty") {
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
        }else if (char1() == "block") {
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
                labs(title = "Average Monthly Rainfall from Floyd, Christiansburg, Pulaski", 
                     caption = "Data Source: US Climate Data",
                     y="Rainfall (in)")+ theme( 
                         plot.subtitle = element_text(size = 9, color = "blue"))
        
            
        }else if (var1() == "min") {
            
            ggplot(climate, aes(fill = County, x = Month, y = Min_Temp)) + 
                geom_bar(position="dodge", stat="identity") + 
                scale_x_discrete(limits = month.abb) +
                labs(title = "Minimum Tempature from Floyd, Christiansburg, Pulaski",
                     caption = "Data Source: US Climate Data",
                     y = "Temperature (F)") + theme( 
                         plot.subtitle = element_text(size = 6, color = "blue"))
            
            
        }else {
            
            ggplot(climate, aes(fill = County, x = Month, y = Max_Temp)) + 
                geom_bar(position="dodge", stat="identity") + 
                scale_x_discrete(limits = month.abb) + 
                labs(title = "Maximum Tempature from Floyd, Christiansburg, Pulaski",
                     caption = "Data Source: US Climate Data",
                     y = "Temperature (F) ")+ theme( 
                         plot.subtitle = element_text(size = 7, color = "blue"))
            
        }
        
        
        
        
    })
    
    # Census tract plot 
    output$water <- renderLeaflet({


        features <- unique(water_springs$feature)
        Pillar_pal <- colorFactor(pal = c('deepskyblue3', 'magenta2'),
                                  levels = features)
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
          addCircleMarkers(lng = ~water_springs$long, lat = ~water_springs$lat,  radius = 1, color = ~Pillar_pal(water_springs$feature)) %>% 
          addMarkers(lng = -80.31891779181245, lat = 36.91313331126569, 
                     label = lapply(
                       paste("<strong>Town of Floyd</strong>",
                             "<br />"),
                       htmltools::HTML)) %>%
          addMarkers(lng = -80.25908794232855, lat = 36.90665582434524, 
                     label = lapply(
                       paste("<strong>Floyd Quarry</strong>",
                             "<br />"),
                       htmltools::HTML)) %>% 
          addLegend(title = "Feature", position = "bottomleft", pal = Pillar_pal, values = features,
                    )
  
        
      
            
        


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
           addMarkers(lng = -80.25908794232855, lat = 36.90665582434524, 
                     label = lapply(
                       paste("<strong>Floyd Quarry</strong>",
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
                labs(title = "Average Daily Withdrawals of Community Wells", 
                     x = "Wells",
                     y = "Withdrawal (GPD) ") + 
                theme(legend.position = "none", 
                      plot.subtitle = element_text(color = "blue", size = 9))
            
            
        }else if (var2() == "depth") {
            wells$Percent <- wells$Withdrawl_GPD/wells$Max_GPD
            
            ggplot(wells, aes(x = Names)) + 
                geom_col(aes(y = Percent), fill = "darkseagreen1")  + 
                geom_line(aes(y = Well_Depth/1000), color = "navyblue", size = 1, group = 1) +
                scale_y_continuous("Percent", sec.axis = sec_axis(~.*1000, name = "Depth (ft)"))  + 
                labs(title = "Percent of Usage and Well Depth of Community Wells", x = "Wells") + 
              theme(plot.subtitle = element_text(color = "blue", size = 7))
            
            
        }else {
            wells %>% 
                ggplot(aes(x = Names, y = Max_GPD)) + 
                geom_bar(stat = "identity", position = "dodge",fill = rgb(0.9,0.4,0.4,0.9))+ 
                labs(title = "Maximum Daily Withdrawals of Community Wells", 
                     x = "Wells",
                     y = "Withdrawal (GPD)") + 
                theme(legend.position = "none")
            
            
        }
        
    })
    
    output$landParcel <- renderLeaflet({
      
      
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
    
    contam <- reactive({
      input$contam
    })
    
    
    output$sources <- renderTable({
      
      if(contam() == "percent") {
            table <- read.csv("data/table-sources.csv")
            table$`X..Exceeding.Standard` <- paste0(table$`X..Exceeding.Standard`, " %")
            colnames(table) <- c("Test", "EPA Standard", "Average", "Maximum Value", "% Exceeding Standard")
            table
      } 
      else {
        
        table <- read.csv("data/table-contaminants.csv")
        table
        
        
      }
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
