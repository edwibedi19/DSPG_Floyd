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
options(tigris_use_cache = TRUE)

census_api_key("6f1a78212175773dd80d1a03bd303e8d181a6096", install = TRUE, overwrite = T)
readRenviron("~/.Renviron")

# data -----------------------------------------------------------
# Demographics 
total_pop <- 15074 
home <- readRDS(paste0(getwd(), "/data/demographics/home.rds")) 
home <- st_transform(home, '+proj=longlat +datum=WGS84')

income <- readRDS(paste0(getwd(), "/data/demographics/income.rds")) 
income <- st_transform(income, '+proj=longlat +datum=WGS84')

age <- readRDS(paste0(getwd(), "/data/demographics/age.rds")) 
age <- st_transform(age, '+proj=longlat +datum=WGS84')

employment_status <- readRDS(paste0(getwd(), "/data/demographics/employment_status.rds")) 
employment_status <- st_transform(employment_status, '+proj=longlat +datum=WGS84')

high <- readRDS(paste0(getwd(), "/data/demographics/high.rds")) 
high <- st_transform(income, '+proj=longlat +datum=WGS84')

bach <- readRDS(paste0(getwd(), "/data/demographics/bach.rds")) 
bach <- st_transform(income, '+proj=longlat +datum=WGS84')

mast <- readRDS(paste0(getwd(), "/data/demographics/mast.rds")) 
mast <- st_transform(mast, '+proj=longlat +datum=WGS84')

doct <- readRDS(paste0(getwd(), "/data/demographics/doct.rds")) 
doct <- st_transform(doct, '+proj=longlat +datum=WGS84')

food <- readRDS(paste0(getwd(), "/data/demographics/food.rds")) 
food <- st_transform(food, '+proj=longlat +datum=WGS84')

poverty <- readRDS(paste0(getwd(), "/data/demographics/poverty.rds")) 
poverty <- st_transform(poverty, '+proj=longlat +datum=WGS84')

total_block <- readRDS(paste0(getwd(), "/data/demographics/total_block.rds")) 
total_block <- st_transform(total_block, '+proj=longlat +datum=WGS84')

total_census <- readRDS(paste0(getwd(), "/data/demographics/total_census.rds")) 
total_census <- st_transform(total_census, '+proj=longlat +datum=WGS84')

new <- readRDS(paste0(getwd(), "/data/demographics/new.rds")) 
new <- st_transform(new, '+proj=longlat +datum=WGS84')


# Water Features 
water_springs <- read.csv(paste0(getwd(), "/data/water/water_springs.csv")) 


mines <- read_tsv(paste0(getwd(), "/data/water/AbandonedMineralMineLands.txt")) 

mines_Floyd <- mines %>%
    filter(County == "Floyd")


# rainfall and temperatures 
climate <- data.frame(read_excel(paste0(getwd(),"/data/water/climate-floyd-county-usClimateData.xlsx"))) 
wells <- data.frame(t(read_excel(paste0(getwd(),"/data/water/NRV-wells-floyd-county-2011.xlsx"), col_types = "numeric")))  

colnames(wells) = c("Number_Wells", "Well_Depth", "Casing_Depth" , "Diameter", "Withdrawl_MGD", "Withdrawl_GPD", "Max_MGD", "Max_GPD", "Permitted")
wells <- wells[-1,]
wells$Names <- c("Christie", "Shortt", "Howard", "Rec.Park", "Comm. Cntr")

## Labor Market (Ryan)
industry <- read_excel(paste0(getwd(),"/data/economics/Floyd Labor Market Statistics.xlsx")) 
industry_df <- as.data.frame(industry)

## Floyd County Employment by Industry Sector
industry_overtime <- read_excel(paste0(getwd(),"/data/economics/industry-overtime.xlsx")) 
industry_overtime$Estimate <- as.numeric(industry_overtime$Estimate)
industry_overtime$Year <- as.character(industry_overtime$Year)
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

capita_income <- read_excel(paste0(getwd(), "/data/economics/capita_income.xlsx"))[,1:3]

retail <- read_excel(paste0(getwd(), "/data/economics/retail-sales.xlsx"))[,1:3]
retail$Year <- as.character(retail$Year)

unempl <- read_excel(paste0(getwd(), "/data/economics/unemployment.xlsx"))[,1:3] 
unempl$Year <- as.character(unempl$Year) 

## Land Parcel Data
agr <- readRDS(paste0(getwd(), "/data/land_parcel/agr.rds")) 
agr <- st_transform(agr, '+proj=longlat +datum=WGS84')

agr_large <- readRDS(paste0(getwd(), "/data/land_parcel/agr_large.rds")) 
agr_large <- st_transform(agr_large, '+proj=longlat +datum=WGS84')

single <- readRDS(paste0(getwd(), "/data/land_parcel/single.rds")) 
single <- st_transform(single, '+proj=longlat +datum=WGS84')

single_urban <- readRDS(paste0(getwd(), "/data/land_parcel/single_urban.rds")) 
single_urban <- st_transform(single_urban, '+proj=longlat +datum=WGS84')

mult <- readRDS(paste0(getwd(), "/data/land_parcel/mult.rds")) 
mult <- st_transform(mult, '+proj=longlat +datum=WGS84')

com <- readRDS(paste0(getwd(), "/data/land_parcel/com.rds")) 
com <- st_transform(com, '+proj=longlat +datum=WGS84')



# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic'); 
           }
           "








# body -----------------------------------------------------------
ui <- navbarPage(title = "DSPG 2021",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 
            ## Tab Overview--------------------------------------------
            tabPanel("Overview", value = "overview",
                     fluidRow(style = "margin: 2px;",
                              align = "center",
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
                                     p(strong("The setting."), a(href = "https://www.floydcova.org/", "Floyd County", target = "_blank"), "is a rural area entirely within the Blue Ridge Major Land Resource Area,
                                            with a increasing population of approximately 15,700 people. Like many other rural areas in the United States, Floyd County is having difficulty meeting its residents’ health and quality of life needs.
                                            The county’s", a(href = "https://www.countyhealthrankings.org/app/virginia/2019/rankings/patrick/county/outcomes/overall/snapshot", "doctor to patient ratios", target = "_blank"),
                                            "of 3,530 to 1 for primary care providers, 8,840 to 1 for dentists, and 2,520 to 1 for mental health providers are 3-
                                            to 8-times higher than statewide, and the county’s only hospital closed in 2017. At the same time, the median income for Floyd County residents is $51,500,
                                            46% of children living in the county are eligible for free or reduced-price school lunch, and 12% of residents are food insecure."),
                                     p(),
                                     p(strong("The project."), "This Virginia Tech", a(href = "https://aaec.vt.edu/index.html", "Department of Argicultural and Applied Economics", target = "_blank"),
                                       "Data Science for Public Good (DSPG) project aimed to build local capacity, leverage social and data science to address current and future resident well-being, and enhance
                                             data-driven decision making about rural health in Floyd County, Virginia.")
                              ),
                              column(4,
                                     h2(strong("Our Work")),
                                     p("Our research team worked closely with Floyd County Extension Office, Virginia Department of Health, and Floyd County stakeholders
                                            to identify the county’s priority challenges in the area of water quality and quantity. The research team reviewed a prior", 
                                       a(href = "http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.306.9815&rep=rep1&type=pdf","water quality assessment", target = "blank"), "and", 
                                       a(href = "http://nrvrc.org/wp-content/uploads/2015/08/NRVWSP0911_Final-Water-Supply-Plan.pdf", "water supply plan", target = "_blank"),
                                       "and held a listening meeting with stakeholders to identify these challenges. Lack of data on private and public wells, water quality and quantity issues, out-commuter rates, and residental and commerial future development hault
                                       emerged as key problems where providing actionable insights could address barriers to Floyd County residents’ health and lifestyles."),
                                     p(),
                                     p("We implemented the", a(href = "https://doi.org/10.1162/99608f92.2d83f7f5", "data science framework", target = "_blank"), "and identified, acquired, profiled, and used
                                      publicly available data to provide Floyd County with data-driven resources in each of the four priority areas. We:"),
                                     br(), 
                                     tags$li("Provided census tract and block group-level maps of Floyd County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                     tags$li("Created barplots of", strong("monthly temperatures and precipitation levels"), "to compare with surrounding towns to better determine the groundwater retention of the area. "),
                                     tags$li("Mapped locations of", strong("streams, lakes, and mines"), "at census block group level to highlight the surface water sources and the potential contaminations sources in the county. "),
                                     tags$li("Calculated and mapped", strong("water usage"), "of households to  "),
                                     tags$li("Constructed", strong("land parcel"), "maps by census tract "),
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
            tabPanel("Sociodemographics",
                    fluidRow(style = "margin: 6px;",
                             h1(strong("Floyd County Residents' Sociodemographic Characteristics"), align = "center"),
                             p("", style = "padding-top:10px;"),
                             column(4, 
                                   h4(strong("Who does Floyd County Serve?")),
                                   p("We examined Floyd County’s population of 15,704 residents to better understand the diverse sociodemographic and socioeconomic characteristics that the county serves."),
                                    p("Utlizing American Community Survey (ACS) data, we calculated this information at census block group and census tract levels. The ACS is an ongoing yearly
                                      survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets to help local officials, community leaders, and businesses 
                                      understand the changes taking place in their communities. We used the most recently available 5-year estimates from 2014/18 to compute percent Floyd County residents 
                                      in each block group or tract level by age, race, ethnicity, employment, health insurance coverage, and other relevant characteristics."), 
                                     p("Our interactive plots visualize census block-group level sociodemographic characteristics of Floyd County residents. 
                                       This format allows for easy digestion and comparison of factors to help us best understand who the residents of Floyd County really are on a statistical level.")
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
            tabPanel("Data", 
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Data and Methodology"), align = "center"),
                              p("", style = "padding-top:10px;"),
                              column(4,
                                     img(src = "data-dmme.png", style = "display: inline; float: left;", width = "200px"),
                                     p(strong("Department of Mines, Minerals and Energy."), "The Department of Mines, Minerals and Energy provides public access to information and data related to energy resources, 
                                       mining operations, geologic features, and abandoned mines. We used DMME data for abandoned and active mines in Floyd to show where potential contamination sources were compared
                                       to streams, lakes and other water bodies. This would give us a better idea of where and how the contaminants are coming from. "),
                                     br(""),
                                     img(src = "data-nrv.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("New River Valley Regional Commission."), "The New River Valley Regional Commission is an organization comprised of 13 local governments and three higher education institutions for the purpose 
                                       of encouraging collaboration to address regionally significant issues and opportunities. Since Floyd is in the New Rivery Valley area, we used their water supply report from 2011 to get the most recent data
                                       on their community wells' daily withdrawals, depths and usages. "),
                                     br(""),
                                     img(src = "data-epa.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("Enviromental Protection Agency."), "The Enviornmental Protection is an independent executive agency of the United States federal government tasked with environmental protection matters. 
                                     The standards of different minerals and contaminants were helpful in determining the quality of drinking water in prior studies. We used different reports and plans in order to make recommendations on Floyd's water quality and quantity issue. "),
                                     br(""),
                                     img(src = "data-ngwa.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("The National Groundwater Association."), "The National Groundwater Association is a  community of groundwater professionals working together to advance groundwater knowledge and the success of our members through education and outreach; 
                                     advocacy; cooperation and information exchange; and enhancement of professional practice. It provides statistics, reports and guidance on water sustainability and quality assurance. 
                                       We researched articles and reports from NGWA in order to better make recommendations for Floyd' water quality and quantity issue. ")
                              ),
                              column(4,
                                     img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                     p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census Bureau. ACS samples households to compile 1-year and 5-year datasets 
                                     providing information on population sociodemographic and ocioeconomic characteristics including employment, disability, and health insurance coverage. We used ACS 2014/18 5-year
                                     estimates to obtain census tract and census block group-level to explore Floyd County resident characteristics."),
                                     br(""),
                                     img(src = "data-usClimate.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("US Climate Data."), "The US Climate Data's purpose is to inform people across the U.S of the climate around them. U.S Climate reports historical temperatures, precipitation, and wind speeds daily, monthly or annually of counties
                                     and towns in the United States. We use U.S Climate to gather data on Floyd, Christiansburg, and Pulaski to compare the 3 adjacent areas in terms of available groundwater. "),
                                     br(""),
                                     img(src = "data-usgsnhd.jpeg", style = "display: inline; float: left;", width = "200px"),
                                     p(strong("USGS National Hydrography Dataset. "), "USGS National Hydrography is represents the water drainage network of the United States with features such as rivers, streams, canals, lakes, ponds, coastline, dams, and streamgages. 
                                     It provides shapefiles with latitude and longitude points. We used 2019 NHD data to plot points that represent the streams, lakes, springs in Floyd County by block group. "),
                                     br(""),
                                     img(src = "data-vdh.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("Virginia Department of Health."), "Virginia Department of Health has a mission to protect the health and promote the well-being of all people in Virginia. VDH provides information and guidance in areas such as water quality
                                       which we used to better inform ourselves on this subject, specifically regarding lead in drinking water. We used those reports and articles to recommend ways to better one's water quality of well water. ")
                              ),
                              column(4,
                                     img(src = "data-vce.jpeg", style = "display: inline; float: left;", width = "100px"),
                                     p(strong("Virginia Cooperative Extension. "), "Virginia Cooperative Extension provides resources and educational outreach to the Commonwealth of Virginia’s more than seven million residents in the areas of agriculture and natural resources, 
                                       family and consumer sciences, community viability, and 4-H youth development. During out literature review, we used the VCE 2010 water quality report done in Floyd County to infer the common contaminants in the area and make
                                       recommendations on how to combat those contaminants since they are still experiencing similar issues today. "),
                                     br(""),
                                     img(src = "data-vec.png", style = "display: inline; float: left;", width = "150px"),
                                     p(strong("Virginia Employment Commission."), "Virginia Employment Commission is a division of the Virginia state government that provides benefits and services to unemployed citizens. We used a report from 
                                       2019-2020 to create graphs on the county's population and business growth, industry by type and more in order to address future development plans. "),
                                     br(""), 
                                     img(src = "data-vdeq.jpeg", style = "display: inline; float: left;", width = "120px"),
                                     p(strong("Virginia Department of Enviromental Quality."), "The United State Department of Agriculture Food Access Research Atlas is a data resource
                                    created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows
                                    individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                                    Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics.")
                              ),
                              
                     )
            ),
            ## Tab Geology--------------------------------------------
            # need a different name
            navbarMenu("Water Soures" , 
                     tabPanel("Groundwater", 
                            fluidRow(style = "margin: 6px;",
                                h1(strong("Groundwater"), align = "center"),
                                p("", style = "padding-top:10px;"), 
                                column(4, 
                                       h4(strong("Climate, Rainfall and Temperature")),
                                       p("Globally, groundwater resources dwarf surface water supplies. But because groundwater is hidden, the resource is often forgotten or misunderstood. 
                                         Groundwater is, in fact, vital to public health, the environment, and the economy. Supplying drinking water to over 50% of the total U.S. population and 99% 
                                         of the rural population, including Floyd County, as well as being a major supply for the irrigation of crops, the protection and regular surveillance of the 
                                         quantity and quality of our groundwater is a key aspect we focused on when assessing Floyd County. Scientists estimate the amount of ground water is 400 times 
                                         greater than all the fresh water in lakes, reservoirs, streams, and rivers. Groundwater feeds streams and rivers, especially during periods of drought or low flow. 
                                         Additionally, groundwater is an important component in many industrial processes."),
                                       
                                       p("Floyd County's climate is characterized by mild to cold winters and hot, humid summers. 
                                         Precipitation patterns in Floyd County are determined generally by prevailing westerly winds which have a southerly component during 
                                         fall and winter. Most moisture comes from storms spawned over the Atlantic Ocean. Using this information and data from the surrounding towns of 
                                         Christiansburg and Pulaski, we can try to picture the groundwater quantity in Floyd and determine how much more residential or commercial development 
                                         the county can withstand." ),
                                       ) ,
                                     column(8, 
                                            h4(strong("Graph of Monthly Climate")),
                                           selectInput("var1", "Select Variable:", width = "100%", choices = c(
                                             "Rainfall" = "rainfall",
                                             "Minimum Temeprature" = "min", 
                                             "Maximum Temeprature" = "max")
                                           ),
                                           plotlyOutput("precipitation"),
                                           p(tags$small("Data Source: US Climate"))))), 
                     tabPanel("Surface Water", 
                              style = "margin: 6px;",
                              h1(strong("Surface Water"), align = "center"),
                              p("", style = "padding-top:10px;"), 
                              column(4, 
                                     h4(strong("Streams, Lakes, and Water Bodies")), 
                                     p("Floyd County consists of 382 square miles: 143,873 acres of forest land, 100,108 acres of non-forest land, and 576 acres of surface water. 
                                       Floyd County is situated on a plateau in the Blue Ridge Uplands, a part of the Blue Ridge Physiographic Province which extends from New York to northwestern
                                       Georgia. Elevations in the County generally range from 2,000 to 3,000 feet, significantly higher neighboring counties to the north, south, and east.
                                       The physiography of the County is characterized by gently rolling land. Most of the land is more suited to grazing and forestry than to 
                                       large-scale cultivation and urban types of development. Nearly half of the County's total acreage is forested."), 
                                       p("Several streams originate in the County. These include major tributaries of the New River (Big Reed Island Creek and Little River) 
                                         and headwater streams of the Dan, Smith, Pigg, Backwater and Roanoke Rivers. Most of the drainage, primarily snowmelt atop Buffalo 
                                         Mountain, goes to the Gulf of Mexico via the New River, Kanawha and Ohio into the Mississippi River system.")),
                              column(8, 
                                     h4(strong("Map of Water Features by Block Group")),
                                     leafletOutput("water"),
                                     p(tags$small("Data Source: USGS National Hydrography Dataset"))) 
                                  ) 
                      
                        
                      
            ), 
            ## Tab Water Usage--------------------------------------------
            tabPanel("Water Usage", value = "usage",
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Water Usage and Quantity" ), align = "center"),
                              p("", style = "padding-top:10px;"),
                              column(4, 
                                     h4(strong("Land and Water Usage")),
                                     ## maybe some valueBoxes to highlight how many people use groundwater as their main source??? 
                                     p("Floyd’s location on the western most rural part of Virginia is a major factor as to why there was little data on the well water level of the public
                                       and private wells that supports the county's water system. Similarly, there is little recent data on the water quality issues they have been facing for 
                                       the past 20 years which is why we were looking at two different metrics to use to estimate the water quantity."),
                                     p("First, Floyd County PSA provided us with data on land usage throughout the county seperated by type. Based on our research and short trip out to Floyd, the majority of the county is farmland and agriculture 
                                       with the Town of Floyd in the southeast area. The town itself is very tiny and flat but the area is surrounded by the Blue Ridge Mountain Range. ")
                                     ), 
                              column(8, 
                                  tabsetPanel(
                                      tabPanel("Land Parcels",
                                               h4(strong("Map of Land Parcels by Type")),
                                               withSpinner(leafletOutput("landParcel")), 
                                               p(tags$small("Data Source: Floyd County"))
                                               
                                      ),
                                      tabPanel("NDWI",
                                               p()
                                               
                                      )
                              ) 
                     ) 
                     
                     
            )), 
            
            ## Tab Wells and Water Quality --------------------------------------------
            navbarMenu("Water Quality", 
                      tabPanel("Wells",
                           fluidRow(style = "margin: 6px;",
                                    h1(strong("Well Information"), align = "center"),
                                    p("", style = "padding-top:10px;"),
                                    column(4, 
                                           h4(strong("Withdrawals and Depths")),
                                           p("About 8 of every 10 Virginians use ground water from public water supplies, private wells, or springs for at least part of their daily water supply. 
                                             Dependable ground water supplies for private wells are available at depths of less than 300 feet in most areas of the state. A well yield of at 
                                             least 6 gallons per minute is usually needed for home use, though 10 gallons per minute is more desirable. Low-yield wells (less than
                                             4 gallons per minute) require a properly sized storage tank and pumping system to supply an adequate amount of water for domestic use. If you use 
                                             a low yield well, a storage tank four to five times larger than your total consumption (approximately 75 gallons a day per person) is recommended by experts."),
                                           p("From the New River Valley Water Supply Plan in 2011, there are 5 community wells that we have data on. Each of these wells have a different depth and a different maximum daily withdrawal. 
                                             We attempted to make an inference on the correlation between the well depth and the water yield.  ")), 
                                    column(8, 
                                           h4(strong("Graph of Well Information")),
                                         selectInput("var2", "Select Variable:", width = "100%", choices = c(
                                           "Average Daily Withdrawals (GPD)" = "gpd",
                                           "Well Depth with Percent of Usage" = "depth", 
                                           "Maximum Daily Withdrawals" = "max")
                                         ), 
                                         plotlyOutput("wells"), 
                                         p(tags$small("Data Source: New River Valley Water Supply Plan 2011"))) 
                             
                     ) ) ,
                     tabPanel("Contamination",
                              fluidRow(style = "margin: 6px;",
                                       h1(strong("Contamination"), align = "center"),
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
                                              these sites can provide direct routes for any contaminants to reach groundwater unless they are properly closed off.")
                                       ),
                                      
                                       column(8, 
                                              h4(strong("Table of Common Contaminants")),
                                             selectInput("contam", "Select Variable:", width = "100%", choices = c(
                                               "Percent Common Contaminants" = "percent",
                                               "Groups of Common Contaminants" = "group")
                                             ),
                                             withSpinner(tableOutput("sources")),
                                             p(tags$small("Data Sources: Virginia Cooperative Extension, Virginia Household Water Quality Program 2010; Town of Christainsburg 2018 Drinking Water Report")), 
                                             p(tags$small("All water testing: The Water Quality Laboratory of the Department of Biological Systems Engineering and Soils Testing Laboratory of the Department of Crop and Soil Environmental Sciences at Virginia Tech")), 
                                             tags$ br(), 
                                             tags$br(), 
                                             h4(strong("Map of Abandoned Mines by Block Group")),
                                             withSpinner(leafletOutput("mines")),
                                             p(tags$small("Data Source: The Department of Mines, Minerals and Energy")))  
                              )
                     ) 
                     
            ),
            
            ## Tab Economics--------------------------------------------
            tabPanel("Economics", value = "economics",
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Residental and Commerical Development"), align = "center"),
                              p("", style = "padding-top:10px;"),
                              column(4, 
                                     h4(strong("Economic Growth")),
                                     p("The residential and commercial businesses have seen growth in the past 10 years in Floyd, 
                                       however there is a different demographic of the new movers. The recent residents, share a household income that is significantly
                                       higher than those residing in Floyd for the past 10 years and their home values have almost doubled. Due to the recent pandemic, 
                                       there was a push on moving to rural areas and working from home resulting in home values increasing in the past two years. Many new 
                                       residents are moving into Floyd for its land features, natural beauty, and vibrant culture of music, arts, local foods and wines, 
                                       and outdoor recreation. However, these same residents work outside the county and contribute less to the county's economy.")
                              ), 
                              column(8, 
                                     h4(strong("Graph of Economic Parameters")),
                                     selectInput("econ1", "Select Variable:", width = "100%", choices = c(
                                       "Employment by Industry" = "industry",
                                       "Projected Population Change" = "pop",
                                       "Income per Capita" = "capita", 
                                       "Population by Age" = "age", 
                                       "Number of Commuters" = "commute", 
                                       "New Business Growth" = "business",
                                       "Retail Sales by Type" = "retail",
                                       "Unemployment Rate Timeseries" = "unemplo")
                                     ),
                                     plotlyOutput("trend1", height = "600px")
                                     
                              )
                     )
                     
                     
            ), 
            
            ## Tab Conclusion --------------------------------------------
            tabPanel("Recommendations", value = "conclusion", 
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Recommendations"), align = "center"),
                              p("", style = "padding-top:10px;"),
                              tabsetPanel(
                                tabPanel("Water Quality",
                                         column(6, 
                                                p("", style = "padding-top:10px;"),
                                                 p("Based on research from Environmental Protection Agency, Virginia Department of Health, and the Virginia Cooperative Exension 2010 report, 
                                                 we have several recommendations that could prevent bacteria and minerals from getting into the drinking water in Floyd county. "),
                                                
                                                 p("The most important action for having clean and safe water is getting your well water tests by the county. The county provides a kit that includes a vial, instructions on how to get the sample and where to mail it. 
                                                 After getting the water sample, place the vial into the styrofoam and into the envelope, then place it in the mail. 
                                                 This kit is so simple that even your ",strong("kids")," can get involved and learn more about the water quality that is flowing through the county. 
                                                 Testing your well water every year or two years is one of the best ways to ensure your water is safe and clean for you and your family. "), 
                                                 br(),
                                                h4(strong("Table of Common Recommendations for Water Quality")), 
                                                withSpinner(tableOutput("qualityRec"))
                                               
                                         ) , 
                                         column(6, 
                                                p("", style = "padding-top:10px;"),
                                                p("The most common contaminants found in drinking water surrounding the New Rivery Valley Area 
                                                 included total coliform bacteria, low levels of pH, sodium and chloride, lead and copper and manganese. "),
                                                br(), 
                                                tags$li("Disinfect", strong(" total coliform bacteria, "), "the Virginia Department of Health recommends the use of Shock Chlorination to clean and sanitize
                                                  the well and entire plumbing system."), 
                                                tags$li("Combatting", strong("manganese: "), " it is recomended to get a distillation or filtration system because it is secondary maximum contaminant level, maximum 0.05 mg/L"),
                                                tags$li("Raising ", strong("low pH levels (<7):"), " is installing an acid neutralizing filter that passes through calcite that raises the pH."),
                                                tags$li("Protecting water from ", strong("lead and copper: "), "leeching into the water, regularly clean your faucet’s screen (also known as an aerator). Sediment, debris, and lead particles can collect in your aerator. 
                                                     Make sure to run your water for at least a minute and use only cold water. If not hot water, use cold and boil on stove. "),
                                                tags$li("For eliminating ", strong("chloride and sodium: "), " activated carbon filters are the most common devices used to dechlorinate
                                                water, remove objectionable chlorine tastes, and reduce corrosion of plumbing systems"),
                                                tags$li("Dealing with", strong("hard water: "), " use water softeners which exchange the minerals (iron, magnesium, and calcium) for sodium. To avoid potential risks, one could
                                                     only soften the the hot water supply to take showers and baths and clean around the house and leave the cold water available for consumption. "),
                                                tags$li("To condition well water for ", strong("safer and cleaner water: "), " many use water softening, iron removal, neutralization of acid water, reverse
                                                osmosis, turbidity control, removal of objectionable tastes and odors, and aeration"),
                                                tags$li("The issue with ", strong("water softeners ,"), " when used in conjuction with filters causes the water to smell rotten. Also, the addition of sodium into the water
                                                     can be a health risk and should talk to their physican. "),
                                                br(), 
                                                p("Protecting water from", strong("agricultural runoff"), "like sediments, animal feeding operations, livestock grazing, irrigation and pesticides can include measures like",  
                                                  strong("applying management practices that control the volume and flow rate"),  "of runoff water, keep the soil in place, and reduce soil transport;", 
                                                  strong("adjusting grazing intensity, keeping livestock out of sensitive areas,"), " providing alternative sources of water and shade, and promoting revegetation of ranges, pastures, and riparian zones; ", strong("applying only the amount of water required for crops,"), " 
                                                  converting irrigation systems to higher efficiency equipment; and following ", strong("Integrated Pest Management Technology"), "to use natural barriers and limit pesticide uses. "), 
                                                br()) 
                                                
                                              
                                         
                                ), 
                                tabPanel("Well Testing Kit",
                                         fluidRow(style = "margin: 6px;",
                                                  p("", style = "padding-top:10px;"),
                                         column(4, 
                                                p("1. Here are pictures of what the kit looks like. All of the kits are ", strong("handed out FREE by the county. ")), 
                                                img(src = "front-page.jpeg", style="display: block; margin-left: auto; margin-right: auto;", width = "300px")
                                         ), 
                                         column(4,
                                                p("2. Once you open the kit, you'll see a clear vial where you want to put the water sample in
                                                  and an envelope. "), 
                                                img(src = "front-page-2.jpeg", style="display: block; margin-left: auto; margin-right: auto;", width = "300px")
                                               ), 
                                         
                                         column(4, 
                                                p("3. Third, you will see a styrofoam square with a cutoff where you will put your vial filled with water. The styrofoam is there to ensure your sample gets
                                                  to the lab safely."),
                                                img(src = "styrofoam.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "300px")
                                          ) 
                                      ),
                                      fluidRow(style = "margin: 6px;",
                                               p("", style = "padding-top:10px;"),
                                               column(4, 
                                                      p("4. Here are the instructions to follow when getting your water for the vial. This process only takes a couple of minutes and again, it is so simple that even your kids
                                                    could follow along and get invovled. "), 
                                                      img(src = "instructions.jpeg", style="display: block; margin-left: auto; margin-right: auto;", width = "300px")
                                               ), 
                                               column(4,
                                                      p("5. In order to get the results back, you will need to fill out this survey that includes why you're getting your water tested, address, name, etc. No information
                                                  is shared publicly about your water sources. "), 
                                                      img(src = "survey.jpeg", style="display: block; margin-left: auto; margin-right: auto;", width = "300px"),
                                               ), 
                                               
                                               column(4, 
                                                      p("6. Here is the front of the envelope and provided address you will send it to once it goes into the mail. ", strong("Notice there is no need for a stamp. "), "All you need to do 
                                                  is seal the envelope and drop it in your mail box or nearest post office. "), 
                                                      img(src = "address.jpeg", style = "display: inline; border: 1px solid #C0C0C0;", width = "300px")
                                               ) 
                                               
                                               
                                      )) ,
                                tabPanel("Water Quantity",
                                         p()
                                         
                                )
                              ) 
                     ) 
                     
                     
            ), 
            ## Tab Team --------------------------------------------
            tabPanel("Team", 
                     fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                              align = "center",
                              br(""),
                              h1(strong("Team")),
                              br(""), 
                              h4(strong("VT Data Science for the Public Good")),
                              p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
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
                                     img(src = "team-julie.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "team-ryan.JPG", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     p(a(href = 'https://www.linkedin.com/in/esha-dwibedi-83a63476/', 'Esha Dwibedi', target = '_blank'), "(Virginia Tech, Graduate in Economics);",
                                       a(href = 'https://www.linkedin.com/in/julie-rebstock', 'Julie Rebstock', target = '_blank'), "(Virgina Tech, Undergraduate in Economics and Computational Modeling and Data Analytics);",
                                       a(href = 'https://www.linkedin.com/in/ryan-jacobs-bb5727174/', 'Ryan Jacobs', target = '_blank'), "(Virginia Tech, Undergraduate in Environmental Economics, Management, and Policy, and Minoring in Industrial Design).",
                                        a(href = 'https://www.linkedin.com/in/john-wright-9a13621a0/', 'John Wright', target = '_blank'), "(Virginia State Univeristy, Undergraduate in Statistical and Data Science)."),
                                     p("", style = "padding-top:10px;") 
                              ),
                              column(6, align = "center",
                                     h4(strong("VT Faculty Team Members")),
                                     img(src = "", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "team-posadas.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     p(a(href = "https://www.linkedin.com/in/susanchenja/", 'Susna Chen', target = '_blank'), "(Project Lead, Research Assistant Professor);",
                                       a(href = "https://www.linkedin.com/in/briannaposadas/", 'Brianna Posadas', target = '_blank'), "(VT Faculty, );",
                                       a(href = '', 'Sarah M. Witiak', target = '_blank'), "(VSU Faculty, Division Director and Distinguished Professor)."),
                                     p("", style = "padding-top:10px;")
                              )
                     ),
                     fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                              h4(strong("Project Stakeholders")),
                              p(a(href = '', 'Dawn Barnes', target = '_blank'), "(Virginia Cooperative Extension, Floyd County at Virginia Tech);",
                                a(href = '', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                              p("", style = "padding-top:10px;"),
                              h4(strong("Acknowledgments"))
                     )
            ) ,
            inverse = T) 
        

     
 



# server --------------------------------------------
server <- function(input, output) {
  runjs(jscode)
  
  
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
                labs(title = "Average Monthly Rainfall", 
                     y="Rainfall (in)")
        
            
        }else if (var1() == "min") {
            
            ggplot(climate, aes(fill = County, x = Month, y = Min_Temp)) + 
                geom_bar(position="dodge", stat="identity") + 
                scale_x_discrete(limits = month.abb) +
                labs(title = "Minimum Monthly Tempature",
                     y = "Temperature (F)") 
            
            
        }else {
            
            ggplot(climate, aes(fill = County, x = Month, y = Max_Temp)) + 
                geom_bar(position="dodge", stat="identity") + 
                scale_x_discrete(limits = month.abb) + 
                labs(title = "Maximum Monthly Tempature",
                     y = "Temperature (F) ")
            
        }
        
        
        
        
    })
    
    # Census tract plot 
    output$water <- renderLeaflet({
      
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = "green"
      )
      
      icons1 <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = "red"
      )


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
          addAwesomeMarkers(lng = -80.31891779181245, lat = 36.91313331126569, 
                     label = lapply(
                       paste("<strong>Town of Floyd</strong>",
                             "<br />"),
                       htmltools::HTML),
                     icon = icons1) %>%
          addAwesomeMarkers(lng = -80.25908794232855, lat = 36.90665582434524, 
                            label = lapply(
                              paste("<strong>Floyd Quarry</strong>",
                                    "<br />"),
                              htmltools::HTML),
                            icon=icons) %>% 
          addLegend(title = "Feature", position = "bottomleft", pal = Pillar_pal, values = features,
                    )
  
        
      
            
        


    })
    
    output$mines <- renderLeaflet({
      
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = "green"
      )
        
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
        addAwesomeMarkers(lng = -80.25908794232855, lat = 36.90665582434524, 
                     label = lapply(
                       paste("<strong>Floyd Quarry</strong>",
                             "<br />"),
                       htmltools::HTML),
                     icon=icons) %>% 
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
                theme(legend.position = "none")
            
            
        }else if (var2() == "depth") {
            wells$Percent <- wells$Withdrawl_GPD/wells$Max_GPD
            
            ggplot(wells, aes(x = Names)) + 
                geom_col(aes(y = Percent), fill = "darkseagreen1")  + 
                geom_line(aes(y = Well_Depth/1000), color = "navyblue", size = 1, group = 1) +
                scale_y_continuous("Percent", sec.axis = sec_axis(~.*1000, name = "Depth (ft)"))  + 
                labs(title = "Percent of Usage and Well Depth of Community Wells", x = "Wells")
            
            
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
      
      state_class_colors <- c("#9dfef8","#00cc00","#2621ff","#ffa500","#ffff19","#ff0000" )
      
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
                    group = "Multi-Family") %>%
        addLayersControl(
          position = "bottomright",
          overlayGroups = c("Agricultural/Undeveloped (20 – 99 acres)",
                            "Agricultural/Undeveloped (100 acres and up)",
                            "Single-Family Residential(Suburban 0-19.99 acres)",
                            "Single Family Residential(Urban)",
                            "Multi-Family",
                            "Commercial/Industrial"), 
          options = layersControlOptions(collapsed = FALSE)) 
      
      
    })
    
    
    econ1 <- reactive({
        input$econ1
    })
    
    output$trend1 <- renderPlotly({
        
        if(econ1() == "industry") {
            
          ggplot(industry_overtime, aes(fill = Year, x = Label, y = Estimate)) + 
            geom_bar(position="dodge", stat="identity") + 
            labs(title = "Industry Sector ", 
                 caption = "Data Source: ACS 5-year Estimates" ,
                 y="Persons ", x= "Sector")+
            theme(axis.text.x = element_text(angle = 50, vjust = 2, color = "black",size = 7))
            
            
        }else if (econ1() == "commute") {
            ggplot(data = commute_df,mapping = aes(Type,Quantity))+geom_bar(stat = "identity",fill="powderblue")+
                labs(title = "Floyd County Commuting",
                     caption = "Data Source: U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination 
                        Employment Statistics, 2014") 
            
        }else if (econ1() == "pop"){ 
            
            ggplot(popch_df, aes(x=Year,y=Population,group=1)) +
                geom_line()+geom_point()+
                labs(title = "Projected Floyd Population Change",
                     caption = "Data Source: U.S. Census Bureau, Weldon Cooper Center for Public Service")
            
            
            
        }else if (econ1() == "age") {
            
            ggplot(data = popage_df,mapping = aes(Age,Quantity))+geom_bar(stat = "identity",fill="dodgerblue2")+
                labs(title = "Floyd County Population by Age",
                     caption = "Data Source: 2010 Census") + coord_flip()
            
            
            
        }else if (econ1() == "retail"){
          ggplot(retail, aes(fill = Year, x = Retail, y = Sales/100000)) + 
            geom_bar(position="dodge", stat="identity") + 
            labs(title = "Retail Sales by Group", 
                 caption = "Data Source: Virginia Department of Taxation and Weldon Cooper Center" ,
                 y="Sales (100,000) ", x= "Retail Group")+
            theme(axis.text.x = element_text(angle = 50, vjust = 3, color = "black",size = 7))
          
        }else if (econ1() == "capita"){
          
          ggplot(capita_income, aes(fill = Area, x = Year, y = Amount)) + 
            geom_bar(position="dodge", stat="identity") + 
            labs(title = "Income per Capita", 
                 caption = "Data Source: U.S Census Bureau",
                 y="Dollar ($) ")
          
        }else if (econ1() == "unemplo"){
          
          ggplot(unempl, aes(group = Area, x = Year, y = Rate*100, color = Area)) + 
            geom_line(linetype = "dotted", size = 2) + 
            labs(title = "Unemployment Rate",
                 y="Rate %", x= "Year" )
          
        }else {
            
            ggplot(busgrowth_df, aes(x=Time,y=Quantity,group=1)) +
                geom_line()+geom_point()+
                labs(title = "New Business Growth in Floyd",
                     caption = "Data Source: Virginia Employment Commission, Economic Information & Analytics, 
                        Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")+
            theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"))
            
        }
        
        
    }) 
    
    contam <- reactive({
      input$contam
    })
    
    
    output$sources <- renderTable({
      
      if(contam() == "percent") {
            table <- read.csv("data/water/table-sources.csv")
            table$`X..Exceeding.Standard` <- paste0(table$`X..Exceeding.Standard`, " %")
            colnames(table) <- c("Test", "EPA Standard", "Average", "Maximum Value", "% Exceeding Standard")
            table
      } 
      else {
        
        table <- read.csv("data/water/table-contaminants.csv")
        table
        
        
      }
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
    
    
    output$qualityRec <- renderTable({
        
        table <- read_excel("data/water/recommendations.xlsx")
        table
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "l", colnames = T, digits = 2)
  
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
