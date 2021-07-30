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
library(viridis)
options(tigris_use_cache = TRUE)

census_api_key("6f1a78212175773dd80d1a03bd303e8d181a6096", install = TRUE, overwrite = T)
readRenviron("~/.Renviron")

# data -----------------------------------------------------------
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

## Land Parcel Data 2020
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

#2017
agr17 <- readRDS(paste0(getwd(), "/data/land_parcel/agr17.rds")) 
agr_large17 <- readRDS(paste0(getwd(), "/data/land_parcel/agr_large17.rds")) 
single17 <- readRDS(paste0(getwd(), "/data/land_parcel/single17.rds")) 
single_urban17 <- readRDS(paste0(getwd(), "/data/land_parcel/single_urban17.rds"))
mult17 <- readRDS(paste0(getwd(), "/data/land_parcel/mult17.rds"))
com17 <- readRDS(paste0(getwd(), "/data/land_parcel/com17.rds")) 

# 2013
agr_large13 <- readRDS(paste0(getwd(), "/data/land_parcel/agr_large13.rds"))
single13 <- readRDS(paste0(getwd(), "/data/land_parcel/single13.rds"))
single_urban13 <- readRDS(paste0(getwd(), "/data/land_parcel/single_urban13.rds"))
mult13 <- readRDS(paste0(getwd(), "/data/land_parcel/mult13.rds"))
com13 <- readRDS(paste0(getwd(), "/data/land_parcel/com13.rds"))


percent_change <- read.csv(paste0(getwd(), "/data/land_parcel/percent_change.csv")) 


## GRACE graphs 
grace <- read.csv(paste0(getwd(), "/data/land_parcel/GRACE_Floyd.csv") ,
                  header = TRUE,
                  na.strings = 999.99)



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
                                     p(strong("The problem."), "Water resource management is an important aspect of sustainable growth for any region. Counties like Floyd that are heavily
                                     dependent on groundwater and rely on well water and natural springs for their water needs often face challenges in managing their water 
                                     resources due to the decentralized nature of the water sources. Potable water sourced from groundwater sources are also often vulnerable to 
                                     contamination stemming from the geological composition, surface contaminants as well as household plumbing contaminants. Identifying strategies to 
                                     manage the water quality and quantity issues faced by the county would be crucial for potential industrial and residential growth in the county. 
                                     Identifying areas of high need or potential solutions may also be difficult for rural areas without adequate resources to acquire, analyze, and interpret
                                     relevant data especially when most of the county's water supply comes from private water wells. "),
                                     p(),
                                     p(strong("The setting."), a(href = "https://www.floydcova.org/", "Floyd County", target = "_blank"), "is a primarily rural area entirely within the Blue Ridge 
                                       Major Land Resource Area, with a population of approximately 15,700 people. All of Floyd County’s water originates in the county with no water source flowing 
                                       into the county. Most households in the county are served through private wells and septic systems, though in an around the tiny town of Floyd there are 
                                       small public water and sewer systems. Floyd County is in the Blue Ridge geologic region and has a fractured rock geology with groundwater existing 
                                       in cracks and fissures. This makes it difficult to estimate the required well depth across the county. "),
                                     p(),
                                     p(strong("The project."), "This Virginia Tech", a(href = "https://aaec.vt.edu/index.html", "Department of Argicultural and Applied Economics", target = "_blank"),
                                       "Data Science for Public Good (DSPG) project aims to leverage data science to provide an estimation of the water resources and the quality 
                                       issues they face in the county in order to design better management strategies for industrial and residential growth.")
                              ),
                              column(4,
                                     h2(strong("Our Work")),
                                     p("Our research team worked closely with Floyd County Virginia Cooperative Extension (VCE) office to identify the county’s priority challenges in the area of water quality and quantity. 
                                       The research team reviewed prior reports on", 
                                       a(href = "http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.306.9815&rep=rep1&type=pdf","water quality assessment", target = "blank"), "and", 
                                       a(href = "http://nrvrc.org/wp-content/uploads/2015/08/NRVWSP0911_Final-Water-Supply-Plan.pdf", "water supply plan", target = "_blank"),
                                       "and held multiple meetings with the stakeholders to identify these challenges. Lack of data on private and public wells, 
                                       water quality and quantity issues, out-commuter rates, and designing water management plans for residential and commercial
                                       development had emerged as key problems where providing actionable insights could address barriers to Floyd County water resource management issues. "),
                                     p(),
                                     p("We implemented the", a(href = "https://doi.org/10.1162/99608f92.2d83f7f5", "data science framework", target = "_blank"), "and identified, acquired, profiled, and used
                                      publicly available data to provide Floyd County with data-driven resources in each of the four priority areas. We:"),
                                     tags$li("Provided census tract and block group-level maps of Floyd County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                     tags$li("Created barplots of", strong("monthly temperatures and precipitation levels"), "and compared data from Floyd with surrounding counties to better determine the groundwater retention of the area."),
                                     tags$li("Mapped locations of", strong("streams, lakes, and mines"), "at census block group level to highlight the surface water sources and the potential contaminations sources in the county.  "),
                                     tags$li("Calculated and mapped", strong("water usage"), "of households to  "),
                                     tags$li("Constructed", strong("land parcel"), "maps by census tract in order to determine the county's water capacity for future development. "),
                                     tags$li("Provided ",strong("estimates of groundwater level and water usage")," in the county through land usage and remote sensing data "), 
                                     p(),
                                     p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                              ),
                              column(4,
                                     h2(strong("Dashboard Aims")),
                                     p("Our dashboard is aimed at:"),
                                     p(strong("Floyd County extension professionals and the communities they serve. "), " Information available through the interface helps extension agents form an idea 
                                    on the estimated groundwater resources availability and seasonal trends for the county. Information on land usage further provides an estimate of water 
                                    requirements of the county’s residents. These information alongside the recommendations provide guidelines for better management of water resources for 
                                    potential residential and industrial growth within the county. ."),
                                     p(strong("Researchers working on hydrology in data scarce areas."), "Our interface provides 
                                       insights into potential avenues to explore for indirect estimation of water resources in areas which suffer from a lack of sufficient data. 
                                       This would help researchers identify ways to model available data to estimate and map out the water resources in such data scare areas."),
                                     p(strong("State government representatives in the Virginia Department of Health."), "These and similar 
                                       stakeholders may need small or rural area-specific potable water quality insights that would help inform the policies for regulations on water quality management across the state.")
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
                                    p("Utilizing American Community Survey (ACS) data, we calculated this information at census block group and census tract levels. The ACS is an ongoing yearly
                                      survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets to help local officials, community leaders, and businesses 
                                      understand the changes taking place in their communities. We used the most recently available 5-year estimates from 2014/18 to compute percent Floyd County residents 
                                      in each block group or tract level by age, race, ethnicity, employment, health insurance coverage, and other relevant characteristics."), 
                                     p("Our interactive plots visualize census block-group level sociodemographic characteristics of Floyd County residents. 
                                       This format allows for easy digestion and comparison of factors to help us best understand who the residents of Floyd County really are on a statistical level.")
                            ) ,
                             column(8,
                                   h4(strong("Resident Socioeconomic Characteristics by Census Tract or Block Group")),
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
                                   p(tags$small("Data Source: American Community Survey 5-year estimate 2015/2019")),
                                 ) 
                          
                    ) 
            ), 
            
            ## Tab Economics--------------------------------------------
            tabPanel("Economy", value = "economy",
                     fluidRow(style = "margin: 6px;",
                              h1(strong("Residental and Commerical Development"), align = "center"),
                              p("", style = "padding-top:10px;"),
                              column(4, 
                                     h4(strong("Economic Growth")),
                                     p("As residential and commercial businesses have grown in the past ten years in Floyd, there continues to be a different demographic of the new movers
                                       into the county. The new residents share a household income that is significantly higher than those traditionally residing in Floyd 
                                       for the past ten years, and their home values have almost doubled. Due to the recent pandemic, there was a push to move to rural areas and work
                                       from home, resulting in home values increasing in the past two years. Many new residents are moving into Floyd for its land features, natural 
                                       beauty, and vibrant culture of music, arts, local foods and wines, and outdoor recreation. However, these same residents work outside the county
                                       and contribute less to the county's economy. This trend is evident when observing commuting data for Floyd County from the Virginia Employment 
                                       Commission [6]. Floyd has roughly 60% of employees that live in Floyd, but commute out of the county for their job, only 15%, in contrast,
                                       that commute into the county for work, leaving the remaining 25% of people who both work and live in the county [5]. "),
                                     p("Note that the only apparent sectors that show an increase include manufacturing and health services whereas the other sectors have stayed 
                                       consistent or have declined within the past 5 years. Depending on these sectors’ water usage, the county may or may not be able to withstand further 
                                       increases in their future development plans [1].  Floyd County’s population is projected  to continue to grow at a steady rate necessitating the need 
                                       for job creation in the county to support the population change. The stakeholders emphasized a high average age which potentially can limit the
                                       type and number of new jobs that are viable to thrive within the county. Therefore, an increase in population as a whole would counter act the county’s 
                                       aging population, opening up the sectors based on their skill base [4]. In terms of income per capita which is an important indicator for the economic 
                                       health of an area, Floyd County is consistently below average compared to the other localities indicating low wages for residents even compared to 
                                       similar counties and though the most recent data for new business growth shows constant new businesses, there is no positive trend which indicators the 
                                       county is growing in terms of GDP [3][6]. The retail sales by type graph shows this inconclusiveness further due to no sector having a strong positive slope 
                                       over the last 8 years [7]. "),
                                     p("As the United States aims to move toward a greener more sustainable future, the job market will evolve accordingly, whether by choice or by government mandate. 
                                       Focusing on new green industries and infrastructure has the potential to attract more residents to work within the county and bring new businesses, residents, and tourists to operate, work, live, and enjoy leisure time in Floyd County. ")
                              ), 
                              column(8, 
                                     h4(strong("Graph of Economic Parameters")),
                                     selectInput("econ1", "Select Variable:", width = "100%", choices = c(
                                       "Employment by Industry [1]" = "industry",
                                       "Projected Population Change [2]" = "pop",
                                       "Income per Capita [3]" = "capita", 
                                       "Population by Age [4]" = "age", 
                                       "Number of Commuters [5]" = "commute", 
                                       "New Business Growth [6]" = "business",
                                       "Retail Sales by Type [7]" = "retail",
                                       "Unemployment Rate Timeseries [8]" = "unemplo")
                                     ),
                                     plotlyOutput("trend1", height = "600px")
                                     
                              ),
                              column(12, 
                              
                              h4("References") , 
                              p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                              p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                              p(tags$small("[3] U.S Census Bureau")), 
                              p(tags$small("[4]  2010 Census")), 
                              p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                              p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                              p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                              p(tags$small("[8]  Virginia Employment Commission")) ) 
                              
                     )
                     
                     
            ), 
            ## Tab Data and Methodology--------------------------------------------
            tabPanel("Data and Methodology", 
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
                                    Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics."),
                                     br(""), 
                                     img(src = "data-google-earth.png", style = "display: inline; float: left;", width = "120px"),
                                     p(strong("Google Earth Engine."), "Google Earth Engine combines a multi-petabyte catalog of satellite imagery and geospatial datasets with planetary-scale analysis 
                                     capabilities and makes it available for scientists, researchers, and developers to detect changes, map trends, and quantify differences on the Earth's surface. We 
                                       used it to collect data on NDWI, precipitation and elevation based on the geographical location of the well sites for the  well water estimation model using NDWI.")
                              ),
                              
                     )
            ),
            ## Tab Geology--------------------------------------------
            # need a different name
            navbarMenu("Water Sources" , 
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
                                         Additionally, groundwater is an important component in many industrial processes. [1]"),
                                       
                                       p("Floyd County's climate is characterized by mild to cold winters and hot, humid summers. 
                                         Precipitation patterns in Floyd County are determined generally by prevailing westerly winds which have a southerly component during 
                                         fall and winter. Most moisture comes from storms spawned over the Atlantic Ocean. Using this information and data from the surrounding towns of 
                                         Christiansburg and Pulaski, we can try to picture the groundwater quantity in Floyd and determine how much more residential or commercial development 
                                         the county can withstand. Compared to Christainsburg and Pulaski, Floyd seems to be the average of the two concerning max temperature and min temperature whereas 
                                         rainfall average per month is somewhat scattered. It seems as though in the warmer months like June to August Floyd is not getting as much participation as they need to in order to sustain their water usage throughout the year. 
                                         Though we can not say for certain, it is likely the groundwater retention will not keep up with the agriculture, commerial and residental needs in the future if the county is planning on developing. " ),
                                       ) ,
                                     column(8, 
                                            h4(strong("Monthly Climate")),
                                           selectInput("var1", "Select Variable:", width = "100%", choices = c(
                                             "Rainfall" = "rainfall",
                                             "Minimum Temperature" = "min", 
                                             "Maximum Temperature" = "max")
                                           ),
                                           plotlyOutput("precipitation"),
                                           p(tags$small("Data Source: US Climate"))),
                                tags$br(), 
                                h4("References: "), 
                                p(tags$small("[1] Groundwater: Groundwater sustainability. (2021). Retrieved July 27, 2021, from https://www.ngwa.org/what-is-groundwater/groundwater-issues/groundwater-sustainability")) 
                                )), 
                     tabPanel("Surface Water", 
                              style = "margin: 6px;",
                              h1(strong("Surface Water"), align = "center"),
                              p("", style = "padding-top:10px;"), 
                              column(4, 
                                     h4(strong("Streams, Lakes, and Water Bodies")), 
                                     p("Floyd County consists of 382 square miles: 143,873 acres of forest land, 100,108 acres of non-forest land, and 576 acres of surface water. 
                                       Floyd County is situated on a plateau in the Blue Ridge Uplands, a part of the Blue Ridge Physiographic Province which extends from New York to northwestern
                                       Georgia. Elevations in the County generally range from 2,000 to 3,000 feet, significantly higher than its neighboring counties to the north, south, and east which is why water tends
                                       to flow out of the county. The physiography of the County is characterized by gently rolling land. Most of the land is more suited to grazing and forestry than to 
                                       large-scale cultivation and urban types of development. Nearly half of the County's total acreage is forested, leaving little area for residental development [1]."), 
                                       p("Several streams originate in the County. These include major tributaries of the New River (Big Reed Island Creek and Little River) 
                                         and headwater streams of the Dan, Smith, Pigg, Backwater and Roanoke Rivers. Most of the drainage, primarily snowmelt atop Buffalo 
                                         Mountain, goes to the Gulf of Mexico via the New River, Kanawha and Ohio into the Mississippi River system. Though the majority of the well's supply comes from groundwater, surface water can be 
                                         a source of contamination if they are located close enough to the source. As mapped to the right, there are many lakes, streams and water bodies that spread across the county. 
                                         Shown in water usage tab (land parcel), the majority of the county uses its land for agricultural purposes which is likely flowing into the enarby surface water. This is where the contamination comes from. 
                                         Mapping the locations of the water bodies in the county is a start to figuring out exactly where and how the wells are getting contaminated. Our plan was to webscrap the locations
                                         of the wells and combine those with this map in order to visualize the possible contamination sources. ")),
                              column(8, 
                                     h4(strong("Water Features by Block Group")),
                                     leafletOutput("water"),
                                     p(tags$small("Data Source: USGS National Hydrography Dataset"))) ,
                              tags$br(), 
                              h4("References"),
                              p(tags$small("[1] Martin, L., Turman, K., &amp; Hodge, T. (2009). What are our Natural Resources? (pp. 13-27, Publication No. 3). VA: Floyd County.")) 
                              
                        ) 
                      
                        
                      
            ), 
            ## Tab Water Usage--------------------------------------------
            navbarMenu("Water Usage" , 
                       tabPanel("Land Parcel", 
                                fluidRow(style = "margin: 6px;",
                                         h1(strong("Land Parcel in Floyd County"), align = "center"),
                                         p("", style = "padding-top:10px;"), 
                                         column(12, 
                                                h4(strong("Overview", align = "center" )),
                                                p("Land is primarily used for agricultural and residential purposes in Floyd. The Town 
                                                  of Floyd and its immediate surroundings does constitute a hub of commercial properties with some other 
                                                  commercial and industrial properties dispersed across the county. This is visible in the land parcel 
                                                  mapping from various years, where with the county’s landscape is dominated by parcels designated “Agriculture 
                                                  over 99 acres” and “Agriculture 20-99 acres”. Alongside, “suburban residential” plots are scattered across 
                                                  the county leading to the prominence of individual water supply from private wells and natural springs in the 
                                                  county. The changes in land usage over years can be tracked on the table displayed alongside. "),
                                                h4(strong("Linkage with Water Usage ", align = "center")),
                                                p("Land use models have been widely used to examine future land-use change and water resource assessments 
                                                  in global (Lotze-Campen et al., 2008; García-Ruiz et al., 2011), national (Calder et al., 2003), and regional analyses
                                                  (Baker et al., 2013; Wilson et al., 2015; Wilson et al., 2016). Understanding potential land-use related water demand in a 
                                                  region serves as a first step in evaluating prospective outcomes and relevant mitigation strategies to address potential vulnerabilities. "),
                                                p("Private wells are not federally regulated or protected by the U.S. Environmental Protection Agency's (EPA) Safe Drinking 
                                                  Water Act, despite the fact that an estimated 43 million people (15% of the population) in the United States rely 
                                                  on private wells for drinking water (DeSimone et al., 2009). This leads to an acute lack of data on the prevalence 
                                                  and extent of use of private wells and thus the burden on the groundwater level for counties like Floyd which are 
                                                  predominantly dependent on private wells. We try to form an estimate of the water usage by land parcel category
                                                  in order to provide some insights on the water resource requirements of the county’s residents. ")) , 
                                       column(4, 
                                                h4(strong("Future Directions")),
                                                p("One potential model that can be used to study the water demand for the region is the LUCAS state-and-transition 
                                                  simulation model (STSM), which is a stochastic, Markov chain, empirical simulation model used to predict how defined 
                                                  variables transition between different specified states over a specified timeframe (Daniel et al., 2011). STSMs have been 
                                                  widely used to simulate changes in land use and land cover (LULC) over time for assessing LULC scenario impacts on future water 
                                                  resources (Wilson et al., 2016; Wilson et al., 2017). The STSM divides the landscape up into spatially discrete simulation cells, 
                                                  each with assigned state classes (i.e., LULC) and transition types. Each state class has pre-defined transition type pathways, 
                                                  allowing or preventing cells to move between different state classes over time. This model would provide an estimate on the potential 
                                                  water demand for the area based on the vegetation cover, water resources map and land use patterns and zoning information. 
                                                  This model can also be used to simulate water demand based on potential changes in land usage. "),
                                                h4(strong("Limitations")),
                                                p("Lack of actual data on private wells is the major obstacle for any kind of estimation of water quantity requirements for the 
                                                  county. The indirect estimation of water usage through categorization of land parcels while providing some insights into 
                                                  water usage, however, cannot be validated unless we have some actual on-ground data on the water usage and number of private 
                                                  wells as well as periodic data on the water level depth of these wells. Alongside this, due to the lack of zoning laws in 
                                                  the county, there is also a lack of regulation and thus data on the how the land parcels are used by the residents. This 
                                                  further leads to a lack of information on the burden placed on the water resources in individual parcels by the residents, 
                                                  which might lead to biased estimation of water usage.   ")
                                         ), 
                                         column(8, 
                                                tabsetPanel(
                                                  tabPanel("Maps",
                                                           h4(strong("2020")), 
                                                           withSpinner(leafletOutput("landParcel2020")), 
                                                           h4(strong("2017")), 
                                                           withSpinner(leafletOutput("landParcel2017")), 
                                                           h4(strong("2013")), 
                                                           withSpinner(leafletOutput("landParcel2013")), 
                                                           p(tags$small("Data Source: Floyd County VCE")) ),
                                                  tabPanel("Tables/Graphs",
                                                           
                                                           withSpinner(tableOutput("parcelTable")) ,
                                                           p(tags$small("Note: 2013 Data did not show any parcels for Agricultural/Undeveloped (20-99 acres) which is why 
                                                                        the % change 13-17 and $ change 13-20 are so high. Though we do not have this data, 
                                                                        we can assume the data is missing and the land parcel count is similar to that of 2017 and 2020 with 1-2% change. ")) , 
                                                           h4(strong("Parcel Count by Category")),
                                                           plotlyOutput("percentChange")
                                                           
                                                           )
                                                )
                                                
                                         ),
                                         column(12, 
                                               h4("References: "),
                                               p(tags$small("Lotze-Campen, H.; Müller, C.; Bondeau, A.; Rost, S.; Popp, A.; Lucht, W. Global food demand, productivity growth, and the scarcity of land and water resources: A spatially explicit mathematical programming approach. Agric. Econ. 2008, 39, 325–338. ")),
                                               p(tags$small("García-Ruiz, J.M.; López-Moreno, J.I.; Vicente-Serrano, S.M.; Lasanta–Martínez, T.; Beguería, S. Mediterranean water resources in a global change scenario. Earth Sci. Rev. 2011, 105, 121–139. ")),
                                               p(tags$small("Calder, I.R.; Reid, I.; Nisbet, T.R.; Green, J.C. Impact of lowland forests in England on water resources: Application of the Hydrological Land Use Change (HYLUC) model. Water Resour. Res. 2003, 39. ")),
                                               p(tags$small("Baker, T.J.; Miller, S.N. Using the Soil and Water Assessment Tool (SWAT) to assess land use impact on water resources in an East African watershed. J. Hydrol. 2013, 486, 100–111. ")),
                                               p(tags$small("Wilson, T.S.; Sleeter, B.M.; Sherba, J.; Cameron, D. Land-use impacts on water resources and protected areas: Applications of state-and-transition simulation modeling of future scenarios. AIMS Environ. Sci. 2015, 2, 282–301. ")),
                                               p(tags$small("Wilson, T.S.; Sleeter, B.M.; Cameron, D.R. Future land-use related water demand in California. Environ. Res. Lett. 2016, 11, 054018. ")),
                                               p(tags$small("Wilson, T.S.; Sleeter, B.M.; Cameron, D.R. Mediterranean California’s water use future under multiple scenarios of developed and agricultural land use change. PLoS ONE 2017, 12, e0187181. ")),
                                               p(tags$small("Daniel, C.J.; Frid, L. Predicting Landscape Vegetation Dynamics Using State-and-Transition Simulation Models. In Proceedings of the First Landscape State-and-Transition Simulaiton Modeling Conference, Portland, OR, USA, 14–16 June 2011; General Technical Report PNW-GTR-869. U.S. Department of Agriculture, Forest Service: Portland, OR, USA, 2011; pp. 5–22. ")),
                                               p(tags$small("DeSimone LA, Hamilton PA, and Gilliom RJ (2009). The quality of our nation’s waters—Quality of water from domestic wells in principal aquifers of the United States, 1991–2004—Overview of major findings. U.S. Geological Survey Circular, 1332(48). 10.3133/cir1360. ")),
                                               
                                               ))), 
            
            
                       tabPanel("Remote Sensing Data", 
                                fluidRow(style = "margin: 6px;",
                                         h1(strong("Remote Sensing Data"), align = "center"),
                                         p("", style = "padding-top:10px;"),
                                         tabsetPanel(
                                           tabPanel("GRACE-CSR",
                                                    p("", style = "padding-top:10px;"),
                                                    h3(strong("What is GRACE-CSR?", align = "center")),
                                                    p("The GRACE twin satellites, launched 17 March 2002, perform detailed measurements of Earth's gravity field changes and have
                                                        revolutionized investigations about Earth's water reservoirs over land, ice and oceans, as well as earthquakes and crustal deformations.
                                                        NASA’s GRACE mission provides the first opportunity to directly measure groundwater changes from space. By observing changes in the Earth’s 
                                                        gravity field, scientists can estimate changes in the amount of water stored in a region, which cause changes in gravity. This makes a 
                                                        huge difference for scientists and water managers who want to understand trends in how our resources are being consumed over the long term. 
                                                        GRACE has returned data on some of the world’s biggest aquifers and how their water storage is changing 
                                                        [Rodell and Famiglietti, 2001; Yeh et al., 2006; Rodell et al., 2007]. Using estimates of changes in snow and surface soil moisture, 
                                                        scientists can calculate an exact change in groundwater in volume over a given time period. "),
                                                    p("GRACE is a collaboration of the US and German space agencies (NASA and DLR). GRACE ground segment operations are currently 
                                                        co-funded by the GFZ German Research Centre for Geosciences and the European Space Agency (ESA). NASA, ESA, GFZ and DLR support 
                                                        the continuation of the measurements of mass redistribution in the Earth system. The key partners in the design, construction and
                                                        launch of the mission have been the Jet Propulsion Laboratory, the University of Texas Center for Space Research, GFZ German Research 
                                                        Centre for Geosciences, as well as Astrium GmBH, Space Systems Loral (SS/L), Onera and Eurockot GmBH. "), 
                                                   
                                                    h3(strong("Data", align = "center")),
                                                    p("GRACE TELLUS provides user-friendly level-3 data grids of monthly surface mass changes, with most geophysical corrections applied, 
                                                           to analyze changes in the mass of the Earth's hydrologic, cryospheric, and oceanographic components. This is done using GRACE Level-2 data,
                                                           with additional post-processing, alone or in combination with other ancillary data, to generate gridded, geo-located products (monthly 
                                                           and time-averaged) with the most up-to-date corrections. GRACE gravity data in spherical harmonic coefficients (Level-2 data), for both 
                                                           the time-averaged and time-variable fields, are available from either JPL's PO.DAAC or GFZ's ISDC. ") , 
                                                    p("Data available here are changes in equivalent water thickness relative to a time-mean baseline. The basic method is explained in Wahr 
                                                           et al. (1998). The land and ocean grids are processed with different filters that are tuned to best filter out noise while preserving 
                                                           real geophysical signals. The data we use come from the GRACE Ground System at CSR (University Texas / Center for Space Research) 
                                                           which generate Level-2 data (spherical harmonic fields). Their output include spherical harmonic coefficients of the gravity field and of the 
                                                           dealiasing fields used to compute them. "),
                                                    img(src = "GRACE_image.gif", style="display: block; margin-left: auto; margin-right: auto;" , width = "300px"),
                                                    p(tags$small("Image Source: GRACE Tellus, Jet Propulsion Laboratory, NASA")), 
                                                    tags$br(), 
                                                    tags$br(), 
                                                    column(4, 
                                                         h3(strong("What is 'Equivalent Water Thickness'? ")),
                                                         p("The observed monthly changes in gravity are caused by monthly changes in mass. Most of the monthly gravity changes are caused by changes 
                                                           in water storage in hydrologic reservoirs, by moving ocean, atmospheric and land ice masses, and by mass exchanges between these Earth 
                                                           system compartments. Their vertical extent is measured in centimeters of equivalent water thickness, much smaller than the radius of the Earth 
                                                           or the horizontal scales of the changes, which are measured in kilometers.  "),
                                                         p("The mass of the atmosphere is removed during processing using ECMWF atmospheric pressure fields, so the GRACE Tellus surface mass grids do not 
                                                           contain atmospheric mass variability over land or continental ice areas like Greenland and Antarctica (except, of course, for errors in ECMWF). "),
                                                         tags$br(), 
                                                         h3(strong("Limitations")),
                                                         tags$li(strong("Abnormal earth mass changes: "), " Some changes in gravity are caused by mass redistribution in the 'solid' Earth, such as those following a large
                                                           earthquake, or those due to glacial isostatic adjustment; in those cases, the interpretation of the gravity changes in terms of 'equivalent 
                                                           water thickness' are not correct, even though it is still possible to compute this quantity (i.e., by removing the solid Earth effects to isolate 
                                                           the water-related mass changes).  "),
                                                         tags$li(strong("Spatial Smoothing: "), " The GRACE satellites fly at over 400 km altitude. The gravity field weakens with altitude, and short wavelengths 
                                                           attenuate more than longer ones. As a consequence, it is necessary to smooth short wavelengths to recover the set of masses on the Earth surface 
                                                           that cause the gravity field seen by GRACE at its altitude. To reduce this source of noise, a spatial averaging smoother (a Gaussian here) is applied here."),
                                                         tags$li(strong("Months with Lower Accuracy: "), " The monthly grids have higher errors when the orbit is near exact repeat. Such months include July to December 
                                                                 2004, and Jan & Feb 2015. Another source of larger error stems from a lack of data in some particular months. "),
                                                         tags$li(strong("Data Gaps since 2011: "), " SActive 'battery management' started in 2011 due to the aging batteries on the GRACE satellites. 
                                                                 During certain orbit periods over several consecutive weeks, no ranging data are collected and hence no gravity fields 
                                                                 can be computed. These gaps occur approximately every 5-6 months, and last for 4-5 weeks (also see 'GRACE months' for a complete list of data months & outages).")
                                                         
                                               ), 
                                               column(8, 
                                                      h4(strong("GRACE Measures")),
                                                      selectInput("graceM", "Select Graph:", width = "100%", choices = c(
                                                        "Total Monthly Equivalent Liquid Water Thickness" = "month",
                                                        "Groundwater Table Level Anomolies, Monthly" = "ground",
                                                        "Groundwater Table Level Anomolies, Yearly" = "groundY"
                                                      )
                                                      ), 
                                                      plotlyOutput("grace", height = "650px", width = "100%"), 
                                                      p(tags$small("Data Source: "))
                                               ),
                                               column(12, 
                                                      h4("References: "),
                                                      p(tags$small("[1] Rodell, M., and J. S. Famiglietti (2001), Terrestrial water storage variations over Illinois: Analysis of observations and implications for Gravity Recovery and Climate Experiment (GRACE), Water Resour. Res., 37(5), 1327–1340. ")) ,
                                                      p(tags$small("[2] Yeh, P. J.-F., Swenson, S. C., Famiglietti, J. S., and Rodell, M. (2006), Remote sensing of groundwater storage changes in Illinois using the Gravity Recovery and Climate Experiment (GRACE), Water Resour. Res., 42, W12203, doi:10.1029/2006WR005374. ")) ,
                                                      p(tags$small("[3] Rodell, M., Chen, J., Kato, H. et al. Estimating groundwater storage changes in the Mississippi River basin (USA) using GRACE. Hydrogeol J 15, 159–166 (2007). https://doi.org/10.1007/s10040-006-0103-7 "))) 
                                         ),
                                         tabPanel("NDWI",
                                                  p("", style = "padding-top:10px;"),
                                                  h3(strong("What is NDWI?", align = "center")),
                                                  p("Measurements of natural phenomena are often too complex to be accurately described solely with remote sensing. Calculated indices bridge the 
                                                    gap between satellite imagery and internal vegetative processes. The Normalized Difference Water Index (NDWI) is strongly correlated to the amount of
                                                    water held in plant foliage. The different wavelengths of light captured by the Landsat 8 satellite can be used to synthesize the NDWI values for any given 
                                                    geographic region of interest.  "), 
                                                  p("The Normalized Difference Water Index (NDWI) is highly correlated with the amount of water stored in the foliage of plants (Gao, 1996).
                                                  The USGS also provides a formula to calculate the NDWI by combining the Short-Wave Infrared with the Near Infrared wavelengths of light captured 
                                                  from the Landsat 8 satellite in the following formula:") , 
                                                p("NDWI = (Band 5 – Band 6) / (Band 5 + Band 6) ", style="display: block; margin-left: auto; margin-right: auto;"), 
                                                p("This formula allows for per-pixel calculation of this index to describe the distribution of water in vegetation. The image below, shows the distribution of water in vegetation throughout the new river valley."), 
                                                img(src = "NDWI-image.png", style="display: block; margin-left: auto; margin-right: auto;" , width = "300px"),
                                                p(tags$small("Image Source: Analyzing Vegetative Health using Landsat 8 Satellite Imagery Project, DSPG 2021 ")), 
                                                
                                                h3(strong("Data", align = "center")),
                                                p("We tested a model to predict the well water level within the county utilizing NDWI and other readily available data on elevation and precipitation
                                                  for the county. The NDWI values were hypothesized to be indicative of changes in groundwater levels across seasons over years. Elevation changes 
                                                  significantly impact NDWI values and were hence included in the model. Precipitation is the major source of groundwater replenishment within most
                                                  areas in Virginia and was hence also include in the model.  ") , 
                                                p("The estimation of the water table level was performed through a Long Short-Term Memory (LSTM) model, which is a Recurrent Neural Network (RNM) 
                                                  architecture used in machine learning. The model used data on well water levels (measured in feet below land surface), taken from ten well sites
                                                  documented under USGS for counties surrounding Floyd. Based on the location (latitude and longitude) of these well sites, corresponding data on 
                                                  NDWI values, elevation of the sites as well as precipitation values from the year 2012 to 2021 was added from Google Earth Engine.  The resulting 
                                                  panel dataset was analyzed using a LSTM model, to get temporal predictions of well water level from the training data for the various well sites. 
                                                  This model was also used to spatially and temporally predict the well water level at Floyd given the county’s location, and the elevation, NDWI 
                                                  and precipitation values for the county.  "),
                                                tags$br(), 
                                                tags$br(), 
                                                  column(6, 
                                                         p("", style = "padding-top:10px;"),
                                                         h4(strong("Model Prediction")),
                                                         p(""), 
                                                         h4(strong("Limitations")),
                                                         p("One major limitation of the model was the lack of training data used in the LSTM model. Given the dearth of data in counties surrounding Floyd, 
                                                           the data used for training the model came from only ten well sites. Even within these well sites, the well water level data is sporadic across months. 
                                                           This is often coupled with the lack of corresponding NDWI data for the specific date ranges due to the limitations of satellite data collection, 
                                                           which often suffers due to any kind of atmospheric disturbances. This lack of sufficient data required for training the model might result in 
                                                           significant underfitting of the model which would result in biased predictions.  "),
                                                         p("Our model also does not include other variables relevant variables which might significantly impact water table level predictions. 
                                                           These factors include geological variables like soil type, permeability of the soil, as well as topology changes within the county, 
                                                           which all determine the extent to which groundwater can be replenished. Other relevant variables would include vegetation type and the extent of homogeneity 
                                                           of the vegetation, which would also impact the NDWI variations within a region. These factors can be further explored in future research on well 
                                                           water predictions using NDWI. ")
                                                  ), 
                                                  column(8, 
                                                         h4(strong("Model"))
                                                  ),
                                                column(12, 
                                                       h4("References: "),
                                                p(tags$small("Gao, B.-C. (1996). NDWI—A normalized difference water index for remote sensing of vegetation liquid water from space. Remote Sensing of Environment, 58(3), 257-266. ")) ) 
                                         )
                                         
                                 ) 
                              )
                       )
                      
                     
                     
            ), 
            
            ## Tab Wells and Water Quality --------------------------------------------
            navbarMenu("Water Quality", 
                      tabPanel("Wells",
                           fluidRow(style = "margin: 6px;",
                                    h1(strong("Well Information"), align = "center"),
                                    p("", style = "padding-top:10px;"),
                                    column(12, 
                                           h4(strong("Overview", align = "center")),
                                           p("About 8 of every 10 Virginians use ground water from public water supplies, private wells, or springs for at least part of their daily water supply. 
                                             Dependable ground water supplies for private wells are available at depths of less than 300 feet in most areas of the state. A well yield of at 
                                             least 6 gallons per minute is usually needed for home use, though 10 gallons per minute is more desirable. Low-yield wells (less than
                                             4 gallons per minute) require a properly sized storage tank and pumping system to supply an adequate amount of water for domestic use. If you use 
                                             a low yield well, a storage tank four to five times larger than your total consumption (approximately 75 gallons a day per person) is recommended by experts. [1] 
                                             There are major differences between using public, private well systems or natural springs. First, public wells have to be regulated according to EPA and built to standards, but private wells are
                                             not regularly tested and the local government has no right to information regarding it so therefore, water coming from a private well does not have the EPA to backup its content [2].   Also,
                                             for a well to be private, it can only serve up to 25 persons and for at least 60 days out of the year [2]. Using this information, we can predict that because public wells have to serve much more persons
                                             in an area (25+), their yield amount of water may not be enough to sustain the county compared to a private well serving a family or two. This is why finding ways to prevent contamination 
                                             or condition a public well system is even more important than a private well because 1 public well serves more persons than 1 private well. In terms of natural springs as a source of water, 
                                             many residents in Floyd get their water from natural springs off the side of a mountain. When our team visited Floyd County, they witnessed multiple residents pulling off the side of the road and walking down to a natural springs to gather their water. 
                                             This seems to be pretty common for the county, but there is no guarantee the water is safe to drink. One resident explained that their well water was completely contaminated which is why
                                             they come to gather their water through this natural spring. Though many water bottle companies advertise their water coming from natural springs, experts explain that natural springs actually aren't
                                             any safer than getting water from a groundwater wells or surface water wells. 
                                              "),
                                           img(src = "private-well.png", style="display: block; margin-left: auto; margin-right: auto;" , width = "300px"),
                                           p(tags$small("Image Source: Virginia Department of Health")) ) ,
                                    column(4, 
                                           h4(strong("Yield vs Depth")),
                                           p("From the New River Valley Water Supply Plan in 2011, there are 5 community wells that we have data on. Each of these wells have a different depth and a different maximum daily withdrawal. 
                                             We attempted to make an inference on the correlation between the well depth and the water yield. Because these are the only 5 well systems we could find data on, 
                                             we cannot make an accurate inference on whether or not the depth of a well correlates to the water yield and amount of contamination. "),
                                           p("As you can see, the well that uses the least percent of maximum withdrawals daily is Community Center well, second to the left. However, their well depth is the deepest. Because we do not have any more 
                                             information than what you see here, we cannot be certain but we can infer that once the well is dug deeper, contamination chances increases which is why their percent usage is less
                                             than the other shallow wells. The other 4 wells are pretty constant in their daily withdrawals and their depths. The average depth of wells seems to be 200-300 ft deep which yields 
                                             an average amount of clean water. ")), 
                                    column(8, 
                                           h4(strong("Well Data")),
                                         selectInput("var2", "Select Variable:", width = "100%", choices = c(
                                           "Average Daily Withdrawals (GPD)" = "gpd",
                                           "Well Depth with Percent of Usage" = "depth", 
                                           "Maximum Daily Withdrawals" = "max")
                                         ), 
                                         plotlyOutput("wells"), 
                                         p(tags$small("Data Source: New River Valley Water Supply Plan 2011"))),
                                    tags$br(), 
                                    h4("References:"),
                                    p(tags$small("[1] A Guide to Private Wells (pp. 5-25, Publication). (1995). Blacksburg, VA: Virginia Water Resources Research Center.")),
                                    p(tags$small("[2] Private water systems. (2014, January 17). Retrieved July 29, 2021, from https://www.cdc.gov/healthywater/drinking/private/index.html")) 
                             
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
                                              The two most common pipe materials were plastic (63%) and copper (25%). [1]") , 
                                              h4(strong("Mining")),
                                              p("Mining is the extraction of valuable minerals or other geological materials from the earth, usually from an orebody, lode, vein, seam, reef or placer deposit. These deposits form a mineralized package that is of economic interest to the miner [3]. Mining can badly affect the environment at the local, regional, and global levels through direct and indirect mining practices by causing contamination of surface water and groundwater through the mining processes. The United States Geological Survey 
                                                (USGS) lists 39 mines in Floyd County, Virginia. Currently, a rock quarry which extracts the mineral amphibolite is the only active site amongst the USGS listed mines in the county.  "),
                                              p("Prior data on previously active mines list copper, iron, sulphur-pyrite, pig iron, gold, sulphur, nickel, lead, cobalt and arsenic as the most commonly mined commodities[4] [5]. While the majority of the mines in the county are currently abandoned, they might still pose a significant threat to surface and groundwater contamination.  "),
                                              p("These abandoned mines provide direct routes for any contaminants to reach the groundwater unless they are properly closed off [4]. If proper precautions are not taken, unnaturally high concentrations of chemicals, such as arsenic, sulphuric acid, lead, mercury and other dangerous contaminants can spread over a significant area of surface or subsurface water (groundwater). Mines use a large amount of water for mine drainage (metal-rich water), mine cooling, aqueous extraction and other mining processes which increase the potential for these chemicals to contaminate ground and surface water [6]. Sub-surface mining often progresses below the water table, so the water must be constantly pumped out of the mine to prevent flooding. When a mine is abandoned, the pumping ceases, and water floods the mine leading to a build-up of contaminated water. "),
                                              p("This introduction of water is the initial step in most acid rock drainage situations [7]. Therefore, if no disposal methods are put in place for these abandoned mines to have contaminated water pumped and treated, then this build-up of water with these contaminants can seep into the water table or become a part of the surface runoff. Surface runoff containing these dissolved heavy metals such as copper, lead, mercury and other chemicals can lead to significant damage of the surrounding vegetation, aquatic life and pose serious health risks to humans.")
                                       ),
                                      
                                       column(8, 
                                              h4(strong("Table of Common Contaminants")),
                                             selectInput("contam", "Select Variable:", width = "100%", choices = c(
                                               "Percent Common Contaminants [1]" = "percent",
                                               "Groups of Common Contaminants [2]" = "group")
                                             ),
                                             withSpinner(tableOutput("sources")),
                                             p(tags$small("All water testing: The Water Quality Laboratory of the Department of Biological Systems Engineering and Soils Testing Laboratory of the Department of Crop and Soil Environmental Sciences at Virginia Tech")),
                                             tags$br(), 
                                             h4(strong("Map of Abandoned Mines by Block Group")),
                                             withSpinner(leafletOutput("mines")),
                                             p(tags$small("Data Source: The Department of Mines, Minerals and Energy")))  ,
                                       column(12, 
                                              h4("References: "), 
                                              p(tags$small("[1] V. (n.d.). Evaluation of Household Water Quality in Floyd County, Virginia APRIL 2010 (pp. 1-6, Rep.). Virginia Polytechnic Institute and State University. doi:Virginia Cooperative Extension")),
                                              p(tags$small("[2] T. (n.d.). 2018 DRINKING WATER QUALITY REPORT (pp. 1-7, Rep.). Town of Christiansburg.")) ,
                                              p(tags$small("[3] GeologyIn. (2014, March 09). Retrieved from GeologyIn: https://www.geologyin.com/2014/03/what-is-mining.html ")),
                                              p(tags$small("[4] Virginia Department of Mines, Minerals and Energy. (2010, October 20). Retrieved from DMME")),
                                              p(tags$small("[5] The Diggings. (© The Diggings™, 2021). Retrieved from The Diggings: https://thediggings.com/usa/virginia/floyd-va063/mines?p=2")),
                                              p(tags$small("[6] Mohapatra, D. P., & Kirpalani, D. M. (2016, December 19). Process effluents and mine tailings: Sources, effects and management and role of nanotechnology. Nanotechnology for Environmental Engineering. https://link.springer.com/article/10.1007/s41204-016-0011-6#citeas. ")),
                                              p(tags$small("[7] Environment (2016, June 17) Retrieved from Environment https://www.environment.co.za/acid-mine-drainage-amd/what-is-acid-mine-drainage-amd-wikipedia-definition.html"))) 
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
                                                withSpinner(tableOutput("qualityRec")),
                                                p(tags$small("Data Sources listed below"))

                                         ) , 
                                         column(6, 
                                                p("", style = "padding-top:10px;"),
                                                p("The most common contaminants found in drinking water surrounding the New Rivery Valley Area 
                                                 included total coliform bacteria, low levels of pH, sodium and chloride, lead and copper and manganese [2]. "),
                                                br(), 
                                                tags$li("Disinfect", strong(" total coliform bacteria, "), "the Virginia Department of Health recommends the use of Shock Chlorination to clean and sanitize
                                                  the well and entire plumbing system [7]"), 
                                                tags$li("Combatting", strong("manganese: "), " it is recomended to get a distillation or filtration system because it is secondary maximum contaminant level, maximum 0.05 mg/L [3]"),
                                                tags$li("Raising ", strong("low pH levels (<7):"), " is installing an acid neutralizing filter that passes through calcite that raises the pH [4]"),
                                                tags$li("Protecting water from ", strong("lead and copper: "), "leeching into the water, regularly clean your faucet’s screen (also known as an aerator). Sediment, debris, and lead particles can collect in your aerator. 
                                                     Make sure to run your water for at least a minute and use only cold water. If not hot water, use cold and boil on stove. [5]"),
                                                tags$li("For eliminating ", strong("chloride and sodium: "), " activated carbon filters are the most common devices used to dechlorinate
                                                water, remove objectionable chlorine tastes, and reduce corrosion of plumbing systems [6]"),
                                                tags$li("Dealing with", strong("hard water: "), " use water softeners which exchange the minerals (iron, magnesium, and calcium) for sodium. To avoid potential risks, one could
                                                     only soften the the hot water supply to take showers and baths and clean around the house and leave the cold water available for consumption. [6]"),
                                                tags$li("To condition well water for ", strong("safer and cleaner water: "), " many use water softening, iron removal, neutralization of acid water, reverse
                                                osmosis, turbidity control, removal of objectionable tastes and odors, and aeration [7]"),
                                                tags$li("The issue with ", strong("water softeners ,"), " when used in conjuction with filters causes the water to smell rotten. Also, the addition of sodium into the water
                                                     can be a health risk and should talk to their physican. [6]"),
                                                br(), 
                                                p("Protecting water from", strong("agricultural runoff"), "like sediments, animal feeding operations, livestock grazing, irrigation and pesticides can include measures like",  
                                                  strong("applying management practices that control the volume and flow rate"),  "of runoff water, keep the soil in place, and reduce soil transport;", 
                                                  strong("adjusting grazing intensity, keeping livestock out of sensitive areas,"), " providing alternative sources of water and shade, and promoting revegetation of ranges, pastures, and riparian zones; ", strong("applying only the amount of water required for crops,"), " 
                                                  converting irrigation systems to higher efficiency equipment; and following ", strong("Integrated Pest Management Technology"), "to use natural barriers and limit pesticide uses [1]. "), 
                                                p("Creating ", strong("new green infrastructure, replacing the polluting mining, and manufacturing industries,"), " will not only create new sustainable jobs but reduce the pollution going into the groundwater."), 
                                                br()),
                                         h4("References: ", algin = "ceneter"),
                                         p(tags$small("[1] Protecting Water Quality from Agricultural Runoff [PDF]. (2005, March). Washington, DC: United States Environmental Protection Agency.")), 
                                         p(tags$small("[2] V. (n.d.). Evaluation of Household Water Quality in Floyd County, Virginia APRIL 2010 (pp. 1-6, Rep.). Virginia Polytechnic Institute and State University. doi:Virginia Cooperative Extension")),
                                         p(tags$small("[3] Learn about water. (n.d.). Retrieved July 26, 2021, from https://www.wqa.org/learn-about-water/water-q-a/manganese")),
                                         p(tags$small("[4] University of Massachusetts Amherst. (2018, March 22). Ph – acidity of private drinking water wells. Retrieved July 26, 2021, from https://ag.umass.edu/cafe/fact-sheets/ph-acidity-of-private-drinking-water-wells")), 
                                         p(tags$small("[5] General Information about Lead in Drinking Water. (n.d.). Retrieved July 26, 2021, from https://www.epa.gov/ground-water-and-drinking-water/basic-information-about-lead-drinking-water#reducehome")), 
                                         p(tags$small("[6] Benham, B., Ling, E. J., Scott, J. P., Haering, K., &amp; Wright, B. (2011). Virginia Household Water Quality Program: Sodium and Chloride in Household Drinking Water (pp. 1-4, Publication No. 442-661). Communications and Marketing, College of Agriculture and Life Sciences, Virginia Polytechnic Institute and State University.")),
                                         p(tags$small("[7] A Guide to Private Wells (pp. 5-25, Publication). (1995). Blacksburg, VA: Virginia Water Resources Research Center.")) 
                                         
                                ), 
                                tabPanel("Well Testing Kit",
                                         fluidRow(style = "margin: 6px;",
                                                  p("", style = "padding-top:10px;"),
                                                  h5("Since one of our team members were local to Floyd County, he picked up a FREE well water testing kit from a local store in order to see how easy the process of testing their water is. 
                                                     Once he got home, he opened up the kit and here are photos of what the kit contains. We would like to make very apparent that the county hands these out for FREE at most local stores in order to 
                                                     keep their residents safe. Once the instructions are followed on how to take a water sample and put into the vial, the kit comes with an envelope already addressed where the vial will go. 
                                                     Since the county is looking to increasing their testing, the envelope does not need a stamp and costs nothing to mail other than putting it into a mailbox or getting dropped off at the post office. 
                                                     Note there is a survey that needs to be filled out of personal information in order to get the results back to the correct person but all of this information is confidental. 
                                                     The stakeholders mentioned many did not want their address or coordinates or information to be public record and they have ensured us the coordinates, location and results of these well water testing kits do 
                                                     not get shared with anyone besides those who are testing it. Once the survey is filled out and sealed into the envelope with the vial, the last step is sending it off. The results may take
                                                     up to a week to get back. This is a huge step towards cleaner, safer water in the county because the problem cannot be fixed if we do not know what we are working with. "), 
                                                  tags$br(),
                                                  tags$br(), 
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
                                         column(6, 
                                                p("", style = "padding-top:10px;"),
                                                p("Based on research from the Environmental Protection Agency, the Virginia Water Resources Research Center, and Virginia Cooperative Extension 2010 report, we have several recommendations that could help aid in increasing water quantity throughout the county."),
                                                p("Lack of viable drinking water poses a large threat to the county. While the use of private wells is widespread, polluted non-potable groundwater continues 
                                                  to flow into these wells rendering them useless at times. It is common in the county for residents who have private well systems to rarely use them for 
                                                  consumptions due to poor quality.  To help combat this issue, residents have the option to ensure safe use and disposal of common harmful substances such as
                                                  ", strong("motor oil, pesticides, leftover paint, moth balls flea collars, household cleaners, and medication"), " just to name a few. These products, if disposed of improperly,
                                                  have the potential to contaminate the soil, groundwater, and nearby surface water.[1] As opposed to pouring these liquids down the drain or into the lawn, 
                                                  hazardous waste dump site exist within the county as well as services that will come to a resident’s home and remove the waste for later proper disposal. 
                                                  Taking these steps not only aids in decreasing water contamination, but increasing the viable water supply within the county. Widespread water testing is needed to 
                                                  minimize groundwater pollution. If residents understand what is in their drinking water, they will likely be more conscious of how they are disposing of their waste. ")),
                                         column(6, 
                                                p("", style = "padding-top:10px;"),
                                                p("Due to Floyd’s geographic location, residents will continue to use private well and natural spring systems in much of the county. While these systems 
                                                  are commonplace throughout the United States as whole, they are prone to deterioration over time. It is recommended by the Center for Disease Control 
                                                  and Prevention that well systems are tested annually for ", strong("mechanical problems, cleanliness, and presence of contaminants [2].")," Additionally, naturally 
                                                  fed systems have the potential to become blocked or slow outlet feed overtime. Proper maintenance of well system and spring systems is imperative 
                                                  to ensure that the water being consumed is safe and that the residents are receiving the proper amount of water for their needs [3]. If flow rates begin to 
                                                  decrease, this may be a sign of mechanical issues or blockage. If these issues are not remediable, it is common for residents to acquire water storage 
                                                  tanks to pump water continually out of a well into a storage vessel to.  . Because water quality and quantity are correlated, once the county addresses the water quality issue in the 
                                                  groundwater and nearby surface water, their water quantity supply will likely increase.  ")),
                                         column(12, 
                                         h4("References:"),
                                         p(tags$small("[1] How Can You Help Protect Source Water? (n.d.). Retrieved July 29, 2021, from https://www.epa.gov/sourcewaterprotection/how-can-you-help-protect-source-water")), 
                                         p(tags$small("[2] Well maintenance. (2009, April 10). Retrieved July 29, 2021, from https://www.cdc.gov/healthywater/drinking/private/wells/maintenance.html#:~:text=Wells%20should%20be%20checked%20and,example%2C%20arsenic%20and%20radon).")) ,
                                         p(tags$small("[3] A Guide to Private Wells (pp. 5-25, Publication). (1995). Blacksburg, VA: Virginia Water Resources Research Center.")) ) 
                                         
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
                                "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural'), "and", a(href = 'https://ext.vt.edu/','Applied Economics and the Virginia Cooperative Extension Service.'),
                                "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical
                                social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how 
                                information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply,
                                and our annual symposium, please visit", 
                                a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                              p("", style = "padding-top:10px;")
                     ),
                     fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                              column(6, align = "center",
                                     h4(strong("DSPG Team Members")),
                                     img(src = "team-esha.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "team-julie.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     br(), 
                                     img(src = "team-ryan.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "team-john.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     p(a(href = 'https://www.linkedin.com/in/esha-dwibedi-83a63476/', 'Esha Dwibedi', target = '_blank'), "(Virginia Tech, PHD in Economics);",
                                       br(), 
                                       a(href = 'https://www.linkedin.com/in/julie-rebstock', 'Julie Rebstock', target = '_blank'), "(Virgina Tech, Undergraduate in Economics and Computational Modeling and Data Analytics);",
                                       br(), 
                                       a(href = 'https://www.linkedin.com/in/ryan-jacobs-bb5727174/', 'Ryan Jacobs', target = '_blank'), "(Virginia Tech, Undergraduate in Environmental Economics, Management, and Policy, and Minoring in Industrial Design).",
                                       br(), 
                                        a(href = 'https://www.linkedin.com/in/john-wright-9a13621a0/', 'John Wright', target = '_blank'), "(Virginia State Univeristy, Undergraduate in Statistical and Data Science)."),
                                     p("", style = "padding-top:10px;") 
                              ),
                              column(6, align = "center",
                                     h4(strong("VT Faculty Team Members")),
                                     img(src = "team-posadas.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                     img(src = "team-sarah.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                     p(a(href = "https://www.linkedin.com/in/briannaposadas/", 'Dr. Brianna B. Posadas', target = '_blank'), "(Postdoctoral Associate Department of Agricultural, Leadership, & Community Education);",
                                       br(), 
                                       a(href = '', 'Dr. Sarah Melissa Witiak', target = '_blank'), "(Associate Professor Department of Biology Virginia State University)."),
                                     p("", style = "padding-top:10px;")
                              )
                     ),
                     fluidRow(tyle = "margin-left: 100px; margin-right: 100px;",
                              align = "center",
                              h4(strong("Project Stakeholders")),
                              img(src = "stake-dawn.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                              img(src = "stake-john.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                              p(a(href = '', 'Dawn Barnes', target = '_blank'), "(Virginia Cooperative Extension, Floyd County at Virginia Tech);",
                                br(), 
                                a(href = '', 'Jon Vest', target = '_blank'), "(Virginia Cooperative Extension, Floyd County at Virginia Tech)."),
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
            
            p <- ggplot(climate, aes(fill = County, x = Month, y = Rainfall)) + 
                geom_bar(position="dodge", stat="identity") + scale_fill_viridis_d() + 
                scale_x_discrete(limits = month.abb)+ 
                labs(title = "Average Monthly Rainfall", 
                     y="Rainfall (in)") + theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())
          
          ggplotly(p, tooltip = "y")
        
            
        }else if (var1() == "min") {
            
            p <- ggplot(climate, aes(fill = County, x = Month, y = Min_Temp)) + 
                geom_bar(position="dodge", stat="identity") + scale_fill_viridis_d() + 
                scale_x_discrete(limits = month.abb) +
                labs(title = "Minimum Monthly Temperature",
                     y = "Temperature (F)")  + theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())
          
          ggplotly(p, tooltip = "y")
            
            
        }else {
            
            p <- ggplot(climate, aes(fill = County, x = Month, y = Max_Temp)) + 
                geom_bar(position="dodge", stat="identity") + scale_fill_viridis_d() + 
                scale_x_discrete(limits = month.abb) + 
                labs(title = "Maximum Monthly Temperature",
                     y = "Temperature (F) ") + theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())
          
          ggplotly(p, tooltip = "y")
            
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
            
            p <- wells %>% 
                ggplot(aes(x = Names, y = Withdrawl_GPD, fill = Names)) + 
                geom_bar(stat = "identity", position = "dodge")+ 
                labs(title = "Average Daily Withdrawals of Community Wells", 
                     x = "Wells",
                     y = "Withdrawal (GPD) ") + scale_fill_viridis_d()  + theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank() , legend.position = "none") 
          
          ggplotly(p, tooltip = "y")
            
            
        }else if (var2() == "depth") {
            wells$Percent <- wells$Withdrawl_GPD/wells$Max_GPD
            
            p <- ggplot(wells, aes(x = Names, fill = "red")) + 
                geom_col(aes(y = Percent))+ 
                geom_line(aes(y = Well_Depth/1000), color = "blue", size = 2, group = 1) +
                scale_y_continuous("Percent", sec.axis = sec_axis(~.*1000, name = "Depth (ft)"))  +  
                labs(title = "Percent of Usage and Well Depth of Community Wells", x = "Wells") + theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") 
            
            ggplotly(p, tooltip = "y")
            
            
        }else {
            p <- wells %>% 
                ggplot(aes(x = Names, y = Max_GPD, fill = Names)) + 
                geom_bar(stat = "identity", position = "dodge")+ 
                labs(title = "Maximum Daily Withdrawals of Community Wells", 
                     x = "Wells",
                     y = "Withdrawal (GPD)") + scale_fill_viridis_d()  + theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank() , legend.position = "none")
          
          ggplotly(p, tooltip = "y")
            
            
        }
        
    })
    
    state_class_colors <- c("#9dfef8","#00cc00","#2621ff","#ffa500","#ffff19","#ff0000" )
    
    
    output$landParcel2020 <- renderLeaflet({
      
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
    output$landParcel2017 <-renderLeaflet({
  
        
        leaflet(options = leafletOptions(minZoom = 10)) %>%
          addProviderTiles(provider = "CartoDB.Positron") %>%
          addPolygons(data = com17,
                      stroke = FALSE,
                      smoothFactor = 0,
                      fillOpacity = 0.7,
                      fillColor = state_class_colors[1],
                      group = "Commercial/Industrial") %>%
          addPolygons(data = agr17,
                      stroke = FALSE,
                      smoothFactor = 0,
                      fillOpacity = 0.7,
                      fillColor = state_class_colors[2] ,
                      group = "Agricultural/Undeveloped (20 – 99 acres)") %>%
          addPolygons(data = agr_large17,
                      stroke = FALSE,
                      smoothFactor = 0,
                      fillOpacity = 0.7,
                      fillColor = state_class_colors[3],
                      group = "Agricultural/Undeveloped (100 acres and up)") %>%
          addPolygons(data = single17,
                      stroke = FALSE,
                      smoothFactor = 0,
                      fillOpacity = 0.7,
                      fillColor = state_class_colors[4],
                      group = "Single-Family Residential(Suburban 0-19.99 acres)") %>%
          addPolygons(data = single_urban17,
                      stroke = FALSE,
                      smoothFactor = 0,
                      fillOpacity = 0.7,
                      fillColor = state_class_colors[5],
                      group = "Single Family Residential(Urban)") %>%
          addPolygons(data = mult17,
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
    
    output$landParcel2013 <- renderLeaflet({
          
          leaflet(options = leafletOptions(minZoom = 10)) %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(data = com13,
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        fillColor = state_class_colors[1],
                        group = "Commercial/Industrial") %>%
            addPolygons(data = agr_large13,
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        fillColor = state_class_colors[3],
                        group = "Agricultural/Undeveloped (100 acres and up)") %>%
            addPolygons(data = single13,
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        fillColor = state_class_colors[4],
                        group = "Single-Family Residential(Suburban 0-19.99 acres)") %>%
            addPolygons(data = single_urban13,
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        fillColor = state_class_colors[5],
                        group = "Single Family Residential(Urban)") %>%
            addPolygons(data = mult13,
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        fillColor = state_class_colors[6],
                        group = "Multi-Family") %>%
            addLayersControl(
              position = "bottomright",
              overlayGroups = c(
                                "Agricultural/Undeveloped (100 acres and up)",
                                "Single-Family Residential(Suburban 0-19.99 acres)",
                                "Single Family Residential(Urban)",
                                "Multi-Family",
                                "Commercial/Industrial"),
              options = layersControlOptions(collapsed = FALSE))
    })
    
    grace_month <- grace %>%
      group_by(month, year) %>%
      summarise(max_ELWT_CSR = sum(ELWT_CSR))
    
    graceM <- reactive({
      input$graceM
    })
    output$grace <- renderPlotly({
      if (graceM() == "ground"){
        grace_monthly<- grace_month %>%
          group_by(month) %>%
          summarise(average = mean(max_ELWT_CSR))
        
        p <- grace_monthly %>%
          ggplot(aes(x = month, y = average)) +
          geom_line(color = "darkorchid4") +
          labs(title = "Monthly Average of Liquid Water Thickness calculated by CSR (in cm), Floyd",
               subtitle = "Data of terrestrial water storage anomolies plotted by year",
               y = "Equivalent Liquid Water Thickness calculated by CSR (in cm)",
               x = "Month") + theme_bw(base_size = 11)
        
        ggplotly(p, tooltip = "y")
        
      } else if (graceM() == "groundY"){
        
        grace_yearly <- grace_month %>%
          group_by(year) %>%
          summarise(average = mean(max_ELWT_CSR))
        
        p <- grace_yearly %>%
          ggplot(aes(x = year, y = average)) +
          geom_line(color = "darkorchid4") +
          labs(title = "Yearly Average of Liquid Water Thickness calculated by CSR (in cm), Floyd",
               subtitle = "Data of terrestrial water storage anomolies plotted by year",
               y = "Equivalent Liquid Water Thickness calculated by CSR (in cm)",
               x = "Year") + theme_bw(base_size = 11)
        
        ggplotly(p, tooltip = "y")
      }
      else {
        # subset 2 months around anomalies
        p <- grace_month %>%
          ggplot(aes(x = month, y = max_ELWT_CSR)) +
          geom_bar(stat = "identity", fill = "darkorchid4") +
          facet_wrap(~ year, ncol = 3) +
          labs(title = "Total Monthly Equivalent Liquid Water Thickness calculated by CSR (in cm), Floyd",
               subtitle = "Data of terrestrial water storage anomolies plotted by year",
               y = "Equivalent Liquid Water Thickness calculated by CSR (in cm)",
               x = "Month") + theme_bw(base_size = 11)
        
        ggplotly(p, tooltip = "y")
        
        
        
      }
      
      
      
    })
    
 
    
    
    econ1 <- reactive({
        input$econ1
    })
    
    output$trend1 <- renderPlotly({
        
        if(econ1() == "industry") {
            
          plot <- ggplot(industry_overtime, aes(group = Label, x = Year, y = Estimate, color = Label)) + 
            geom_line() + 
            labs(title = "Industry Sector ",
                 y="Total Residents Employed ", x= "")+ theme_minimal() +
            theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="bottom")
          
          ggplotly(plot, tooltip = "y") %>% layout(legend = list(orientation = "h", y=-0.2)) 
         
          
         
            
        }else if (econ1() == "commute") {
            p <- ggplot(data = commute_df,mapping = aes(Type,Quantity, fill = Type))+geom_bar(stat = "identity")+
              scale_fill_viridis_d() + 
                labs(title = "Floyd County Commuting",
                     x = "",
                     y = "Total Residents")   + theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank() , legend.position = "none")
            
            ggplotly(p, tooltip = "y")
            
            
            
            
        }else if (econ1() == "pop"){ 
            
            p <- ggplot(popch_df, aes(x=Year,y=Population,group=1)) +
                geom_line()+geom_point()+
                labs(title = "Projected Floyd Population Change",
                      x= "") + theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())
          
          ggplotly(p, tooltip = "y")
            
            
            
        }else if (econ1() == "age") {
            
            p <- ggplot(data = popage_df,mapping = aes(Age,Quantity, fill = Age))+geom_bar(stat = "identity")+
            scale_fill_viridis_d() + 
                labs(title = "Floyd County Population by Age", 
                      y = "") + coord_flip() +  theme_minimal() +
              theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
          
          ggplotly(p, tooltip = "y")
            
            
            
        }else if (econ1() == "retail"){
          
          plot <- ggplot(retail, aes(group = Retail, x = Year, y = Sales/1000, color = Retail)) + 
            geom_line() + 
            labs(title = "Retail Sales by Type", 
                 y="Sales (100,000) ", x= "Retail Group")+  theme_minimal() +
            theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="bottom")
          
          ggplotly(plot, tooltip = "y") %>% layout(legend = list(orientation = "h", y=-0.2))
         
          
        }else if (econ1() == "capita"){
          
          p <- ggplot(capita_income, aes(fill = Area, x = Year, y = Amount)) + 
            geom_bar(position="dodge", stat="identity") + scale_fill_viridis_d() + 
            labs(title = "Income per Capita", 
                 y="Dollar ($) ") + theme_minimal() +
            theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())
          
          ggplotly(p, tooltip = "y")
          
        }else if (econ1() == "unemplo"){
          
          p <- ggplot(unempl, aes(group = Area, x = Year, y = Rate*100, color = Area)) + 
            geom_line(linetype = "dotted", size = 2) + 
            labs(title = "Unemployment Rate",
                 y="Rate %", x= "Year" ) + theme_minimal() +
            theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())
          
          ggplotly(p, tooltip = "y")
          
        }else {
            
            p <- ggplot(busgrowth_df, aes(x=Time,y=Quantity,group=1)) +
                geom_line()+geom_point()+
                labs(title = "New Business Growth in Floyd")+ theme_minimal() +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    axis.text.x = element_text(angle = 45, vjust = .5, color = "black"))
          
          ggplotly(p, tooltip = "y")
            
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
  
    output$parcelTable <- renderTable({
      percentTable <- read.csv(paste0(getwd(), "/data/land_parcel/percentTable.csv")) 
      
      colnames(percentTable) <- c("Land Parcel", "2013", "2017", "2020", "%change 13-17", "%change 17-20", "%change 13-20")
      percentTable
      
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "l", colnames = T, digits = 2)
    
    output$percentChange <- renderPlotly({
      
      p <- ggplot(aes(Year, Amount, group = Land, color = Land), data = percent_change) + 
        geom_line(size = 2, linetype = "dashed") + scale_color_manual(values=cbPalette)+ 
        labs(y = "Parcel Count", 
             x= "") + theme_minimal() +
        theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

      
      ggplotly(p, tooltip = "y") %>% layout(legend = list(orientation = "h", y=-0.2))
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
