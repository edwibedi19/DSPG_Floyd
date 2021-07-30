library(tidyverse)
library(readxl)
library(raster)
library(rgdal)
library(maps) 
library(dplyr) 
library(ggplot2)
library(sf)
library(lubridate)
library(scales)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#import data
grace <- read.csv("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Floyd-County/DSPG_Floyd/shinyApp/data/land_parcel/GRACE_Floyd.csv",
                                 header = TRUE,
                                 na.strings = 999.99)
# view structure of data
str(grace)

# are there any unusual / No data values?
summary(grace$ELWT_CSR)

# plot the data using ggplot2 and pipes
grace %>%
  ggplot(aes(x = year, y = ELWT_CSR)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Groundwater Table Level Anomolies, Floyd",
       y = "Equivalent Liquid Water Thickness calculated by CSR (in cm)",
       x = "Year") + theme_bw(base_size = 12)


# calculate the sum grace value for each month
grace_month <- grace %>%
  group_by(month, year) %>%
  summarise(max_ELWT_CSR = sum(ELWT_CSR))%>%
  mutate(month = case_when(month == 1 ~ 'Jan', 
                           month == 2 ~ 'Feb',
                           month == 3 ~ 'Mar', 
                           month == 4 ~ 'Apr',
                           month == 5 ~ 'May', 
                           month == 6 ~ 'Jun',
                           month == 7 ~ 'Jul', 
                           month == 8 ~ 'Aug',
                           month == 9 ~ 'Sep', 
                           month == 10 ~ 'Oct',
                           month == 11 ~ 'Nov', 
                           month == 12 ~ 'Dec'
  ))



# subset 2 months around anomalies
grace_month %>%
  ggplot(aes(x = month, y = max_ELWT_CSR)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Total Monthly Equivalent Liquid Water Thickness calculated by CSR (in cm), Floyd",
       subtitle = "Data of terrestrial water storage anomolies plotted by year",
       y = "Equivalent Liquid Water Thickness calculated by CSR (in cm)",
       x = "Month") + theme_bw(base_size = 11)+scale_x_discrete(limits = month.abb)+ 
theme(axis.text.x = element_text(angle = 90))

grace_yearly <- grace_month %>%
  group_by(month) %>%
  summarise(average = mean(max_ELWT_CSR)) 

grace_yearly %>%
  ggplot(aes(x = month, y = average, group = 1)) +
  geom_line() +
  labs(title = "Yearly Average of Liquid Water Thickness calculated by CSR (in cm), Floyd",
       subtitle = "Data of terrestrial water storage anomolies plotted by year",
       y = "Equivalent Liquid Water Thickness calculated by CSR (in cm)",
       x = "Month") + theme_bw(base_size = 11)

