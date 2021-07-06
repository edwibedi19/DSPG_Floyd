library(ggplot2)
library(readxl)
industry <- read_excel("~/Desktop/Floyd Water/Floyd Labor Market Statistics.xlsx")
View(industry)
industry_df <- as.data.frame(industry)
## Floyd County Employment by Industry ##
emp_df <- industry_df[1:24,1:2]
colnames(emp_df) <- c("Employment_by_Industry","Quantity")
emp_df$Quantity <- as.numeric(emp_df$Quantity)
emp_df[2,2] <- 0
emp_df[9,2] <- 0
ggplot(data = emp_df,mapping = aes(Employment_by_Industry,Quantity))+geom_bar(stat = "identity",fill="blue")+
  labs(title = "Floyd County Employment by Industry", 
       subtitle = "* indicates non-disclosable data",
       caption = "Data Source: Virginia Employment Commission, Economic Information & Analytics,
       Quarterly Census of Employment and Wages (QCEW), 4th", 
       x="Industry")+coord_flip()

## Floyd Population Change ##  
popch_df <- industry_df[34:38,1:3]
colnames(popch_df) <- c("Year","Population","percent_Change")
popch_df$Population <- as.numeric(popch_df$Population)
popch_df$percent_Change <- as.numeric(popch_df$percent_Change)
ggplot(popch_df, aes(x=Year,y=Population,group=1)) +
  geom_line()+geom_point()+
  labs(title = "Projected Floyd Population Change",
       caption = "Data Source: U.S. Census Bureau, Weldon Cooper Center for Public Service")

## Floyd County Population by Age ##
popage_df <- industry_df[54:71,1:2]
colnames(popage_df) <- c("Age","Quantity")
popage_df$Quantity <- as.numeric(popage_df$Quantity)
popage_df$Age <- factor(popage_df$Age, levels=unique(popage_df$Age))
ggplot(data = popage_df,mapping = aes(Age,Quantity))+geom_bar(stat = "identity",fill="blue")+
  labs(title = "Floyd County Population by Age",
       caption = "Data Source: 2010 Census")+coord_flip()

## Floyd County Commuting ##
commute_df <- industry_df[79:81,1:2]
colnames(commute_df) <- c("Type","Quantity")
commute_df$Type <- factor(commute_df$Type, levels=unique(commute_df$Type))
commute_df$Quantity <- as.numeric(commute_df$Quantity)
ggplot(data = commute_df,mapping = aes(Type,Quantity))+geom_bar(stat = "identity",fill="blue")+
  labs(title = "Floyd County Commuting",
       caption = "Data Source: U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination 
       Employment Statistics, 2014")+coord_flip()

## New Business Growth in Floyd ##
busgrowth_df <- industry_df[103:115,1:2]
colnames(busgrowth_df) <- c("Time","Quantity")
busgrowth_df$Time <- factor(busgrowth_df$Time, levels=unique(busgrowth_df$Time))
busgrowth_df$Quantity <- as.numeric(busgrowth_df$Quantity)
ggplot(busgrowth_df, aes(x=Time,y=Quantity,group=1)) +
  geom_line()+geom_point()+
  labs(title = "New Business Growth in Floyd",
       caption = "Data Source: Virginia Employment Commission, Economic Information & Analytics, 
       Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")
       