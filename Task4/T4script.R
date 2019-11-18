library(dplyr)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(forecast)

storedata <- read.csv("Task4/storedata.csv")

storedata$Order_Date <- as.Date(storedata$Order_Date)
table1 <- storedata %>% filter(year(Order_Date) == 2017 & month(Order_Date) >= 10) %>% filter(Region == "Region 1" | Region == "Region 9") %>% filter(Customer_Segment == "Corporate" | Customer_Segment == "Consumer")
table1 <- table1 %>% mutate(Month = month(Order_Date))
table1 <- table1 %>% group_by(Month, Region, Customer_Segment) %>% summarize(Total_Sales = sum(Sales))
ggplot(table1, aes(x=Month, y=Total_Sales, color = Customer_Segment)) + geom_line(size = 1.3) + facet_wrap(~Region) + ggtitle("Total sales for the last three months of 2017 in regions 1 and 9") + xlab("Months") + ylab("Total Sales") + labs(color = "Customer Segment") + theme_stata() + scale_color_brewer(palette = "Greens") 

tabfig1 <- storedata %>% filter(Region == "Region 1" | Region == "Region 13") %>% mutate(Year = year(Order_Date), Month = month(Order_Date)) %>% filter(Year != 2014) %>% group_by(Year, Month, Region) %>% summarize(Total_Sales = sum(Sales))
tabfig1$Total_Sales <- as.numeric(tabfig1$Total_Sales)
tabfig1 <- tabfig1 %>% mutate(Year_Month = paste(Year, Month, "01", sep="-"))
tabfig1$Year_Month <- as.Date(tabfig1$Year_Month, format="%Y-%m-%d")
tabfig1 <- tabfig1[,c(3:5)]
figure1 <- ggplot(tabfig1, aes(x=Year_Month, y=Total_Sales, color = Region)) + geom_line(size = 1.3) + ggtitle("Total sales for region 1 and 13, from 2015 to 2018 excluded") + xlab("Year") + ylab("Total Sales") + theme_stata() + scale_color_brewer(palette = "Greens")
figure1

table2 <- tabfig1 %>% spread(Region, Total_Sales)
names(table2) <- c("Total_Sales", "Region_1", "Region_13")
table2 <- table2 %>% filter(Region_13 > Region_1)

table3 <- storedata %>% filter(year(Order_Date) == 2017, Region != "Region 3", Region != "Region 5", Region != "Region 8") %>% group_by(Customer_Segment, Product_Category) %>% summarize(Average_Profit = mean(Profit))
maxtable3 <- table3 %>% ungroup() %>% filter(Average_Profit == max(Average_Profit))
ggplot(table3, aes(x = Product_Category, y = Average_Profit, fill=Product_Category)) + geom_histogram(stat="identity") + facet_wrap(~Customer_Segment) + ggtitle("Average profit by customer segment and product category in 2017") + ylab("Average Profit") + labs(fill = "Product Category") + theme_stata() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_fill_brewer(palette = "Greens")

Model_List <- 
  list(p = c(0,1,2,3,4),
       q = c(0,1,2,3,4),
       d = c(0,1),
       P = c(0,1,2,3,4),
       Q = c(0,1,2,3,4),
       D = c(0,1),
       S = 12,
       sep = c(", ")) %>% 
  cross() %>% map(lift(paste))
model1416 <- storedata %>% filter(year(Order_Date) < 2017) %>% filter(Customer_Segment == "Small Business" & Product_Category == "Office Supplies")
auto.arima(model1416$Order_Quantity, max.p = 4, max.q = 4, max.P = 4, max.Q = 4, max.d = 1, max.D = 1, start.p = 0, start.q = 0, start.P = 0, start.Q = 0, seasonal = TRUE, stationary = TRUE)
sarima <- Arima(model1416$Order_Quantity, order=c(3,1,2), seasonal=list(order = c(1,1,2), period = 12))
sarima %>% forecast(h=200) %>% autoplot + theme_stata()