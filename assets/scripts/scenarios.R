#Scenario 1
library(RODBC)
library(DBI)
library(odbc)
conn1 <- odbcConnect("MySQL_DNS")#Stablishing connection with MySQL

df_1 <-sqlQuery(conn1,'SELECT * FROM employees;') #Creating df_1

df_1#Printing results from df_1

#Creating df_2
df_2 <-sqlQuery(conn1,"SELECT 'Customers' AS 'Table', COUNT(*) AS Num_Rows FROM customers
                    UNION
                    SELECT 'Employees' AS 'Table', COUNT(*) AS Num_Rows FROM employees
                    UNION
                    SELECT 'Offices' AS 'Table', COUNT(*) AS Num_Rows FROM offices
                    UNION
                    SELECT 'Order Details' AS 'Table', COUNT(*) AS Num_Rows FROM orderdetails
                    UNION
                    SELECT 'Orders' AS 'Table', COUNT(*) AS Num_Rows FROM orders
                    UNION
                    SELECT 'Payments' AS 'Table', COUNT(*) AS Num_Rows FROM payments
                    UNION
                    SELECT 'Product Lines' AS 'Table', COUNT(*) AS Num_Rows FROM productlines
                    UNION
                    SELECT 'Products' AS 'Table', COUNT(*) AS Num_Rows FROM products;")

library(ggplot2) #importing library
theme_set(theme_classic())

# Plot
g <- ggplot(df_2, aes(Table, Num_Rows))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
theme(text = element_text(size = 20))+
  labs(title="Bar Chart", 
       subtitle="Records per table in classicmodels DB", 
       caption="Source: Classic Models DB ") +
  
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Scenario 2

#Creating df_3
df_3 <-sqlQuery(conn1,"SELECT  productLine,
                        SUM(quantityOrdered*priceEach) AS TotalSalesVolume
                FROM    productlines
                        JOIN products USING (productLine)
                        JOIN orderdetails USING (productCode)
                GROUP BY    productLine
                ORDER BY    TotalSalesVolume DESC;")
#Plotting

library(tidyverse)#Remember to load this library for scenario 2

# Get the positions
df_4 <- df_3 %>% 
  mutate(csum = rev(cumsum(rev(TotalSalesVolume))), 
         pos = TotalSalesVolume/2 + lead(csum, 1),
         pos = if_else(is.na(pos), TotalSalesVolume/2, pos))

ggplot(df_3, aes(x = "", y = TotalSalesVolume, fill = fct_inorder(productLine))) +
  geom_col(width = 1, color = 1) +
  geom_text(aes(label = TotalSalesVolume,x=1.6),
            position = position_stack(vjust = 0.6)) +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "productLine")) +
  scale_y_continuous(breaks = df_4$pos, labels = df_3$productLine) +
  ggtitle("Product Line by Total Sales Volume")+
  theme(text = element_text(size = 20),
        axis.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "white"))

#Scenario 3

#Creating df_5
df_5 <-sqlQuery(conn1,"SELECT CONCAT(firstName, ',', lastName) AS FIRSTNAME,
                      SUM(quantityOrdered*priceEach) AS totalSales
                      FROM    orderdetails JOIN orders USING (orderNumber)
                      JOIN customers USING (customerNumber)
                      JOIN employees ON
                      customers.salesRepEmployeeNumber = employees.employeeNumber
                      GROUP BY    salesRepEmployeeNumber
                      ORDER BY    totalSales DESC;")

#Initialising diverging barchart
theme_set(theme_bw())  

# Data Prep
data(datatable(df_5))  # load data
df_5$RepName <- row.names(df_5)  # create new column for car names
df_5$totalSales_z<- round((df_5$totalSales - mean(df_5$totalSales))/sd(df_5$totalSales), 2)  # compute normalized total sales
df_5$totalSales_type <- ifelse(df_5$totalSales_z < 0, "below", "above")  # above / below avg flag
df_5 <- df_5[order(df_5$totalSales_z), ]  # sort
df_5$RepName  <- factor(df_5$RepName, levels = df_5$RepName)  # convert to factor to retain sorted order in plot.

# Diverging barchart
ggplot(df_5, aes(x=RepName, y=totalSales_z, label=totalSales_z),) + 
  geom_bar(stat='identity', aes(fill=totalSales_type), width=.9)  +
  geom_text(aes(label = FIRSTNAME), hjust = 0.1,nudge_y = -.5,colour = "black",fontface="bold") +
  
  scale_fill_manual(name="Total Sales", 
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised total sales for Sales Reps", 
       title= "Diverging Bars") + 
  coord_flip()

#Scenario 4

#Creating df_6
df_6 <-sqlQuery(conn1,"select
                        productCode,
                        productName,
                        productLine,
                        productVendor
                        from classicmodels.products;")

theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(df_6, aes(productVendor))
g + geom_bar(aes(fill=productLine), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="ProductVendor across productLine") 


#Scenario 5

#Creating df_7
df_7 <-sqlQuery(conn1,"SELECT                                     
                      productName,
                              SUM(quantityOrdered*priceEach) AS totalSales
                      FROM    products JOIN
                              orderdetails USING (productCode)
                      GROUP BY    productCode
                      ORDER BY    totalSales DESC
                                    limit 40;")

# Prepare data: product name by revenue.
df7_pbr <- aggregate(df_7$totalSales, list(df_7$productName), mean)  # aggregate
colnames(df7_pbr) <- c("productName", "totalSales")  # change column names
df7_pbr  <- df7_pbr[order(df7_pbr$totalSales), ]  # sort
df7_pbr$productName <- factor(df7_pbr$productName , levels = df7_pbr$productName)  # to retain the order in plot.
head(df7_pbr, 4)
library(scales)
library(ggplot2)
theme_set(theme_bw())

# Draw plot
ggplot(df7_pbr, aes(x=productName, y=totalSales)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  scale_y_continuous(name="totalSales", labels = comma)+
  labs(title="Ordered Bar Chart", 
       subtitle="Product by Revenue", 
       caption="source: classicmodels db") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Scenario 6

#Creating df_8
df_8 <-sqlQuery(conn1,"SELECT
                        productLine, 
                        productCode,
                                SUM(quantityOrdered*priceEach) AS totalSales
                        FROM    products JOIN
                                orderdetails USING (productCode)
                        GROUP BY    productCode
                        ORDER BY    totalSales DESC;")

library(treemapify)#We need this library to get a treemap

# plot
ggplot(df_8, aes(area = totalSales, fill = productLine, label = productCode,subgroup = productLine)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.5, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "white", place = "centre",
                    size = 15, grow = TRUE)

#Scenario 7

library(dplyr)#Import this library for this scenario

#Manually typing totalSales values for each of the countries
ddf = read.table(text="
country value
USA 3479192
UK 143651
Japan 457110
France 3083752
Australia 1147176
", header=T)
options(scipen=10000)#No scientific notation on legend
world <- map_data("world")

world %>%
  merge(ddf, by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = value))+
  labs(title="World Map", 
       subtitle="Office countries by Revenue", 
       caption="source: classicmodels db") +
  geom_polygon()



