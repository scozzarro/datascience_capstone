#load packages######

library(tidyverse)
library(data.table)
library(ggplot2)
library(stringr)
library(DT)

#import datasets######

orders <- fread('orders.csv')
department<- fread('departments.csv')
products<- fread('products.csv')
order_products<- fread('order_products__train.csv')
order_products_prior<- fread('order_products__prior.csv')

#show data######

str(orders) #dataset overview
str(department) #dataset overview
str(products) #dataset overview
str(order_products) #dataset overview
str(order_products_prior) #dataset overview

summary(orders) #data overview
summary(department) #data overview
summary(products) #data overview
summary(order_products) #data overview
summary(order_products_prior) #data overview

#correct NAs######

orders$days_since_prior_order[is.na(orders$days_since_prior_order)]<- 0

#recode variables######

orders<- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products<- products %>% mutate(aisle_id = as.factor(aisle_id))
department<- department %>% mutate(department = as.factor(department))

#EDA######

#When people order ?##########

#hours of day

orders %>%
  ggplot(aes(order_hour_of_day)) +
  geom_histogram(stat = 'count', color="#69b3a2", fill="#69b3a2") +
  ggtitle("Order density per hours of the day")

#day of the week

orders %>% 
  ggplot(aes(order_dow)) +
  geom_histogram(stat = 'count',color="#69b3a2", fill="#69b3a2") +
  ggtitle('Order density per day of the week')

#how many time between orders

orders %>%
  ggplot(aes(days_since_prior_order)) +
  geom_histogram(stat = 'count', color="#69b3a2", fill="#69b3a2") +
  ggtitle('Days people order more often')


#how much people order?#####

orders %>%
  filter(eval_set == 'prior') %>%
  count(order_number)%>%
  ggplot(aes(order_number,n)) +
  geom_line(color="#69b3a2", size = 1) +
  ylab('number of customer')
  
  

