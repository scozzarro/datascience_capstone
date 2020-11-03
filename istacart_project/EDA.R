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
  ylab('number of customers')
  

mean_1to2<- orders %>%
            filter(order_number == 2) %>%
            summarise(mean(days_since_prior_order))
  
mean_1to2<- mean_1to2[1,1]

mean_2to3<- orders %>%
  filter(order_number == 3) %>%
  summarise(mean(days_since_prior_order))

mean_2to3<- mean_2to3[1,1]

mean_3to4<- orders %>%
  filter(order_number == 4) %>%
  summarise(mean(days_since_prior_order))

mean_3to4<- mean_3to4[1,1]

mean_4to5<- orders %>%
  filter(order_number == 5) %>%
  summarise(mean(days_since_prior_order))

mean_4to5<- mean_4to5[1,1]


#What people order###########

# most ordered item
orders_by_product<- merge(order_products, products, by = 'product_id')

top10_product<- orders_by_product %>%
                group_by(product_name)%>%
                count(product_id)


top10_product<- top10_product[order(top10_product$n, decreasing = TRUE),]
top10_product<- top10_product[1:10,]

top10_product %>%
  ggplot(aes(x=product_name,y=n)) +
  geom_segment(aes(x=product_name, xend=product_name, y=0, yend=n), color='grey') +
  geom_point(color="#69b3a2", size = 4) +
  theme_light() +
  coord_flip()

#cart size
cart= orders_by_product %>%
       group_by(order_id) %>%
       summarise(cart_variety =product_id, cart_size = add_to_cart_order)

cartsize<- cart %>%
           group_by(order_id) %>%
           summarise(variance=n_distinct(cart_variety), size=sum(cart_size))

