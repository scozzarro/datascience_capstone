#load packages######

library(tidyverse)
library(data.table)
library(ggplot2)
library(stringr)
library(corrplot)
library(treemap)
library(DT)

#import datasets######

orders <- fread('orders.csv')
department<- fread('departments.csv')
products<- fread('products.csv')
order_products<- fread('order_products__train.csv')
order_products_prior<- fread('order_products__prior.csv')
aisle<- fread('aisles.csv')

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
aisle <- aisle %>% mutate(aisle_id = as.factor(aisle_id))

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
  ylab('number of customers') +
  ggtitle('reorder trends')
  

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
  coord_flip() +
  ggtitle('Most ordered products')

#cart size
cart= orders_by_product %>%
       group_by(order_id) %>%
       summarise(cart_variety =product_id, cart_size = add_to_cart_order)

cartsize<- cart %>%
           group_by(order_id) %>%
           summarise(variance=n_distinct(cart_variety), size=sum(cart_size))

#cart size density

order_products %>%
  group_by(order_id) %>%
  summarise(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(n_items)) +
  geom_histogram(stat = 'count', color="white", fill="#69b3a2" ) +
  ggtitle('Cart size density')


#how many time people reorder the same item
order_products %>%
  mutate(reordered = as.factor(reordered)) %>%
  group_by(reordered) %>%
  summarise(count = n()) %>%
  ggplot(aes(reordered, count, fill = reordered)) +
  geom_bar(stat = 'identity') +
  ggtitle('How many time people reorder the same item')


#most reordered items
tmp <- orders_by_product %>%
       mutate(reordered = as.factor(reordered)) %>%
       filter(reordered == 1) %>%
       group_by(product_name) %>%
       summarise(n = n())

tmp<- tmp[order(tmp$n, decreasing = TRUE),]
tmp<- tmp[1:10,]

tmp %>%
  ggplot(aes(x=product_name, y=n)) +
  geom_segment(aes(x=product_name, xend=product_name, y=0, yend=n), color='grey') +
  geom_point(color="#69b3a2", size = 4) +
  theme_light() +
  coord_flip() +
  ggtitle('Most reordered products')

#most probably to be reordered
orders_by_product %>%
  group_by(product_name) %>%
  summarise(proportion_reorder=mean(reordered), n=n()) %>%
  arrange(desc(proportion_reorder)) %>%
  filter(n>40) %>%
  top_n(10, wt=proportion_reorder) %>%
  arrange(desc(proportion_reorder)) %>%
  ggplot(aes(x=product_name, y=proportion_reorder)) +
  geom_segment(aes(x=product_name, xend=product_name, y=0, yend=proportion_reorder), color='grey') +
  geom_point(color="#69b3a2", size = 4) +
  theme_light() +
  coord_flip() +
  ggtitle('Most probable to be reordered')


#Probability to be added first in the cart
order_products %>%
  group_by(product_id, add_to_cart_order) %>%
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)) %>%
  filter(add_to_cart_order == 1, count>10) %>%
  arrange(desc(pct)) %>%
  left_join(products, by = 'product_id') %>%
  select(product_name, pct, count) %>%
  ungroup() %>%
  top_n(10, wt = pct) %>%
  ggplot(aes(reorder(product_name,-pct), y = pct)) +
  geom_bar(stat = 'identity', fill = "#69b3a2") +
  ylab('Probability') +
  xlab('Product name') +
  ggtitle('Probability to add first in the cart') +
  coord_flip()

#Association last order and probability of reorder
order_products %>%
  left_join(orders, by = 'order_id') %>%
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x = days_since_prior_order, y = mean_reorder)) +
  geom_bar(stat = 'identity', fill = "#69b3a2") +
  ggtitle('Association between time of last order and reordered product')


#Association numbers of order and reordering
order_products %>%
  group_by(product_id) %>%
  summarise(prop = mean(reordered), n = n()) %>%
  ggplot(aes(n, prop)) +
  geom_point() +
  geom_smooth(color = "#69b3a2") +
  coord_cartesian(xlim = c(0,2000)) +
  ylab('Reorder probability') +
  xlab('Number of time ordered')

#Organic vs Non-Organic
products <- products %>% 
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), 
         organic= as.factor(organic))

order_products %>%
  left_join(products, by = 'product_id') %>%
  group_by(organic) %>%
  summarise(order_count = n()) %>%
  ggplot(aes(organic, order_count, fill = organic)) +
  geom_bar(stat = 'identity') +
  ggtitle('Organic Vs Non-Organic')

#Reordering Organic vs Non-Organic
order_products %>%
  left_join(products, by = 'product_id') %>%
  group_by(organic) %>%
  summarise(mean_reorder = mean(reordered)) %>%
  ggplot(aes(organic, mean_reorder, fill = organic)) +
  geom_bar(stat = 'identity') +
  ggtitle('Reordering Organic vs Non-Organic')

#Visualizing product offer
tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(department, by="department_id")
tmp <- tmp %>% left_join(aisle, by="aisle_id")

tmp2<- order_products %>%
       group_by(product_id) %>%
       summarise(n = n()) %>%
       left_join(products, by = 'product_id') %>%
       ungroup() %>%
       group_by(department_id, aisle_id) %>%
       summarise(sumcount = sum(n)) %>%
       left_join(tmp, by = c('department_id', 'aisle_id')) %>%
       mutate(onesize = 1)

treemap(tmp2, index = c('department', 'aisle'), vSize = 'onesize', vColor = 'department', palette = 'Set3',
        title = 'Product Offer', sortID = 'sumcount', border.col = 'white', type = 'categorical', fontsize.legend = 0,
        bg.labels = 'white')  

treemap(tmp,index=c("department","aisle"),vSize="n",title="",palette="Set3",border.col="white")

treemap(tmp2,index=c("department","aisle"),vSize="sumcount",title="",palette="Set3",border.col="white")
