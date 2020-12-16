#New Whatsapp project for text analysis

library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)

#1. Import chat ----
mychat<- rwa_read('chat_A_G.txt')

#2. Clean Data
summary(mychat)
str(mychat)

mychat<- mychat[-1,] #delete first raw with whatsapp privacy encoding disclaimer



mychat<- mychat %>% 
         mutate(day = date(time))%>%
         mutate(season = case_when(
                                   day >= dmy(05012017) & day <= dmy(31032017) ~ 'Winter 2017',
                                   day >= dmy(01042017) & day <= dmy(21062017) ~ 'Spring 2017',
                                   day >= dmy(22062017) & day <= dmy(23092017) ~ 'Summer 2017',
                                   day >= dmy(24092017) & day <= dmy(20122017) ~ 'Autumn 2017',
                                   day >= dmy(21122017) & day <= dmy(31032018) ~ 'Winter 2018',
                                   day >= dmy(01042018) & day <= dmy(21062018) ~ 'Spring 2018',
                                   day >= dmy(22062018) & day <= dmy(23092018) ~ 'Summer 2018',
                                   day >= dmy(24092018) & day <= dmy(20122018) ~ 'Autumn 2018',
                                   day >= dmy(21122018) & day <= dmy(31032019) ~ 'Winter 2019',
                                   day >= dmy(01042019) & day <= dmy(21062019) ~ 'Spring 2019',
                                   day >= dmy(22062019) & day <= dmy(23092019) ~ 'Summer 2019',
                                   day >= dmy(24092019) & day <= dmy(20122019) ~ 'Autumn 2019',
                                   day >= dmy(21122019) & day <= dmy(31032020) ~ 'Winter 2020',
                                   day >= dmy(01042020) & day <= dmy(21062020) ~ 'Spring 2020',
                                   day >= dmy(22062020) & day <= dmy(23092020) ~ 'Summer 2020',
                                   day >= dmy(24092020) & day <= dmy(15122020) ~ 'Autumn 2020'
                                   ))

mychat$season<- factor(mychat$season)

#3. EDA ----

#3.1 Messages per seasons ----
mychat %>% group_by(season) %>%
           count(day) %>%
           ggplot(aes(day, n, fill = season)) +
           geom_bar(stat = 'identity') +
           ylab('Numbers of messages') +
           xlab('season') +
           ggtitle('Messages per Seasons') +
           theme_minimal() +
           theme(legend.position = 'bottom')

#3.2 Messages per day of week
mychat %>% mutate(wday_num = wday(day), wday_name = weekdays(day)) %>%
           group_by(season, wday_num, wday_name) %>%
           count() %>%
           ggplot(aes(reorder(wday_name, -wday_num), n, fill = season)) +
           geom_bar(stat = 'identity') +
           xlab('') +
           coord_flip() +
           ggtitle('Messages per day of week', 'Frequency per seasons') +
           theme_minimal() +
           theme(legend.title = element_blank(), legend.position = 'bottom')

#3.3 Message frequency by the time of day ----
wdays<- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
names(wdays)<- 1:7 #Messages per day hours
mychat %>% mutate(hours = hour(time), wday_num = wday(day), wday_name = weekdays(day)) %>%
           count(season, wday_num, wday_name, hours) %>%
           ggplot(aes(hours, n, fill = season)) +
           geom_bar(stat = 'identity') +
           ylab('Number of messages') +
           xlab('Hours') +
           ggtitle('Number of messages per day hours', 'Frequency per seasons') +
           facet_wrap(~wday_num, ncol = 7, labeller = labeller(wday_num = wdays)) +
           theme_minimal() +
           theme(legend.title = element_blank(), legend.position = 'bottom', 
                 panel.spacing.x = unit(0.0, 'lines'))

#3.4 Who has sent the most messages? ----
mychat %>% mutate(day = date(time)) %>%
           group_by(season) %>%
           count(author) %>%
           ggplot(aes(reorder(author,n), n, fill = season)) +
           geom_bar(stat = 'identity') +
           ylab('Total number of messages') +
           xlab('User') +
           coord_flip() +
           ggtitle('Total number of messages per user', 'Who has sent the most messages?, Frequency per season') +
           theme_minimal() +
           theme(legend.title = element_blank(), legend.position = 'bottom')

#3.4 lenght of messages ----
mychat %>%