library(dplyr)
library(readr)
library(ggplot2)

#display forest fires by month
dt <- forestfires %>% group_by(month) %>% summarise(total_fires = n())
ggplot(data = dt) + aes(x= month, y = total_fires)+geom_bar(stat="identity") #stat identity makes the bar height to the value of the y variable.


#display forest fires by day
dt <- forestfires %>% group_by(day) %>% summarise(total_fires = n())
ggplot(data = dt) + aes(x= day, y = total_fires)+geom_bar(stat="identity") #stat identity makes the bar height to the value of the y variable.

#convert to factor, a factor is like a category or a classifier set as a vector of characters.
forestfires <- forestfires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

forestfires <- forestfires %>% mutate(day = factor(day, levels = c("sun", "mon", "tue","wed", "thu","fri","sat")))