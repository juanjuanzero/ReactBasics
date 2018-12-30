library(dplyr)
library(readr)
library(ggplot2)
library(purrr)

#load the dataframe
forestfires <- read_csv("forestfires.csv")

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

#get the unique identifiers for forest fires
yvars <- colnames(forestfires)[6:length(colnames(forestfires))-1]

#summarize by month and show box plot.
by_month <- function(x,y){
  dt_month <- forestfires %>% group_by(month)
  ggplot(data = dt_month) + aes_string(x=x, y =y)+geom_boxplot()
}
xvar <- "month"
monthly_look <- map2(xvar, yvars, by_month)

#summarize by day to see variability throughout the day
by_day <- function(x, fire_data){
  dt <- forestfires %>% group_by(day)
  ggplot(data = forestfires) + aes_string(x= x, y = fire_data)+geom_boxplot()
}
xvar <- "day"
daily_look <- map2(xvar, yvars, by_day)


##-PLOTING BY THE AREA-------------------------------------
#function that plots the area vs the yvar
vs_area <- function(x,y){
  ggplot(data = forestfires) + aes_string(x = x , y = y)+geom_point()
}

#get plot comparison of area vs the yvars
#updating xvar
xvar <- colnames(forestfires)[6:length(colnames(forestfires))-1]
yvars <- "area"

area_look <- map2(xvar, yvars, vs_area )

##looks like the data shows that there is a large number of fires with zero area.
#writing a function that subdivides the area parameter.
vs_area_mod <- function(x, value){
  #modify the data from
  if(value == 0){
    dt <- forestfires %>% filter(area == 0)
  } else{
    dt <- forestfires %>%filter(area > value)
  }
  
  ggplot(data = dt) + aes_string(x = x , y = "area")+geom_point()
}


##--------------------------------------------------------------
