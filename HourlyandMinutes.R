#For Hourly data... try...
library(tidyverse)
library(lubridate)

timeattempt <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US-MMS_BASE-BADM_18-5/AMF_US-MMS_BASE_HR_18-5.csv", comment.char="#")


t <- tibble(id = timeattempt, date = timeattempt$TIMESTAMP_START)
t


t2 <- t %>% mutate(date = as.character(date), date = as_datetime(date, format = "%Y%m%d%H%M"), year = year(date), month = month(date), day = day(date), hour = hour(date), minute = minute(date))
  
t2
 
View(t2)

tail(t2)
str(t2)
