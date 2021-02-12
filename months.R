US_NE2_Hour <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US-MMS_BASE-BADM_18-5/AMF_US-MMS_BASE_HR_18-5.csv", comment.char="#")
setwd("D:\Research\R_Files\US-NE")
head(US_NE2_Hour)

library(lubridate)


#date ex. 199901010100 -- 1999 01 01 0100
US_NE2_HHour <- US_NE2_Hour
head(US_NE2HOUR)

#Time looks like 199901010000 (YYYYMMDDHHMM) under TIMESTAMP_START/TIMESTAMP_END
#Turn YYYYMMDDHHMM into YYYYMMDD
library("lubridate")
read.delim("US_NE2HOUR$TIMESTAMP_START")

US_NE2HOUR$TIMESTAMP_START <- as.Date(as.character(US_NE2HOUR$TIMESTAMP_START), format="%Y%m%d")


head(US_NE2HOUR)

library("lubridate")
parse_date_time("2017-05-25 20:10:05", orders = "ymd HMS")

#Turn -9999 into NAs
US_NE_Step_1[US_NE_Step_1 == -9999] <- NA

##add column for start_year and start_month; will use this for now
US_NE_Step_1[, "Start_Year"] <- format(US_NE_Step_1[,"TIMESTAMP_START"], "%Y")
US_NE_Step_1[, "Start_Month"] <- format(US_NE_Step_1[,"TIMESTAMP_START"], "%m")
US_NE_Step_1[, "Start_Day"] <- format(US_NE_Step_1[,"TIMESTAMP_START"], "%d")


##add column for end_year and end_month
US_NE_Step_1[, "End_Year"] <- format(US_NE_Step_1[,"TIMESTAMP_END"], "%Y")
US_NE_Step_1[, "End_Month"] <- format(US_NE_Step_1[,"TIMESTAMP_END"], "%m")

#Check if it worked; it did
head(US_NE_Step_1)




























library(ggplot2)
library(lubridate)
library(dplyr)

PrecipVPD <- US_NE_Step_2 #data before ET calculations
View(PrecipVPD)

# set strings as factors to false
options(stringsAsFactors = FALSE)

monthzzz <- US_NE2_ET

#Drought Years: 2010, 2012 (row # 812 - 873)
Year_2012 <- filter(monthzzz, Year == "2012")

Year_2012BeforeET <- filter(PrecipVPD, Start_Week_Year == "2012")

Year_2012BeforeET$VPD_PI_1_1_1 <- Year_2012BeforeET$VPD_PI_1_1_1 * 0.1
#need to average all variables except for precip and ET (sum those) then combine
#group by Month
Average_Months_2012 <- Year_2012BeforeET %>% group_by(Start_Month) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)

Average_Months_2012$Months <- Average_Months_2012$Start_Month

#Precip
Precip_Months_2012 <- Year_2012BeforeET %>% group_by(Start_Month) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)

#group by year (SUM) -> Sum ET 
Sum_Months_2012 <- Year_2012 %>% group_by(Month) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)

View(Precip_Months_2012)
Precip_Months_2012$Month <- Precip_Months_2012$Start_Month

GraphingMonths_2012 <- data.frame(Months = Precip_Months_2012$Month, ET = Sum_Months_2012$ETpm_inweek, VPD = Average_Months_2012$VPD_PI_1_1_1, Precip = Precip_Months_2012$P_1_1_1, Temp = Average_Months_2012$TA_1_1_1)
View(GraphingMonths_2012)






#Precip
MonthlyPrecip_2012 <- ggplot(GraphingMonths_2012, aes(x = Months, y = Precip, label="Name"))+
  geom_point(size =3, color="blue")+ #color="firebrick"
  labs(title = "US-NE2 Site: Total Precipitation (mm) (2012)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Month",
       y = "Total Precipitation (mm)")+
  theme_gray()


#Temp
MonthlyTemp_2012 <- ggplot(GraphingMonths_2012, aes(x = Months, y = Temp, label="Name"))+
  geom_point(size =3, color = "red")+ #color="firebrick"
  labs(title = "US-NE2 Site: Average Temperature (C) (2012)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Month",
       y = "Average Temperature (C)")+
  theme_gray()#+ 
 # geom_smooth(fill="blue", colour="darkblue", size=1)



#ET
ggplot(GraphingMonths_2012, aes(x = Months, y = ET, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Total ET (mm) (2012)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Month",
       y = "Total Penman-Monteith ET (mm)")+
  theme_gray()

#VPD
ggplot(GraphingMonths_2012, aes(x = Months, y = VPD, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Average VPD (2012)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Month",
       y = "VPD (kpa)")+
  theme_gray()





###################
#DAILY#


library(ggplot2)
library(lubridate)
library(dplyr)

PrecipVPD <- US_NE_Step_2 #data before ET calculations
View(PrecipVPD)

# set strings as factors to false
options(stringsAsFactors = FALSE)

monthzzz <- US_NE2_ET

#Drought Years: 2010, 2012 (row # 812 - 873)
Year_2012 <- filter(monthzzz, Year == "2012")

Year_2012BeforeET <- filter(PrecipVPD, Start_Week_Year == "2012")

Year_2012BeforeET$VPD_PI_1_1_1 <- Year_2012BeforeET$VPD_PI_1_1_1 * 0.1
#need to average all variables except for precip and ET (sum those) then combine
#group by Month
head(Year_2012BeforeET)

Average_Daily_2012 <- Year_2012BeforeET %>% group_by(Start_weekID, Start_Month) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)
View(Average_Daily_2012)

Average_Daily_2012$Month <- Average_Daily_2012$Start_Month
Average_Daily_2012$WeekID <- Average_Daily_2012$Start_weekID

#Precip
Precip_Daily_2012 <- Year_2012BeforeET %>% group_by(Start_weekID, Start_Month) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)

Precip_Daily_2012$Month <- Precip_Daily_2012$Start_Month
Precip_Daily_2012$WeekID <- Precip_Daily_2012$Start_weekID

#group by year (SUM) -> Sum ET  ###ISUE ET IS WEEKLY; week ID per month... 
Sum_Daily_2012 <- Year_2012 %>% group_by(WeekID, Month) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)



GraphingDaily_2012 <- data.frame(Month = Precip_Daily_2012$Month, WeekID = Precip_Daily_2012$Start_weekID, VPD = Average_Daily_2012$VPD_PI_1_1_1, Precip = Precip_Daily_2012$P_1_1_1, Temp = Average_Daily_2012$TA_1_1_1)
View(GraphingDaily_2012)

#Graph ET from Sum_Daily_2012$ETpm_mmday





#Precip
DailyPrecip_2012 <- ggplot(GraphingDaily_2012, aes(x = WeekID, y = Precip, label="Name"))+
  geom_point(size =3, color="blue")+ #color="firebrick"
  labs(title = "US-NE2 Site: Total Precipitation (mm) (2012)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Weeks",
       y = "Total Weekly Precipitation (mm)")+
  theme_gray()
DailyPrecip_2012

#Temp
DailyTemp_2012 <- ggplot(GraphingDaily_2012, aes(x = WeekID, y = Temp, label="Name"))+
  geom_point(size =3, color = "red")+ #color="firebrick"
  labs(title = "US-NE2 Site: Average Temperature (C) (2012)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Weeks",
       y = "Average Temperature (C)")+
  theme_gray()#+ 
# geom_smooth(fill="blue", colour="darkblue", size=1)
DailyTemp_2012

head(Sum_Daily_2012)
#ET
WeekID_ET_2012 <- ggplot(Sum_Daily_2012, aes(x = WeekID, y = ETpm_mmday, label="Name"))+
  geom_point(size =3, color = "darkgreen")+ #color="firebrick"
  labs(title = "US-NE2 Site: Total Weekly ET (mm) (2012)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Weeks",
       y = "Total Penman-Monteith ET (mm)")+
  theme_gray()

WeekID_ET_2012

#VPD
weekly_VPD_2012 <- ggplot(GraphingDaily_2012, aes(x = WeekID, y = VPD, label="Name"))+
  geom_point(size =3, color = "orange")+ #color="firebrick"
  labs(title = "US-NE2 Site: Average VPD (2012)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Weeks",
       y = "VPD (kpa)")+
  theme_gray()

weekly_VPD_2012



