#Step 1: determine drought years by plotting annual total precipitation at each site (should be 2010 and 2012)
#Step 1A: determine drought months by plotting monthly total precipitation at each site
#Step 1B: determine drought days by plotting daily total precipitation at each site; if this is unhelpful; try daily temperature then select max temp
#Step 2: Calculate hourly ET and VPD at each site
#Step 3: Plot hourly ET and VPD
#Step 4: Plot daily max ET and Rn; see what time this occurs; how do they interact with each other?


#US-NE3: Mead - rainfed maize-soybean rotation site 
#(https://ameriflux.lbl.gov/sites/siteinfo/US-ne3) 

#install.packages("raster")
#install.packages("rgdal")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyverse")

library("ggplot2")
library("dplyr")
library("raster")
library ("lubridate")
library("rgdal")
library("tidyverse")


#general path :  C:\Users\keyke\OneDrive - Indiana University\Drought_Research_Data\US-NE
#US-NE3 -> Rainfed maize site
AMF_US.Ne3_BASE_HR_10.5 <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Drought_Research_Data/US-NE/US-NE3/AMF_US-Ne3_BASE-BADM_10-5/AMF_US-Ne3_BASE_HR_10-5.csv", comment.char="#")
US_NE3 <- AMF_US.Ne3_BASE_HR_10.5

View(US_NE3)
#Fixing Time columns and adding columns for each year, month, day, day of week, hour, minute for start and end times

#this adds columns as well
library(tidyverse)
library(lubridate)
library(dplyr)
US_NE3_Date <- US_NE3 %>%
  mutate(TIMESTAMP_START = ymd_hm(TIMESTAMP_START), 
         start_day = day(TIMESTAMP_START),
         start_month = month(TIMESTAMP_START), 
         start_year = year(TIMESTAMP_START),
         start_dayofweek = wday(TIMESTAMP_START),
         start_hour = hour(TIMESTAMP_START),
         start_minute = minute(TIMESTAMP_START),
         start_second = second(TIMESTAMP_START))

US_NE3_Date1 <- US_NE3_Date %>%
  mutate(TIMESTAMP_END = ymd_hm(TIMESTAMP_END), 
         end_day = day(TIMESTAMP_END),
         end_month = month(TIMESTAMP_END), 
         end_year = year(TIMESTAMP_END),
         end_dayofweek = wday(TIMESTAMP_END),
         end_hour = hour(TIMESTAMP_END),
         end_minute = minute(TIMESTAMP_END),
         end_second = second(TIMESTAMP_END))

US_NE3_Date2 <- US_NE3_Date1

#Clean up data a bit; turn -9999 into NAs
US_NE3_Date2[US_NE3_Date2 == -9999] <- NA


#Calculate ET using Qing's Code:

#Set Latitude
#latitude for US-NE3: Lat	41.1797, Long -96.4397
lat <- 41.1797
US_NE3_Date2$lat <- lat

head(US_NE3_Date2)
US_NE3_Data <- US_NE3_Date2

#LE             
US_NE3_Data$LE <- US_NE3_Data$LE_1_1_1 # w/m2 Latent heat turbulent flux (no storage correction)
US_NE3_Data$LE[US_NE3_Data$LE <0] <- 0 #PET is zero

#TA             (deg C): Air temperature
US_NE3_Data$Tem <- US_NE3_Data$TA_1_1_1  #temperature in degree C

#USTAR          (m s-1): Friction velocity
US_NE3_Data$ustar <- US_NE3_Data$USTAR_1_1_1 #Friction velocity, m s-1

#H              (W m-2): Sensible heat flux
US_NE3_Data$Hs <- US_NE3_Data$H_1_1_1 #Sensible heat turbulent flux (no storage correction); W m-2

#NETRAD         (W m-2): Net radiation
US_NE3_Data$Rn <- US_NE3_Data$NETRAD_1_1_1  #net radiation; W m-2


#P              (mm): Precipitation
US_NE3_Data$Pre <- US_NE3_Data$P_PI_F_1_1_1 * 7 * 0.0394 #Precipitation inch/7day; _PI (Provided by PI / tower team)
#: _PI indicates a variable that has been QA/QC filtered, spatially-aggregated, or calculated by the tower team.
#Details: _PI_F indicates that the variable has been gap-filled by the tower team.

#WS             (m s-1): Wind speed
US_NE3_Data$U <- US_NE3_Data$WS_1_2_1 #Wind speed; ws_1_1_1 is missing lots of datapoints, will use ws_1_2_1 but must verify if this is ok!


#NO VPD DATA; Calculate VPD:
#VPD            (hPa or kpa; check note - assuming kpa for now): Vapor Pressure Deficit                     ##########SEE NOTE#######
#US_NE3_Data$VPD <- # .... #* 0.1 #from hPa to Kpa #Vapor Pressure Deficit, hPa; ###For all files with processing version 1, VPD_PI is provided in the units kPa (instead of hPa). I'm not sure which version US-MMS has
#no VPD data?
US_NE3_Data$RH <- US_NE3_Data$RH_1_1_1 #relative humidity (%); US_NE3_Data$Tem (temp(c))

#from stack overflow, the The ASCE Standardized Reference Evapotranspiration Equation is:
#https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-pressure-deficit-from-temperature-and-relative-humidit
T <- US_NE3_Data$Tem #temp in C
RH <- US_NE3_Data$RH #relative humidity (%) 

#Saturation Vapor Pressure (es)
es <- 0.6108 * exp((17.27 * T) / (T + 237.3))
#actual VPD
vpd <- (es*(100-RH))/100

#Calculate VPD
#VPD <- ea - es #hPA, I think but need to verify
#US_NE3_Data$VPD <- VPD
#results in VPD from around -5 to 0

#air VPD -https://pulsegrow.com/blogs/learn/vpd#:~:text=VPD%20%3D%20SVP%20%E2%80%93%20AVP,called%20the%20Vapor%20Pressure%20Deficit.
#svp <- 610.78 * exp(T / ((T + 238.3)*17.2694))
#vpd <- svp * (1 - (RH/100))
US_NE3_Data$VPD <- vpd



#Constants ------- some of these constants (except for elev) are based on the morgan monroe forest from a paper kim wrote; not sure if these are okay for the US-NE sites?
Elev <- 	363 #in m, elevation data for air pressure calculation
zm <- 46 #(measurement height for wind speed and humidity)
h <- 27 #canopy height
zd <- 2/3 * h #zd is zero plane displacement height 
zo <- 0.1 * h #roughness length governing momentum transfer and governing transfer of heat and vapour 

#Convert LE to ET
Lv <- 2.500 * 10^6 - 2.386 *10^3*US_NE3_Data$Tem  #J/kg   #this is the latent heat of vaporization, Allen 1998 2.45 MJ/m^3
ETlv_mmday <- 86400 * US_NE3_Data$LE / Lv #kg/m2/s = 86400mm/day ET calculated directly from LE
ETlv_inweek <- ETlv_mmday * 7 * 0.03937 #inch/week, 7 days a week
US_NE3_Data$ETlv_mmday <- ETlv_mmday
US_NE3_Data$ETlv_inweek <- ETlv_inweek 

#Define Constants for ET calculation
#define some constants necessary for the Penman-Monteith Equation, a few of which depend on temperature
cp <- 1006 #specific heat capacity of dry air, J/kg/?C.  Cp = 1.013 10-3 [MJ kg-1 ?C-1]
rho_w <- 1000 #density of water, kg/m^3
k <- 0.4 #von Karman constant, unitless
rho_a <- 101.3*10^3/(287.058) /(US_NE3_Data$Tem+273.15) #density of dry air kg/m^3  #287.058 ideal gas law: dry air density = P/ (specific gas constant * Tk) 
#S <- 2508/(US_NE3_Data$Tem+237.3)^2 * exp(17.3*US_NE3_Data$Tem/(df_AMF_USMMSTem+237)) #slope of saturation water vapor function, #kPa/KSTst
delta <-  4098*0.6108*exp(17.27*US_NE3_Data$Tem/(US_NE3_Data$Tem+237.3))/(US_NE3_Data$Tem+237.3)^2 #slope of saturation water vapor function, #kPa/K 
pres <- 101.3 * (((293-0.0065*Elev)/293)^5.26)#air pressure, kPa
gamma <- cp * pres/(0.622*Lv)  #psychrometric constant, kPa/K,cp: 1.013 10-3 [MJ kg-1 ?C-1],
Ta_K <- US_NE3_Data$Tem + 273.15 #air temp in kelvin
Rho <- (1.3079 - 0.0045*US_NE3_Data$Tem)  #density of dry air kg m-3
Cp <- 1005 #specific heat capacity of air, J/kg/?C


#Correction parameter for ga
#find the aerodynamic conductance 
#OL is Monin-Obhukov lengh #stab is the atmospheric stability facture
#consult SI of Novick et al. 2016 (Nature Climate Change) for details
eps <- 0.1^6
OL <- -Rho * (US_NE3_Data$ustar + eps)^3 /(0.4*9.81*((US_NE3_Data$Hs + eps)/(Cp*Ta_K))) #the ep is a very small number, #which prevents ustart and H from being exactly zero, which mucks up the calculations
stab <- (zm-2/3*zd) / OL
psiH <- 6*log(1+stab) #atmospheric stability correction
if (length(psiH) == length(stab)) {
  psiH[which(stab <0)] <- -2*log((1+(1-16*stab[which(stab<0)])^0.5)/2)
} else {
  print ("check the dataset")
}


#ga (m/s), Gs (m/s) calculation
#atmospheric stability correction
ga <- US_NE3_Data$U * k^2 /((log((zm-zd)/zo) + psiH)^2) #m/s
#ga = NAs....

#Finally, find the reference surface conductance by inverting the penman monteith equation
Gs <- gamma*US_NE3_Data$LE*ga / (delta*US_NE3_Data$Rn + rho_a*cp*ga*US_NE3_Data$VPD - US_NE3_Data$LE*(delta+gamma))                     
US_NE3_Data$Gs <- Gs
#Gs = nothing 

#####Error in `$<-.data.frame`(`*tmp*`, Gs, value = numeric(0)) : 
#replacement has 0 rows, data has 175320


#Peman-Monteith PET calculation
dfref <- US_NE3_Data[abs(US_NE3_Data$VPD - 1) < 0.05,]
if (nrow(dfref) == 0) {
  print("check condition setting,no data at VPD~1kpa")
} else {
  Gsref <- mean(dfref$Gs,na.rm = TRUE)
}
US_NE3_Data$Gsref <- Gsref



#daily PET kg/m2/s = 86400mm/day
PETpm_mmday <- 86400 * ( delta*US_NE3_Data$Rn + rho_a*cp*ga*US_NE3_Data$VPD ) / (( delta + gamma*(1+(ga/Gsref))) * Lv)                             
US_NE3_Data$PETpm_mmday <- PETpm_mmday #P-M PET mm/day

#weekly PET in inch (1mm = 0.0393701 inch)
PETpm_inweek <- PETpm_mmday * 7 * 0.03937 
US_NE3_Data$PETpm_inweek <- PETpm_inweek #P-M PET inch/week

#Prestley-Tylor PET
PETpt_mmday <- 1.26 * delta*US_NE3_Data$Rn / (Lv * (delta + gamma)) * 86400
US_NE3_Data$PETpt_mmday <- PETpt_mmday # Priestley-Taylor PET mm/day
PETpt_inweek <- PETpt_mmday * 7 * 0.03937
US_NE3_Data$PETpt_inweek <- PETpt_inweek # Priestley-Taylor PET inch/week

#Calculate Penman-Monteith ET -------------------------------------------------------------- These don't work for some reason :( )
ETpm_mmday <- 86400 * (delta*US_NE3_Data$Rn + rho_a*cp*ga*US_NE3_Data$VPD) / (( delta + gamma*(1+(ga/Gs))) * Lv)
ETpm_inday <- ETpm_mmday * 0.0393701 #inch/day
ETpm_inweek <- ETpm_mmday * 7 * 0.03937 #inch/week

US_NE3_Data$ETpm_mmday <- ETpm_mmday
US_NE3_Data$ETpm_inday <- ETpm_inday
#Error in `$<-.data.frame`(`*tmp*`, PETpm_mmday, value = numeric(0)) : 
#replacement has 0 rows, data has 175320
US_NE3_Data$ETpm_inweek <- ETpm_inweek 

#some errors here but it looks like theres still data
write.csv(US_NE3_Data, file = "C:/Users/keyke/OneDrive - Indiana University/Drought_Research_Data/US-NE/US-NE3/AMF_US-Ne3_BASE-BADM_10-5/US_NE3_Data",row.names = FALSE)

US_NE3_Data <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Drought_Research_Data/US-NE/US-NE3/AMF_US-Ne3_BASE-BADM_10-5/US_NE3_Data")

head(US_NE3_Data)

#locate drought years
#need to average all variables except for precip and ET (sum those) then combine
#group by Month


#Precip (Pre) #sum total precipitation by year
Sum_Years_US_NE3_Data <- US_NE3_Data %>% group_by(start_year) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)
head(Sum_Years_US_NE3_Data)


precip_year <- data.frame(Year = Sum_Years_US_NE3_Data$start_year, Precip = Sum_Years_US_NE3_Data$Pre)
View(precip_year)
#lowest precip years for the data from US-NE3
#2001: 80.19244 mm
#2020: 103.28710 mm
#2012: 117.90450 mm

#is there enough data for 2001 and 2020 to declare a low precip year?
#US_NE3_Data
#how many data points for precip and year: 2001

#this gives the counts of data for precip per year while excluding NA's (I think)
library(dplyr)
US_NE3_Data %>% 
  group_by(start_year) %>% 
  summarise_at(vars(starts_with("Pre")), ~sum(!is.na(.)))
#the number of datapoints for precip per year averages around 8700; 2001 only has 4848 points so we can't say the precip count for that is accurate 

#Based only on total precip from this site; the possible drought years are 2020 and 2012
#some quick random articles look like this might be accurate
#removing 2001 because of this
test <- US_NE3_Data
head(test)
US_NE3_Data_2002_2020 <- subset(test, start_year!="2001")
View(US_NE3_Data_2002_2020)
#it worked! removes all data for 2001; data is now 2002 - 2020


#Average per year
mean_year_US_NE3_Data_2002_2020 <- US_NE3_Data_2002_2020 %>% group_by(start_year) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)

#Sum per year
sum_year_US_NE3_Data_2002_2020 <- US_NE3_Data_2002_2020 %>% group_by(start_year) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)
head(sum_year_US_NE3_Data_2002_2020)

#Precip
total_precip_year <- ggplot(sum_year_US_NE3_Data_2002_2020, aes(x = start_year, y = Pre, label="Name"))+
  geom_line(color="blue") +
  geom_point(size =3, color="blue")+ #color="firebrick"
  labs(title = "US-NE3 Site: Total Precipitation (mm)",
       subtitle = "Rainfed Maize-Soybean Site",
       x = "Year",
       y = "Total Precipitation (mm)")+
  theme_gray()
#

#Step 1: determine drought years by plotting annual total precipitation at each site
#2020 and 2012

#Step 1A: determine drought months by plotting monthly total precipitation at each site
#filter for 2020
test <- US_NE3_Data
US_NE3_Data_2020 <- subset(US_NE3_Data, start_year == 2020)
View(test1)
#originally this didnt have any ET data when ws_1_1_1 was used but this problem is resolved with using ws_1_2_1
#do any of the ws variables have much for 2020?
#ws_2020 <- data.frame(date = test1$TIMESTAMP_START, Year = test1$start_year, WS_1_1_1 = test1$WS_1_1_1, WS_1_2_1 = test1$WS_1_2_1, WS_1_3_1 = test1$WS_1_3_1, WS_2_1_1 = test1$WS_2_1_1)
#View(ws_2020)

#test1$WD_1_1_1
#HUUUUUGE differences! WS_1_1_1 has all NAs but the others have lots of data; ws_1_2_1 has most of it. Switching ET calculate to use WS_1_2_!
#IT WORKED

#2020 year is US_NE3_Data_2020 

sum_months_US_NE3_Data_2020 <- US_NE3_Data_2020 %>% group_by(start_month) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)
View(sum_months_US_NE3_Data_2020)

#precip per month
precip_months_2020 <- data.frame(month = sum_months_US_NE3_Data_2020$start_month, Precip = sum_months_US_NE3_Data_2020$Pre)
View(precip_months_2020)
#shows february and april as least amount of precip with june having the highest

#want to compare with average temp
average_months_US_NE3_Data_2020 <- US_NE3_Data_2020 %>% group_by(start_month) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)

precip_months_2020 <- data.frame(month = sum_months_US_NE3_Data_2020$start_month, Precip = sum_months_US_NE3_Data_2020$Pre, Temp = average_months_US_NE3_Data_2020$Tem)
View(precip_months_2020)
#june july are hottest months with the highest precipitation about
#HMMMMMMM


#moving on to 2012 cuz idk....
test <- US_NE3_Data
View(US_NE3_Data)
US_NE3_Data_2012 <- subset(test, start_year == 2012)
View(US_NE3_Data_2012)

#general 2012 data is US_NE3_Data_2012 

sum_months_US_NE3_Data_2012 <- US_NE3_Data_2012 %>% group_by(start_month) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)
View(sum_months_US_NE3_Data_2012)

#precip per month
precip_months_2012 <- data.frame(month = sum_months_US_NE3_Data_2012$start_month, Precip = sum_months_US_NE3_Data_2012$Pre)
View(precip_months_2012)
#lowest precip january and july

#want to compare with average temp
average_months_US_NE3_Data_2012 <- US_NE3_Data_2012 %>% group_by(start_month) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)

precip_months_2012 <- data.frame(month = sum_months_US_NE3_Data_2020$start_month, Precip = sum_months_US_NE3_Data_2020$Pre, Temp = average_months_US_NE3_Data_2012$Tem)
View(precip_months_2012)
#higher avg temp and low precip for july; high temp and precip for june
#will focus on July 2012 for highest average temperature of the year; the lowest precip is for january which is one of the coldest months.. hm

#subset july 2012

test <- US_NE3_Data_2012
July_US_NE3_Data_2012 <- subset(test, start_month == 7)
View(July_US_NE3_Data_2012)

#july 2012: July_US_NE3_Data_2012

sum_daily_July_US_NE3_Data_2012 <- July_US_NE3_Data_2012 %>% group_by(start_day) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE)
View(sum_daily_July_US_NE3_Data_2012)

#want to compare with average temp
average_daily_July_US_NE3_Data_2012 <- July_US_NE3_Data_2012 %>% group_by(start_day) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)
View(average_daily_July_US_NE3_Data_2012)


July_precip_temp_daily_2012 <- data.frame(day = sum_daily_July_US_NE3_Data_2012$start_day, Precip = sum_daily_July_US_NE3_Data_2012$Pre, Temp = average_daily_July_US_NE3_Data_2012$Tem)
View(July_precip_temp_daily_2012)
#18th and 19th look like lower precip and higher temp days
#18th: 2.564940 mm precip; 25.73806 C  ---- only about 78F.... hmmm 
#19th: 8.191260 mm precip; 26.09208 C
#lowest precip day: 10th: 2.151240 mm precip; 24.74054 C

###Step 1B: determine drought days by plotting daily total precipitation at each site; if this is unhelpful; try daily temperature then select max temp
#For now, let's go with July 18th, 2012

#Step 2: Calculate hourly ET and VPD at each site
#Step 3: Plot hourly ET and VPD
#I have daily ET and VPD, not certain how to do hourly

#Step 4: Plot daily max ET and Rn; see what time this occurs; how do they interact with each other?
#okay, for july 2012... 
head(July_US_NE3_Data_2012)
library(dplyr)
max_rn <- July_US_NE3_Data_2012 %>% 
  group_by(start_day) %>%
  filter(Rn == max(Rn)) %>%
  arrange(Rn)
View(result)
View(July_US_NE3_Data_2012)
#this should give max Rn Value for each day
#688.3 is highest in data for 26th; same for in the results data

rn_July_2012 <- data.frame(Day = max_rn$start_day, hour = max_rn$start_hour, Max_Rn = max_rn$Rn, ETpm_inday=max_rn$ETpm_inday)
View(rn_July_2012)
#max Rn occurs from 10am to 12pm

View(July_US_NE3_Data_2012)

#maybe ETpm_inday? while it is inches per day, it changes based on the hour; so maybe it's actually hourly ET????


#for some reason, this is only working for a few of the days; ET data has a number for each row, even if its "o"s
head(July_US_NE3_Data_2012)
#maybe ETpm_mmday? same issue; only gives 11 days
library(dplyr)
max_ET <- July_US_NE3_Data_2012 %>% 
  group_by(start_day) %>%
  filter(ETpm_mmday == max(ETpm_mmday)) %>%
  arrange(ETpm_mmday)
View(result)
View(July_US_NE3_Data_2012)
#this should give max Rn Value for each day
#688.3 is highest in data for 26th; same for in the results data





#How to fix daily ET:
#Hey Kesondra,

#Yes, you can make it to hourly ET. Just use hourly parameters in all your calculations. And then when you calculate ET, make sure to use “86400 / 24” for hourly instead of using “86400” in the following two lines, since there are 24 hours a day. Just feel free to let me know if you still have any questions. 

#Here, 86400 is the number of total seconds in day. 
#ETlv_mmday <- 86400 * df_PM$LE / Lv #kg/m2/s = 86400mm/day ET calculated directly from LE
#ETpm_mmday <- 86400 * (delta*df_PM$Rn + rho_a*cp*ga*df_PM$VPD) / (( delta + gamma*(1+(ga/Gs))) * Lv)

#-Qing














ggplot(data = US_NE3_Data, aes(x = US_NE3_Data$WeekID, y = US_NE3_Data$ETpm_inday)) +
  geom_line()


Weekly_ET_2000 <- df_PM %>%
  ggplot(mapping = aes(x = WeekID, y = ETpm_inweek, color = Month)) +
  geom_line()

Weekly_ET_2000


