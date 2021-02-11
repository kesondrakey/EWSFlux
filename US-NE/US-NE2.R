#US-NE2 (https://ameriflux.lbl.gov/sites/siteinfo/US-Ne2) 
#irrigated maize-soybean rotation site
#2001 - 2019

library("ggplot2")
library("dplyr")
library("raster")
library ("lubridate")
library("rgdal")
library("tidyverse")

#Step 1. Open Data

US_NE_Step_1 <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US-MMS_BASE-BADM_18-5/AMF_US-MMS_BASE_HR_18-5.csv", comment.char="#")
setwd("D:\Research\R_Files\US-NE")
head(US_NE_Step_1)


#Step 2. Clean up the data
#Fix Time issues

#Time looks like 199901010000 (YYYYMMDDHHMM) under TIMESTAMP_START/TIMESTAMP_END
#Turn YYYYMMDDHHMM into YYYYMMDD
US_NE_Step_1$TIMESTAMP_START <- as.Date(as.character(US_NE_Step_1$TIMESTAMP_START), format="%Y%m%d")
US_NE_Step_1$TIMESTAMP_END <- as.Date(as.character(US_NE_Step_1$TIMESTAMP_END), format = "%Y%m%d")

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


#add columns for weeks
##add column for start week year and week ID for each year
#Data starts on 01-01-1999 starts on a Friday; not sure how to fix this yet ####

US_NE_Step_1[, "Start_Week_Year"] <- format(US_NE_Step_1[,"TIMESTAMP_START"], "%G")
US_NE_Step_1[, "Start_weekID"] <- format(US_NE_Step_1[,"TIMESTAMP_START"], "%V")

#Set Latitude
lat <- 41.1648700
US_NE_Step_1$lat <- lat

US_NE_Step_2 <- US_NE_Step_1

head(US_NE_Step_2)

#
Yearly_US_NE2 <- US_NE_Step_2 %>% group_by(Start_Week_Year) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)
head(Yearly_US_NE2)

#Find Drought years; plot Precipitation (P_1_1_1) (mm) per year; 2010 and 2012 look like drought years
###how much yearly precip (mm) results in a "drought" year?

g<-ggplot(Yearly_US_NE2, aes(x = Start_Week_Year, y = P_1_1_1, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Average Precipitation (mm) (1998-2020)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Year",
       y = "Average Precipitation (mm)")+
 #theme_dark()+
  theme_gray()+
  geom_text(aes(label=ifelse(P_1_1_1<0.1,as.character(" < 0.1mm"),'')),hjust=0,vjust=0)
#+
 # geom_smooth(method = "lm") #attempt at adding a trendline: failed
g



#Want to see how many datapoints there are per year
library(plyr)
View(US_NE_Step_2)
count(US_NE_Step_2, "Start_Week_Year", "P_1_1_1")
#what's happening???

#make new dataframe to count how many points there are for precip
count_year_precip <- data.frame(Year =US_NE_Step_2$Start_Week_Year, Precip = US_NE_Step_2$P_1_1_1)
View(count_year_precip)

count(count_year_precip, "Year")
#freq looks good! 1998 only has 72 counts of precip data though



#Look at ET for this too; ggplot code on line #312
et_1999_2014


#Look at VPD for this too; VPD_PI_1_1_1 hpa or kpa?
ggplot(Yearly_US_NE2, aes(x= Start_Week_Year, y= P_1_1_1, colour="green", label="Name")) + 
  geom_point(size = 2,alpha = 0.6) +
  theme_bw()+
  geom_text(aes(label=ifelse(P_1_1_1>.26,as.character(Name),'')),hjust=0,vjust=0)

#VPD from the averaged raw data
VPD1 <- ggplot(Yearly_US_NE2, aes(x = Start_Week_Year, y = VPD_PI_1_1_1, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Average VPD (1999-2014)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Year",
       y = "VPD (hpa or kpa?)")+
  #theme_dark()+
  theme_gray()#+
VPD1


#VPD from after calculating ET
VPD <- ggplot(ET_1999_2014, aes(x = Year, y = VPD, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Average VPD (1999-2014)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Year",
       y = "VPD (hpa or kpa?)")+
  #theme_dark()+
  theme_gray()#+
VPD



###CALCULATE ET###

#Take the weekly averages of each variable for each year and month
Weekly_US_NE2 <- US_NE_Step_2 %>% group_by(Start_Week_Year, Start_weekID, Start_Month) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)
head(Weekly_US_NE2)
#includes 1998 since 01-01-1999 and 01-02-1999 are a friday and saturday
str(Weekly_US_NE2)


#Step 3. Calculate ET; Ameriflux variables are explained here: (https://ameriflux.lbl.gov/data/aboutdata/data-variables/#base)
#Our Relevant Data

#LE          (W m-2): Latent heat flux 
#TA          (deg C): Air temperature;                   Qing's is Ta
#PA          (kPa): Atmospheric pressure;                Qing's is pres
#USTAR       (m s-1): Friction velocity;                 Qing's is ustar
#H           (W m-2): Sensible heat flux
#NETRAD      (W m-2): Net radiation;                     Qing's is Rn
#VPD         (hPa): Vapor Pressure Deficit; 1hpa = 100pa
#P           (mm): Precipitation
#WS          (m s-1): Wind speed

#For now, we will use _1_1_1 data since I'm not sure which height is the best
# LE ->       LE_1_1_1
#TA ->        TA_1_1_1 
#PA ->        PA_1_1_1
#USTAR ->     USTAR_1_1_1
#H ->         H_1_1_1
#NETRAD ->    NETRAD_1_1_1 
#VPD ->       VPD_PI_1_1_1; *For all files with processing version 1, VPD_PI is provided in the units kPa (instead of hPa) (https://ameriflux.lbl.gov/data/data-change-log/) ### I'm unsure what level processing this data has, hmm. 
#VPD_PI is ranged from 0-20?

#P ->         P_1_1_1
#WS ->        WS_1_1_1 

head(Weekly_US_NE2)

lat <- 41.1648700
Weekly_US_NE2$lat <- lat




##Tem is degree C (not be used in PM-PET calculation); no latitude column, latitude for US-bo1 is 40.0062
df_US_NE2 <- data.frame(Lat = Weekly_US_NE2$lat, Year = Weekly_US_NE2$Start_Week_Year, Month = Weekly_US_NE2$Start_Month, WeekID = Weekly_US_NE2$Start_weekID) 
head(df_US_NE2)

#unsure how to add the timestamp variable back; got an error when I tried
#We will use the start dates for time

#LE             
df_US_NE2$LE <- Weekly_US_NE2$LE_1_1_1 # w/m2 Latent heat turbulent flux (no storage correction)
df_US_NE2$LE[df_US_NE2$LE <0] <- 0 #PET is zero

#TA             (deg C): Air temperature
df_US_NE2$Tem <- Weekly_US_NE2$TA_1_1_1  #temperature in degree C

#USTAR          (m s-1): Friction velocity
df_US_NE2$ustar <- Weekly_US_NE2$USTAR_1_1_1 #Friction velocity, m s-1

#H              (W m-2): Sensible heat flux
df_US_NE2$Hs <- Weekly_US_NE2$H_1_1_1 #Sensible heat turbulent flux (no storage correction); W m-2

#NETRAD         (W m-2): Net radiation
df_US_NE2$Rn <- Weekly_US_NE2$NETRAD_1_1_1  #net radiation; W m-2

#VPD            (hPa or kpa; check note - assuming kpa for now): Vapor Pressure Deficit                     ##########SEE NOTE#######
df_US_NE2$VPD <- Weekly_US_NE2$VPD_PI_1_1_1 #* 0.1 #from hPa to Kpa #Vapor Pressure Deficit, hPa; ###For all files with processing version 1, VPD_PI is provided in the units kPa (instead of hPa). I'm not sure which version US-MMS has

#P              (mm): Precipitation
df_US_NE2$Pre <- Weekly_US_NE2$P_1_1_1 * 7 * 0.0394 #Precipitation inch/7day

#WS             (m s-1): Wind speed
df_US_NE2$U <- Weekly_US_NE2$WS_1_1_1 #Wind speed


#Constants
Elev <- 362 #in m, elevation data for air pressure calculation
zm <- 46 #(measurement height for wind speed and humidity)
h <- 27 #canopy height
zd <- 2/3 * h #zd is zero plane displacement height 
zo <- 0.1 * h #roughness length governing momentum transfer and governing transfer of heat and vapour 



#Convert LE to ET
Lv <- 2.500 * 10^6 - 2.386 *10^3*df_US_NE2$Tem  #J/kg   #this is the latent heat of vaporization, Allen 1998 2.45 MJ/m^3
ETlv_mmday <- 86400 * df_US_NE2$LE / Lv #kg/m2/s = 86400mm/day ET calculated directly from LE
ETlv_inweek <- ETlv_mmday * 7 * 0.03937 #inch/week, 7 days a week
df_US_NE2$ETlv_mmday <- ETlv_mmday
df_US_NE2$ETlv_inweek <- ETlv_inweek 


#Define Constants for ET calculation
#define some constants necessary for the Penman-Monteith Equation, a few of which depend on temperature
cp <- 1006 #specific heat capacity of dry air, J/kg/?C.  Cp = 1.013 10-3 [MJ kg-1 ?C-1]
rho_w <- 1000 #density of water, kg/m^3
k <- 0.4 #von Karman constant, unitless
rho_a <- 101.3*10^3/(287.058) /(df_US_NE2$Tem+273.15) #density of dry air kg/m^3  #287.058 ideal gas law: dry air density = P/ (specific gas constant * Tk) 
#S <- 2508/(df_US_NE2$Tem+237.3)^2 * exp(17.3*df_US_NE2$Tem/(df_US_NE2Tem+237)) #slope of saturation water vapor function, #kPa/KSTst
delta <-  4098*0.6108*exp(17.27*df_US_NE2$Tem/(df_US_NE2$Tem+237.3))/(df_US_NE2$Tem+237.3)^2 #slope of saturation water vapor function, #kPa/K 
pres <- 101.3 * (((293-0.0065*Elev)/293)^5.26)#air pressure, kPa
gamma <- cp * pres/(0.622*Lv)  #psychrometric constant, kPa/K,cp: 1.013 10-3 [MJ kg-1 ?C-1],
Ta_K <- df_US_NE2$Tem + 273.15 #air temp in kelvin
Rho <- (1.3079 - 0.0045*df_US_NE2$Tem)  #density of dry air kg m-3
Cp <- 1005 #specific heat capacity of air, J/kg/?C


#Correction parameter for ga
#find the aerodynamic conductance 
#OL is Monin-Obhukov lengh #stab is the atmospheric stability facture
#consult SI of Novick et al. 2016 (Nature Climate Change) for details
eps <- 0.1^6
OL <- -Rho * (df_US_NE2$ustar + eps)^3 /(0.4*9.81*((df_US_NE2$Hs + eps)/(Cp*Ta_K))) #the ep is a very small number, #which prevents ustart and H from being exactly zero, which mucks up the calculations
stab <- (zm-2/3*zd) / OL
psiH <- 6*log(1+stab) #atmospheric stability correction
if (length(psiH) == length(stab)) {
  psiH[which(stab <0)] <- -2*log((1+(1-16*stab[which(stab<0)])^0.5)/2)
} else {
  print ("check the dataset")
}



#ga (m/s), Gs (m/s) calculation
#atmospheric stability correction
ga <- df_US_NE2$U * k^2 /((log((zm-zd)/zo) + psiH)^2) #m/s

#Finally, find the reference surface conductance by inverting the penman monteith equation
Gs <- gamma*df_US_NE2$LE*ga / ( delta*df_US_NE2$Rn + rho_a*cp*ga*df_US_NE2$VPD - df_US_NE2$LE*(delta+gamma) )                     
df_US_NE2$Gs <- Gs
#Gs m/s
#gamma kPa/K
#LE W/m2 
#ga m/s
#delta kPa/K
#Rn W/m2
#rho_a kg/m^3
#cp j/kg/K  1j/kg/K = 1j/kg/C
#VPD kpa
#Lv J/kg
#w/m^2 = 1 J/m^2/s= 0.0864*10^6/(24*60*60) J m-2 s-1

#Peman-Monteith PET calculation
dfref <- df_US_NE2[abs(df_US_NE2$VPD - 1) < 0.05,]
if (nrow(dfref) == 0) {
  print("check condition setting,no data at VPD~1kpa")
} else {
  Gsref <- mean(dfref$Gs,na.rm = TRUE)
}
df_US_NE2$Gsref <- Gsref

#daily PET kg/m2/s = 86400mm/day
PETpm_mmday <- 86400 * ( delta*df_US_NE2$Rn + rho_a*cp*ga*df_US_NE2$VPD ) / (( delta + gamma*(1+(ga/Gsref))) * Lv)                             
df_US_NE2$PETpm_mmday <- PETpm_mmday #P-M PET mm/day

#weekly PET in inch (1mm = 0.0393701 inch)
PETpm_inweek <- PETpm_mmday * 7 * 0.03937 
df_US_NE2$PETpm_inweek <- PETpm_inweek #P-M PET inch/week

#Prestley-Tylor PET
PETpt_mmday <- 1.26 * delta*df_US_NE2$Rn / (Lv * (delta + gamma)) * 86400
df_US_NE2$PETpt_mmday <- PETpt_mmday # Priestley-Taylor PET mm/day
PETpt_inweek <- PETpt_mmday * 7 * 0.03937
df_US_NE2$PETpt_inweek <- PETpt_inweek # Priestley-Taylor PET inch/week

#Calculate Penman-Monteith ET
ETpm_mmday <- 86400 * (delta*df_US_NE2$Rn + rho_a*cp*ga*df_US_NE2$VPD) / (( delta + gamma*(1+(ga/Gs))) * Lv)
ETpm_inweek <- ETpm_mmday * 7 * 0.03937 #inch/week
df_US_NE2$ETpm_mmday <- ETpm_mmday
df_US_NE2$ETpm_inweek <- ETpm_inweek 

#Save Dataframe; not working
write.csv(df_US_NE2,file ="D:\Research\R_Files\US-NE\AMF_US_NE2.csv",row.names = FALSE)
US_NE2_ET <- read.csv("D:\Research\R_Files\US-NE\AMF_US_NE2.csv")

US_NE2_ET <- df_US_NE2

head(US_NE2_ET)

library(ggplot2)
ggplot(US_NE2_ET, aes(x = Month, y = ETpm_inweek)) +
  geom_line() +
  labs(title = "ET Weekly Averages from 1990-2020", 
       x = "Month",
       y = "ET Averaged (in/wk)")


ggplot(US_NE2_ET, aes(x = Year, y = ETpm_inweek)) +
  geom_line() +
  labs(title = "ET Weekly Averages from 1990-2020",
       x = "Year",
       y = "ET Averaged (in/wk)")

###########################
View(US_NE2_ET)

ETAVG <- US_NE2_ET
#Want to see average ET per Year
#ETpm_mmday - mm ET per day

#group by year
head(ETAVG)
YearlyAvgET <- ETAVG %>% group_by(Year) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)
View(YearlyAvgET)
#remove 1999 and 2015 from low data - outliers!
#

et<-ggplot(YearlyAvgET, aes(x = Year, y = ETpm_mmday, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Average ET (mm) (1998-2020)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Year",
       y = "Penman-Monteith ET (mm/day)")+
  #theme_dark()+
  theme_gray()#+
 # geom_text(aes(label=ifelse(ETpm_mmday<0.1,as.character(" < 0.1mm"),'')),hjust=0,vjust=0)
#+
# geom_smooth(method = "lm") #attempt at adding a trendline: failed
et

tail(YearlyAvgET)
#remove 1998 (row 1) and 2015-2020 (row 18-23)... 
ET_1999_2014 <- YearlyAvgET[-c(1, 18:23), ]

et_1999_2014 <-ggplot(ET_1999_2014, aes(x = Year, y = ETpm_mmday, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Average ET (mm) (1999-2014)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Year",
       y = "Penman-Monteith ET (mm/day)")+
  #theme_dark()+
  theme_gray()#+
et_1999_2014


#why is this different? #Pre = Precipitation inch/7day
inchesrain <- ggplot(YearlyAvgET, aes(x = Year, y = Pre, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Average Weekly Precip (inches) (1999-2014)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Year",
       y = "Average Weekly Precip (inches)")+
  #theme_dark()+
  theme_gray()#+

#precip - averaged from daily mm
g

inchesrain



#VPD
VPD <- ggplot(ET_1999_2014, aes(x = Year, y = VPD, label="Name"))+
  geom_point(size =3)+ #color="firebrick"
  labs(title = "US-NE2 Site: Average VPD (1999-2014)",
       subtitle = "Irrigated Maize-Soybean Rotation Site",
       x = "Year",
       y = "VPD (hpa or kpa?)")+
  #theme_dark()+
  theme_gray()#+
VPD

