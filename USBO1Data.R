#bo1 is the Ameriflux data for US-Bo1 (link: https://ameriflux.lbl.gov/sites/siteinfo/US-Bo1)
#Data from 1996 to present

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Variable List from the metadata
#-- TIMEKEEPING
#TIMESTAMP_START (YYYYMMDDHHMM): ISO timestamp start of averaging period
#TIMESTAMP_END   (YYYYMMDDHHMM): ISO timestamp end of averaging period

#-- GASES
#CO2         (umolCO2 mol-1): Carbon Dioxide (CO2) mole fraction
#H2O           (mmolH2O mol-1): Water (H2O) vapor mole fraction
#CH4        (nmolCH4 mol-1): Methane (CH4) mole fraction
#FC            (umolCO2 m-2 s-1): Carbon Dioxide (CO2) flux
#SC            (umolCO2 m-2 s-1): Carbon Dioxide (CO2) storage flux
#FCH4        (nmolCH4 m-2 s-1): Methane (CH4) flux
#SCH4        (nmolCH4 m-2 s-1): Methane (CH4) storage flux

#-- HEAT
#G           (W m-2): Soil heat flux
#H           (W m-2): Sensible heat flux
#LE          (W m-2): Latent heat flux
#SH          (W m-2): Heat storage in the air
#SLE         (W m-2): Latent heat storage flux

#-- MET_WIND
#WD            (Decimal degrees): Wind direction
#WS            (m s-1): Wind speed
#USTAR    (m s-1): Friction velocity
#ZL            (adimensional): Stability parameter

#-- MET_ATM
#PA             (kPa): Atmospheric pressure
#RH             (%): Relative humidity, range 0-100
#TA             (deg C): Air temperature
#VPD        (hPa): Vapor Pressure Deficit *****

#-- MET_SOIL
#SWC        (%): Soil water content (volumetric), range 0-100
#TS          (deg C): Soil temperature ****
#^soil temp can indicate best planting dates

#WTD        (m): Water table depth

#-- MET_RAD
#NETRAD       (W m-2): Net radiation***********
#PPFD_IN      (umolPhoton m-2 s-1): Photosynthetic photon flux density, incoming
#PPFD_OUT     (umolPhoton m-2 s-1): Photosynthetic photon flux density, outgoing
#SW_IN       (W m-2): Shortwave radiation, incoming
#SW_OUT       (W m-2): Shortwave radiation, outgoing
#LW_IN        (W m-2): Longwave radiation, incoming
#LW_OUT      (W m-2): Longwave radiation, outgoing

#-- MET_PRECIP
#P              (mm): Precipitation

#-- PRODUCTS
#NEE        (umolCO2 m-2 s-1): Net Ecosystem Exchange
#RECO        (umolCO2 m-2 s-1): Ecosystem Respiration
#GPP        (umolCO2 m-2 s-1): Gross Primary Productivity *************
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Packages: Raster, GDal
#install.packages("raster")
#install.packages("rgdal")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("dplyr")

library("ggplot2")
library("dplyr")
library("raster")
library ("lubridate")
library("rgdal")

#data is databo1
setwd("D:/Research/US_BO1_2_FluxTower/R")

databo1 <- read.csv("AMF_US-Bo1_BASE_HH_2-1.csv", header = TRUE)
head(databo1)


#header is broken
#fix from here (https://stackoverflow.com/questions/38311808/more-columns-than-column-names/38311962)

#read only the first row from the table
NAMES <- read.table("AMF_US-Bo1_BASE_HH_2-1.csv", nrow = 1, stringsAsFactors = FALSE, sep = ",")


#skip the first row in the table
DATA <- read.table("AMF_US-Bo1_BASE_HH_2-1.csv", skip = 1, stringsAsFactors = FALSE, sep = ",")

#combine them
names(DATA) <- NAMES

#it worked!
head(DATA)
View(DATA)

#clone
DATA1 <- DATA
head(DATA1)

#double header in the actual headeer and in row 1 so will delete row 1
DATA2 <- DATA1[- 1, ] 
#View(DATA2)


#dealing with time
#TIMESTAMP_START (YYYYMMDDHHMM): ISO timestamp start of averaging period
#TIMESTAMP_END   (YYYYMMDDHHMM): ISO timestamp end of averaging period
#timezone GMT-6

#date data now looks like Y-m-d... couldn't get the time (hour/min) to work
DATA2$TIMESTAMP_START <- as.Date(as.character(DATA2$TIMESTAMP_START), format="%Y%m%d")
DATA2$TIMESTAMP_END <- as.Date(as.character(DATA2$TIMESTAMP_END), format = "%Y%m%d")
View(DATA2)
head(DATA2)
DATA3 <- DATA2

#lots of -9999 which are actually NAs
DATA3[DATA3 == -9999] <- NA
View(DATA3)

#on to the averages
#tutorial: https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/ 
options(stringsAsFactors = FALSE)


str(DATA3)

#all my numbers are characters!
DATA3$USTAR <- as.numeric(DATA3$USTAR)
summary(DATA3$USTAR)

#TA             (deg C): Air temperature
DATA3$TA <- as.numeric(DATA3$TA)
summary(DATA3$TA)

#VPD        (hPa): Vapor Pressure Deficit *****
DATA3$VPD <- as.numeric(DATA3$VPD)
summary(DATA3$VPD)

DATA3$WD <- as.numeric(DATA3$WD)
DATA3$WS <- as.numeric(DATA3$WS)
DATA3$NEE_PI <- as.numeric(DATA3$NEE_PI)
DATA3$FC <- as.numeric(DATA3$FC)
DATA3$SC <- as.numeric(DATA3$SC)
DATA3$H <- as.numeric(DATA3$H)
DATA3$SH <- as.numeric(DATA3$SH)
DATA3$LE <- as.numeric(DATA3$LE)
DATA3$SLE <- as.numeric(DATA3$SLE)
DATA3$G <- as.numeric(DATA3$G)
DATA3$TS_1 <- as.numeric(DATA3$TS_1)
DATA3$TS_2 <- as.numeric(DATA3$TS_2)
DATA3$P <- as.numeric(DATA3$P)
DATA3$RH <- as.numeric(DATA3$RH)
DATA3$PA <- as.numeric(DATA3$PA)
DATA3$CO2_1 <- as.numeric(DATA3$CO2_1)
DATA3$CO2_2 <- as.numeric(DATA3$CO2_2)
DATA3$VPD_PI <- as.numeric(DATA3$VPD_PI)
DATA3$SWC_1 <- as.numeric(DATA3$SWC_1)
DATA3$SWC_2 <- as.numeric(DATA3$SWC_2)
DATA3$NETRAD <- as.numeric(DATA3$NETRAD)
DATA3$PPFD_IN <- as.numeric(DATA3$PPFD_IN)
DATA3$SW_IN <- as.numeric(DATA3$SW_IN)
DATA3$SW_DIF <- as.numeric(DATA3$SW_DIF)
DATA3$PPFD_OUT <- as.numeric(DATA3$PPFD_OUT)
DATA3$SW_OUT <- as.numeric(DATA3$SW_OUT)
DATA3$LW_IN <- as.numeric(DATA3$LW_IN)
DATA3$LW <- as.numeric(DATA3$LW)


#goal: average TA and VPD over each month for all the years... data collected says "to present" but years available end at 2008
tail(DATA3)

# plot the data using ggplot2 and pipes
DATA3 %>%
  ggplot(aes(x = TIMESTAMP_START, y = TA)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Temperature (c) at the BO-1 Flux Site",
       subtitle = "Illinois, 1996-2008",
       y = "Temperature (C)",
       x = "Date") + theme_bw(base_size = 15)


DATA3 %>%
  ggplot(aes(x = TIMESTAMP_START, y = VPD)) +
  geom_point(color = "darkorchid4") +
  labs(title = "VPD at the BO-1 Flux Site",
       subtitle = "Illinois, 1996-2008",
       y = "VPD (hPa)",
       x = "Date") + theme_bw(base_size = 15)



####separate months and years
DATA4 <- DATA3

##add column for start_year and start_month
DATA4[, "start_year"] <- format(DATA4[,"TIMESTAMP_START"], "%Y")
DATA4[, "start_month"] <- format(DATA4[,"TIMESTAMP_START"], "%m")
head(DATA4)

##add column for end_year and end_month
DATA4[, "end_year"] <- format(DATA4[,"TIMESTAMP_END"], "%Y")
DATA4[, "end_month"] <- format(DATA4[,"TIMESTAMP_END"], "%m")
View(DATA4)

#add columns for weeks
#data starts jan 1st, 1996, monday

##add column for start week year and week ID for each year
#each week starts on monday; each week is monday-sunday

DATA4[, "start_week_year"] <- format(DATA4[,"TIMESTAMP_START"], "%G")
DATA4[, "start_weekID"] <- format(DATA4[,"TIMESTAMP_START"], "%V")
View(DATA4)

?strptime

DATA5 <- DATA4


#mean function
mean_na <- function(x) {
  mean(x,na.rm=T)
}

#alot of data unavailable
DATA6 <- DATA5 %>% group_by(start_year) %>%       
  summarise_at(.vars = names(.)[1:50],.funs = mean,na.rm=TRUE)

View(DATA6)

#mean by month
DATA7 <- DATA5 %>% group_by(start_month) %>%       
  summarise_at(.vars = names(.)[1:50],.funs = mean,na.rm=TRUE)

head(DATA7)

#mean by week - average each numbers week for ALL years
DATA8 <- DATA5 %>% group_by(start_weekID) %>%       
  summarise_at(.vars = names(.)[1:50],.funs = mean,na.rm=TRUE)

View(DATA8)

#################################

#cant figure out an easier way to do this, so will try to filter by each year and average that way
#year 2000
library(tidyverse)
library(dplyr)

Year_2000 <- DATA5 %>% filter(start_week_year == "2000")
View(Year_2000)

#average each week in year 2000; deletes year/month column... hmm.. 
WeeklyAverage_2000 <- Year_2000 %>% group_by(start_weekID) %>%       
  summarise_at(.vars = names(.)[1:50],.funs = mean,na.rm=TRUE)

View(WeeklyAverage_2000)

#the function removes the year/month columns; redoing this from $TIMESTAMP_START
dataWeeklyAverage_2000 <- as.data.frame(WeeklyAverage_2000)

dataWeeklyAverage_2000[, "Year"] <- format(dataWeeklyAverage_2000[,"TIMESTAMP_START"], "%Y")
dataWeeklyAverage_2000[, "Month"] <- format(dataWeeklyAverage_2000[,"TIMESTAMP_START"], "%m")

#latitude for US-bo1 is 40.0062
lat <- 40.0062
dataWeeklyAverage_2000$lat <- lat

View(dataWeeklyAverage_2000)

#############################

#SPEI Index R tutorial https://rstudio-pubs-static.s3.amazonaws.com/202764_0f164cddb7fd415eae1389ea27cec447.html
install.packages("SPEI")
library("SPEI")
#Data5 is the base cleaned up data - not averaged or weekly
#weekly_average_2000 is averaged data by week in only 2000

#What you need to calculate evapotranspiration from the Penman-Monteith equation
#latitude for US-bo1 is 40.0062

#The FAO Penman-Monteith method to estimate ETo can be
#derived [Eq. 1]:

#ETo = reference evapotranspiration, mm day-1

#Rn= net radiation at the crop surface, MJ m-2d-1
#G = soil heat flux density, MJ m-2d-1
#T = mean daily air temperature at 2 m height, ºC;
#u2= wind speed at 2 m height, m s-1 
#es= saturation vapor pressure, kPa;
#ea= actual vapor pressure, kPa;
#es-ea= saturation vapor pressure deficit, kPa;
#î = slope of the vapor pressure curve, kPa ºC-1
#³ = psychrometric constant, kPa ºC-1

####################

#Our Relevant Data
#LE          (W m-2): Latent heat flux
#TA             (deg C): Air temperature;                       Qing's is Ta
#PA             (kPa): Atmospheric pressure;                    Qing's is pres
#USTAR    (m s-1): Friction velocity;                           Qing's is ustar
#H           (W m-2): Sensible heat flux
#NETRAD       (W m-2): Net radiation;                           Qing's is Rn
#VPD        (hPa): Vapor Pressure Deficit; 1hpa = 100pa
#P              (mm): Precipitation

#only one with different units:
#WS            (m s-1): Wind speed (m/s) (meters per second);    Qing's is u
#Qing's data for wind speed (U) is W/m2 (Watt Per Square Metre)
#Penman Monteith equation needs windspeed in m/s so will leave as is.



#Qing's Data is::::
#LE <- latent heat flux (W/m2)
#Ta <- air temperature, deg C
#pres <- air pressure, kPa
#ustar <- friction velocity, m/s
#H <- sensible heat flux (W/m2)
#Rn <- net radiation (W/m2)
#VPD <- hpa 1hpa = 100pa
        #only one with different units:
#U <- wind speed (after coordinate rotation), W/m2




#Following Qing's ET R code ............ 
#YEAR 2000

#read weekly averaged US-MMS flux data. calculated from FLUXNET 2015
TestWeeklyAverage_2000 <- dataWeeklyAverage_2000
##1.2 make the data frame
##Tem is degree C (not be used in PM-PET calculation); no latitude column, latitude for US-bo1 is 40.0062
df_PM <- data.frame(Lat = TestWeeklyAverage_2000$lat, Year = TestWeeklyAverage_2000$Year, Month = TestWeeklyAverage_2000$Month, WeekID = TestWeeklyAverage_2000$start_weekID) 
df_PM$TIMESTAMP_START <- TestWeeklyAverage_2000$TIMESTAMP_START
df_PM$Tem <- TestWeeklyAverage_2000$TA 
df_PM$Pre <- TestWeeklyAverage_2000$P * 7 * 0.0394 #Precipitation inch/7day 
df_PM$LE <- TestWeeklyAverage_2000$LE # w/m2 Latent heat flux, ###gapfilled using MDS method; I didn't do this
df_PM$LE[df_PM$LE <0] <- 0 #PET is zero
df_PM$ustar <- TestWeeklyAverage_2000$USTAR #Friction velocity
df_PM$Hs <- TestWeeklyAverage_2000$H #Sensible heat flux, #### gapfilled using MDS method; I didn't do this
df_PM$U <- TestWeeklyAverage_2000$WS #Wind speed
df_PM$Rn <- TestWeeklyAverage_2000$NETRAD  #net radiation
df_PM$VPD <- TestWeeklyAverage_2000$VPD * 0.1 #from hPa to Kpa 


##1.3 constant parameters for flux site and canopy; site info: https://ameriflux.lbl.gov/sites/siteinfo/US-Bo1 
Elev <- 219 #in m, elevation data for air pressure calculation; US-Bo1 is 219

#not sure if these are correct.... or where to find them for a specific flux tower
zm <- 46 #(measurement height for wind speed and humidity); I'm not sure where to find this?????? Will keep Qing's # for now
h <- 27 #canopy height; ; I'm not sure what this is for this site; Gussing lower for farm site, 0 creates error 
zd <- 2/3 * h #zd is zero plane displacement height ; I'm not sure where to find this??????
zo <- 0.1 * h #roughness length governing momentum transfer and governing transfer of heat and vapour ;I'm not sure where to find this??????


##1.4 Calculate ET
#define a variable, 'avgvar', that is 1 if tower data are half-hourly, 2 if data are hourly. 
#avgvar <- 48
#Convert LE to ET
Lv <- 2.500 * 10^6 - 2.386 *10^3*df_PM$Tem  #J/kg   #this is the latent heat of vaporization, Allen 1998 2.45 MJ/m^3
#ET <- df_PM$LE / Lv* 1.8 * avgvar  #the 1.8 is the number of seconds in a half-hour/1000
ETlv_mmday <- 86400 * df_PM$LE / Lv #kg/m2/s = 86400mm/day ET calculated directly from LE
ETlv_inweek <- ETlv_mmday * 7 * 0.03937 #inch/week, 7 days a week
df_PM$ETlv_mmday <- ETlv_mmday
df_PM$ETlv_inweek <- ETlv_inweek 


##1.5 define some constants necessary for the Penman-Monteith Equation, a few of which depend on temperature
cp <- 1006 #specific heat capacity of dry air, J/kg/?C.  Cp = 1.013 10-3 [MJ kg-1 ?C-1]
rho_w <- 1000 #density of water, kg/m^3
k <- 0.4 #von Karman constant, unitless
rho_a <- 101.3*10^3/(287.058) /(df_PM$Tem+273.15) #density of dry air kg/m^3  #287.058 ideal gas law: dry air density = P/ (specific gas constant * Tk) 
#S <- 2508/(df_PM$Tem+237.3)^2 * exp(17.3*df_PM$Tem/(df_PM$Tem+237)) #slope of saturation water vapor function, #kPa/KSTst
delta <-  4098*0.6108*exp(17.27*df_PM$Tem/(df_PM$Tem+237.3))/(df_PM$Tem+237.3)^2 #slope of saturation water vapor function, #kPa/K 
pres <- 101.3 * (((293-0.0065*Elev)/293)^5.26)#air pressure, kPa
gamma <- cp * pres/(0.622*Lv)  #psychrometric constant, kPa/K,cp: 1.013 10-3 [MJ kg-1 ?C-1],
Ta_K <- df_PM$Tem + 273.15 #air temp in kelvin
Rho <- (1.3079 - 0.0045*df_PM$Tem)  #density of dry air kg m-3
Cp <- 1005 #specific heat capacity of air, J/kg/?C



##1.6 correction parameter for ga
#find the aerodynamic conductance 
#OL is Monin-Obhukov lengh #stab is the atmospheric stability facture
#consult SI of Novick et al. 2016 (Nature Climate Change) for details
eps <- 0.1^6
OL <- -Rho * (df_PM$ustar + eps)^3 /(0.4*9.81*((df_PM$Hs + eps)/(Cp*Ta_K))) #the ep is a very small number, #which prevents ustart and H from being exactly zero, which mucks up the calculations
stab <- (zm-2/3*zd) / OL
psiH <- 6*log(1+stab) #atmospheric stability correction
if (length(psiH) == length(stab)) {
  psiH[which(stab <0)] <- -2*log((1+(1-16*stab[which(stab<0)])^0.5)/2)
} else {
  print ("check the dataset")
}

#ISSUE HERE: 
##1.7 ga (m/s), Gs (m/s) calculation
#atmospheric stability correction
ga <- df_PM$U * k^2 /((log((zm-zd)/zo) + psiH)^2) #m/s

#Finally, find the reference surface conductance by inverting the penman monteith equation
Gs <- gamma*df_PM$LE*ga / ( delta*df_PM$Rn + rho_a*cp*ga*df_PM$VPD - df_PM$LE*(delta+gamma) )                     
df_PM$Gs <- Gs
View(Gs)


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
##1.8 Peman-Monteith PET calculation
dfref <- df_PM[abs(df_PM$VPD - 1) < 0.05,]
if (nrow(dfref) == 0) {
  print("check condition setting,no data at VPD~1kpa")
} else {
Gsref <- mean(dfref$Gs,na.rm = TRUE)
}
#ISSUE: Gsref not actually created from function above.... 
Gsref <- mean(dfref$Gs,na.rm = TRUE)


df_PM$Gsref <- Gsref
#daily PET kg/m2/s = 86400mm/day
PETpm_mmday <- 86400 * ( delta*df_PM$Rn + rho_a*cp*ga*df_PM$VPD ) / (( delta + gamma*(1+(ga/Gsref))) * Lv)                             
df_PM$PETpm_mmday <- PETpm_mmday #P-M PET mm/day
#weekly PET in inch (1mm = 0.0393701 inch)
PETpm_inweek <- PETpm_mmday * 7 * 0.03937 
df_PM$PETpm_inweek <- PETpm_inweek #P-M PET inch/week
##1.9 Prestley-Tylor PET
PETpt_mmday <- 1.26 * delta*df_PM$Rn / (Lv * (delta + gamma)) * 86400
df_PM$PETpt_mmday <- PETpt_mmday # Priestley-Taylor PET mm/day
PETpt_inweek <- PETpt_mmday * 7 * 0.03937
df_PM$PETpt_inweek <- PETpt_inweek # Priestley-Taylor PET inch/week
##1.10 calculate Penman-Monteith ET
ETpm_mmday <- 86400 * (delta*df_PM$Rn + rho_a*cp*ga*df_PM$VPD) / (( delta + gamma*(1+(ga/Gs))) * Lv)
ETpm_inweek <- ETpm_mmday * 7 * 0.03937 #inch/week
df_PM$ETpm_mmday <- ETpm_mmday
df_PM$ETpm_inweek <- ETpm_inweek 

write.csv(df_PM,file = "2000_ET.csv",row.names = FALSE)

