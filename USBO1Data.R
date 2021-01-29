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
View(DATA4)

##add column for end_year and end_month
DATA4[, "end_year"] <- format(DATA4[,"TIMESTAMP_END"], "%Y")
DATA4[, "end_month"] <- format(DATA4[,"TIMESTAMP_END"], "%m")
View(DATA4)


DATA5 <- DATA4


#mean by year
mean_na <- function(x) {
  mean(x,na.rm=T)
}

#alot of data unavailable
DATA6 <- DATA5 %>% group_by(start_year) %>%       
  summarise_at(.vars = names(.)[1:30],.funs = mean,na.rm=TRUE)

View(DATA6)

#mean by year; doesnt seem to work?
DATA7 <- DATA5 %>% group_by(start_month) %>%       
  summarise_at(.vars = names(.)[1:30],.funs = mean,na.rm=TRUE)

View(DATA7)


#SPEI Index R tutorial https://rstudio-pubs-static.s3.amazonaws.com/202764_0f164cddb7fd415eae1389ea27cec447.html
install.packages("SPEI")
library("SPEI")

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


#what our data has
#NETRAD       (W m-2): Net radiation
#G           (W m-2): Soil heat flux
#TA             (deg C): Air temperature
#WS            (m s-1): Wind speed
#VPD        (hPa): Vapor Pressure Deficit


#attempt without unit conversion (let's see if it works)
PET = thornthwaite(DATA5$T,40.0062)
#nope
