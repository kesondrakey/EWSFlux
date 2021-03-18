#Ameriflux Data
#Calculate ET From Ameriflux Data (https://ameriflux.lbl.gov/sites/siteinfo/US-mms) 
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
library("tidyverse")

#Step 1. Open Data

AMF_US.MMS_BASE_HR_18.5 <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US-MMS_BASE-BADM_18-5/AMF_US-MMS_BASE_HR_18-5.csv", comment.char="#")
setwd("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/")
AMF_USMMS1 <- AMF_US.MMS_BASE_HR_18.5

head(AMF_USMMS1)

#Step 2. Clean up the data
#Fix Time issues

#Time looks like 199901010000 (YYYYMMDDHHMM) under TIMESTAMP_START/TIMESTAMP_END
#Turn YYYYMMDDHHMM into YYYYMMDD
AMF_USMMS1$TIMESTAMP_START <- as.Date(as.character(AMF_USMMS1$TIMESTAMP_START), format="%Y%m%d")
AMF_USMMS1$TIMESTAMP_END <- as.Date(as.character(AMF_USMMS1$TIMESTAMP_END), format = "%Y%m%d")

#Turn -9999 into NAs
AMF_USMMS1[AMF_USMMS1 == -9999] <- NA

##add column for start_year and start_month; will use this for now
AMF_USMMS1[, "Start_Year"] <- format(AMF_USMMS1[,"TIMESTAMP_START"], "%Y")
AMF_USMMS1[, "Start_Month"] <- format(AMF_USMMS1[,"TIMESTAMP_START"], "%m")
AMF_USMMS1[, "Start_Day"] <- format(AMF_USMMS1[,"TIMESTAMP_START"], "%d")


##add column for end_year and end_month
AMF_USMMS1[, "End_Year"] <- format(AMF_USMMS1[,"TIMESTAMP_END"], "%Y")
AMF_USMMS1[, "End_Month"] <- format(AMF_USMMS1[,"TIMESTAMP_END"], "%m")

#Check if it worked; it did
head(AMF_USMMS1)

#add columns for weeks
##add column for start week year and week ID for each year
#Data starts on 01-01-1999 starts on a Friday; not sure how to fix this yet ####

AMF_USMMS1[, "Start_Week_Year"] <- format(AMF_USMMS1[,"TIMESTAMP_START"], "%G")
AMF_USMMS1[, "Start_weekID"] <- format(AMF_USMMS1[,"TIMESTAMP_START"], "%V")

#Set Latitude
#latitude for US-MMS is 39.3232 (info found here: https://ameriflux.lbl.gov/sites/siteinfo/US-MMS) 
lat <- 39.3232
AMF_USMMS1$lat <- lat

AMF_USMMS <- AMF_USMMS1

head(AMF_USMMS)

#Take the weekly averages of each variable for each year and month
WeeklyAMF_USMMS <- AMF_USMMS %>% group_by(Start_Week_Year, Start_weekID, Start_Month) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)
head(WeeklyAMF_USMMS)
#includes 1998 since 01-01-1999 and 01-02-1999 are a friday and saturday
str(WeeklyAMF_USMMS)


#Step 3. Calculate ET; Ameriflux variables are explained here: (https://ameriflux.lbl.gov/data/aboutdata/data-variables/#base)
#Our Relevant Data

#LE          (W m-2): Latent heat flux
#TA             (deg C): Air temperature;             Qing's is Ta
#PA             (kPa): Atmospheric pressure;          Qing's is pres
#USTAR    (m s-1): Friction velocity;                 Qing's is ustar
#H           (W m-2): Sensible heat flux
#NETRAD       (W m-2): Net radiation;                 Qing's is Rn
#VPD        (hPa): Vapor Pressure Deficit; 1hpa = 100pa
#P              (mm): Precipitation
#WS             (m s-1): Wind speed

#For now, we will use _1_1_1 data since I'm not sure which height is the best
# LE ->       LE_1_1_1
#TA ->        TA_1_1_1 
#PA ->        PA_1_1_1
#USTAR ->     USTAR_1_1_1
#H ->         H_1_1_1
#NETRAD ->    NETRAD_1_1_1 
#VPD ->       VPD_PI_1_1_1; *For all files with processing version 1, VPD_PI is provided in the units kPa (instead of hPa) (https://ameriflux.lbl.gov/data/data-change-log/) ### I'm unsure what level processing this data has, hmm. 
#P ->         P_1_1_1
#WS ->        WS_1_1_1 


#Create a Dataframe
#Add Latitude variable; 39.3232 is the latitude for the US-MMS site (https://ameriflux.lbl.gov/sites/siteinfo/US-MMS#overview)
lat <- 39.3232
WeeklyAMF_USMMS$lat <- lat

##Tem is degree C (not be used in PM-PET calculation); no latitude column, latitude for US-bo1 is 40.0062
df_AMF_USMMS <- data.frame(Lat = WeeklyAMF_USMMS$lat, Year = WeeklyAMF_USMMS$Start_Week_Year, Month = WeeklyAMF_USMMS$Start_Month, WeekID = WeeklyAMF_USMMS$Start_weekID) 
head(df_AMF_USMMS)

#unsure how to add the timestamp variable back; got an error when I tried
#We will use the start dates for time

#LE             
df_AMF_USMMS$LE <- WeeklyAMF_USMMS$LE_1_1_1 # w/m2 Latent heat turbulent flux (no storage correction)
df_AMF_USMMS$LE[df_AMF_USMMS$LE <0] <- 0 #PET is zero

#TA             (deg C): Air temperature
df_AMF_USMMS$Tem <- WeeklyAMF_USMMS$TA_1_1_1  #temperature in degree C

#USTAR          (m s-1): Friction velocity
df_AMF_USMMS$ustar <- WeeklyAMF_USMMS$USTAR_1_1_1 #Friction velocity, m s-1

#H              (W m-2): Sensible heat flux
df_AMF_USMMS$Hs <- WeeklyAMF_USMMS$H_1_1_1 #Sensible heat turbulent flux (no storage correction); W m-2

#NETRAD         (W m-2): Net radiation
df_AMF_USMMS$Rn <- WeeklyAMF_USMMS$NETRAD_1_1_1  #net radiation; W m-2

#VPD            (hPa or kpa; check note - assuming kpa for now): Vapor Pressure Deficit                     ##########SEE NOTE#######
df_AMF_USMMS$VPD <- WeeklyAMF_USMMS$VPD_PI_1_1_1 #* 0.1 #from hPa to Kpa #Vapor Pressure Deficit, hPa; ###For all files with processing version 1, VPD_PI is provided in the units kPa (instead of hPa). I'm not sure which version US-MMS has

#P              (mm): Precipitation
df_AMF_USMMS$Pre <- WeeklyAMF_USMMS$P_1_1_1 * 7 * 0.0394 #Precipitation inch/7day

#WS             (m s-1): Wind speed
df_AMF_USMMS$U <- WeeklyAMF_USMMS$WS_1_1_1 #Wind speed


#Constants
Elev <- 275 #in m, elevation data for air pressure calculation
zm <- 46 #(measurement height for wind speed and humidity)
h <- 27 #canopy height
zd <- 2/3 * h #zd is zero plane displacement height 
zo <- 0.1 * h #roughness length governing momentum transfer and governing transfer of heat and vapour 



#Convert LE to ET
Lv <- 2.500 * 10^6 - 2.386 *10^3*df_AMF_USMMS$Tem  #J/kg   #this is the latent heat of vaporization, Allen 1998 2.45 MJ/m^3
ETlv_mmday <- 86400 * df_AMF_USMMS$LE / Lv #kg/m2/s = 86400mm/day ET calculated directly from LE
ETlv_inweek <- ETlv_mmday * 7 * 0.03937 #inch/week, 7 days a week
df_AMF_USMMS$ETlv_mmday <- ETlv_mmday
df_AMF_USMMS$ETlv_inweek <- ETlv_inweek 


#Define Constants for ET calculation
#define some constants necessary for the Penman-Monteith Equation, a few of which depend on temperature
cp <- 1006 #specific heat capacity of dry air, J/kg/?C.  Cp = 1.013 10-3 [MJ kg-1 ?C-1]
rho_w <- 1000 #density of water, kg/m^3
k <- 0.4 #von Karman constant, unitless
rho_a <- 101.3*10^3/(287.058) /(df_AMF_USMMS$Tem+273.15) #density of dry air kg/m^3  #287.058 ideal gas law: dry air density = P/ (specific gas constant * Tk) 
#S <- 2508/(df_AMF_USMMS$Tem+237.3)^2 * exp(17.3*df_AMF_USMMS$Tem/(df_AMF_USMMSTem+237)) #slope of saturation water vapor function, #kPa/KSTst
delta <-  4098*0.6108*exp(17.27*df_AMF_USMMS$Tem/(df_AMF_USMMS$Tem+237.3))/(df_AMF_USMMS$Tem+237.3)^2 #slope of saturation water vapor function, #kPa/K 
pres <- 101.3 * (((293-0.0065*Elev)/293)^5.26)#air pressure, kPa
gamma <- cp * pres/(0.622*Lv)  #psychrometric constant, kPa/K,cp: 1.013 10-3 [MJ kg-1 ?C-1],
Ta_K <- df_AMF_USMMS$Tem + 273.15 #air temp in kelvin
Rho <- (1.3079 - 0.0045*df_AMF_USMMS$Tem)  #density of dry air kg m-3
Cp <- 1005 #specific heat capacity of air, J/kg/?C


#Correction parameter for ga
#find the aerodynamic conductance 
#OL is Monin-Obhukov lengh #stab is the atmospheric stability facture
#consult SI of Novick et al. 2016 (Nature Climate Change) for details
eps <- 0.1^6
OL <- -Rho * (df_AMF_USMMS$ustar + eps)^3 /(0.4*9.81*((df_AMF_USMMS$Hs + eps)/(Cp*Ta_K))) #the ep is a very small number, #which prevents ustart and H from being exactly zero, which mucks up the calculations
stab <- (zm-2/3*zd) / OL
psiH <- 6*log(1+stab) #atmospheric stability correction
if (length(psiH) == length(stab)) {
  psiH[which(stab <0)] <- -2*log((1+(1-16*stab[which(stab<0)])^0.5)/2)
} else {
  print ("check the dataset")
}



#ga (m/s), Gs (m/s) calculation
#atmospheric stability correction
ga <- df_AMF_USMMS$U * k^2 /((log((zm-zd)/zo) + psiH)^2) #m/s

#Finally, find the reference surface conductance by inverting the penman monteith equation
Gs <- gamma*df_AMF_USMMS$LE*ga / ( delta*df_AMF_USMMS$Rn + rho_a*cp*ga*df_AMF_USMMS$VPD - df_AMF_USMMS$LE*(delta+gamma) )                     
df_AMF_USMMS$Gs <- Gs
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
dfref <- df_AMF_USMMS[abs(df_AMF_USMMS$VPD - 1) < 0.05,]
if (nrow(dfref) == 0) {
  print("check condition setting,no data at VPD~1kpa")
} else {
  Gsref <- mean(dfref$Gs,na.rm = TRUE)
}
df_AMF_USMMS$Gsref <- Gsref

#daily PET kg/m2/s = 86400mm/day
PETpm_mmday <- 86400 * ( delta*df_AMF_USMMS$Rn + rho_a*cp*ga*df_AMF_USMMS$VPD ) / (( delta + gamma*(1+(ga/Gsref))) * Lv)                             
df_AMF_USMMS$PETpm_mmday <- PETpm_mmday #P-M PET mm/day

#weekly PET in inch (1mm = 0.0393701 inch)
PETpm_inweek <- PETpm_mmday * 7 * 0.03937 
df_AMF_USMMS$PETpm_inweek <- PETpm_inweek #P-M PET inch/week

#Prestley-Tylor PET
PETpt_mmday <- 1.26 * delta*df_AMF_USMMS$Rn / (Lv * (delta + gamma)) * 86400
df_AMF_USMMS$PETpt_mmday <- PETpt_mmday # Priestley-Taylor PET mm/day
PETpt_inweek <- PETpt_mmday * 7 * 0.03937
df_AMF_USMMS$PETpt_inweek <- PETpt_inweek # Priestley-Taylor PET inch/week

#Calculate Penman-Monteith ET
ETpm_mmday <- 86400 * (delta*df_AMF_USMMS$Rn + rho_a*cp*ga*df_AMF_USMMS$VPD) / (( delta + gamma*(1+(ga/Gs))) * Lv)
ETpm_inweek <- ETpm_mmday * 7 * 0.03937 #inch/week
df_AMF_USMMS$ETpm_mmday <- ETpm_mmday
df_AMF_USMMS$ETpm_inweek <- ETpm_inweek 

#Save Dataframe
write.csv(df_AMF_USMMS,file = "D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US_MMS.csv",row.names = FALSE)

US_MMS_ET <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/AMF_US_MMS.csv")



library(ggplot2)
ggplot(US_MMS_ET, aes(x = Month, y = ETpm_inweek)) +
  geom_line() +
  labs(title = "ET Weekly Averages from 1990-2020",
       x = "Month",
       y = "ET Averaged (in/wk)")


ggplot(US_MMS_ET, aes(x = Year, y = ETpm_inweek)) +
  geom_line() +
  labs(title = "ET Weekly Averages from 1990-2020",
       x = "Year",
       y = "ET Averaged (in/wk)")

###########################
#Next Step:

#How to forecast it:
#machine learning (Mallory) vs mechanistic (Sander)

#Sander: simple model for conductance at the canopy scale

#Kesondra and Qing
#provide minimal model for surface conductance for daily scale
#provide all the other variables in columns 
#windspeed forecasting from NEON?
#if WS is not available


#Data being procided from EFI
#35 day forecast from NOAA

#humidity, temp, precip?
#Are the LE and NEE gap filled?






























############################
#FluxNet 2015 DATA
#Goal: Calculate ET from FluxNet 2015 Data using the Penman Monteith Method

#step One
#Open Files
#Set directory and read in Fluxnet data (from FLUXNET 2015) for weekly averages for US-MMS Site (https://fluxnet.org/data/download-data/)
setwd("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/FluxNet_US_MMS/")
dfUSMMS <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/FluxNet_US_MMS/FLX_US-MMS_FLUXNET2015_FULLSET_1999-2014_1-4/Weekly/FLX_US-MMS_FLUXNET2015_FULLSET_WW_1999-2014_1-4.csv")
library(data.table)

#Step Two
#Clean up the Data

#Fix Time issues
#Time looks like 199901010000 (YYYYMMDDHHMM) under TIMESTAMP_START/TIMESTAMP_END
#Turn YYYYMMDDHHMM into YYYYMMDD
dfUSMMS$TIMESTAMP_START <- as.Date(as.character(dfUSMMS$TIMESTAMP_START), format="%Y%m%d")
dfUSMMS$TIMESTAMP_END <- as.Date(as.character(dfUSMMS$TIMESTAMP_END), format = "%Y%m%d")

#Turn -9999 into NAs
dfUSMMS[dfUSMMS == -9999] <- NA

##add column for start_year and start_month; will use this for now
dfUSMMS[, "Start_Year"] <- format(dfUSMMS[,"TIMESTAMP_START"], "%Y")
dfUSMMS[, "Start_Month"] <- format(dfUSMMS[,"TIMESTAMP_START"], "%m")

##add column for end_year and end_month
dfUSMMS[, "End_Year"] <- format(dfUSMMS[,"TIMESTAMP_END"], "%Y")
dfUSMMS[, "End_Month"] <- format(dfUSMMS[,"TIMESTAMP_END"], "%m")

#Check if it worked; it did
head(dfUSMMS)

#add columns for weeks
##add column for start week year and week ID for each year
#Data starts on 01-01-1999 starts on a Friday; not sure how to fix this yet ####

dfUSMMS[, "Start_Week_Year"] <- format(dfUSMMS[,"TIMESTAMP_START"], "%G")
dfUSMMS[, "Start_weekID"] <- format(dfUSMMS[,"TIMESTAMP_START"], "%V")

#Set Latitude
#latitude for US-MMS is 39.3232 (info found here: https://ameriflux.lbl.gov/sites/siteinfo/US-MMS) 
lat <- 39.3232
dfUSMMS$lat <- lat

df_USMMS <- dfUSMMS


#Step 3
#Make the Dataframe

#To calculate ET, We need....
#LE             (W m-2): Latent heat flux 
#TA             (deg C): Air temperature
#PA             (kPa): Atmospheric pressure
#USTAR          (m s-1): Friction velocity
#H              (W m-2): Sensible heat flux
#NETRAD         (W m-2): Net radiation
#VPD            (hPa): Vapor Pressure Deficit
#P              (mm): Precipitation
#WS             (m s-1): Wind speed

#Variables from the data
#will Use start year, month, and weekIDs for ET calculations; please correct if this is wrong
df_PM <- data.frame(Lat = df_USMMS$lat, Year = df_USMMS$Start_Week_Year, Month = df_USMMS$Start_Month, WeekID = df_USMMS$Start_weekID) 

#We will use the start dates for time
df_PM$TIMESTAMP_START <- df_USMMS$TIMESTAMP_START
#LE             (W m-2): Latent heat flux 
df_PM$LE <- df_USMMS$LE_F_MDS # w/m2 Latent heat flux, gapfilled using MDS method
df_PM$LE[df_PM$LE <0] <- 0 #PET is zero
#TA             (deg C): Air temperature
df_PM$Tem <- df_USMMS$TA_F #temperature in degree C
#USTAR          (m s-1): Friction velocity
df_PM$ustar <- df_USMMS$USTAR #Friction velocity
#H              (W m-2): Sensible heat flux
df_PM$Hs <- df_USMMS$H_F_MDS #Sensible heat flux, gapfilled using MDS method
#NETRAD         (W m-2): Net radiation
df_PM$Rn <- df_USMMS$NETRAD  #net radiation
#VPD            (hPa): Vapor Pressure Deficit
df_PM$VPD <- df_USMMS$VPD_F * 0.1 #from hPa to Kpa #Vapor Pressure Deficit consolidated from VPD_F_MDS and VPD_ERA
#P              (mm): Precipitation
df_PM$Pre <- df_USMMS$P_F * 7 * 0.0394 #Precipitation inch/7day
#WS             (m s-1): Wind speed
df_PM$U <- df_USMMS$WS_F #Wind speed, consolidated from WS and WS_ERA

#Constants
Elev <- 275 #in m, elevation data for air pressure calculation
zm <- 46 #(measurement height for wind speed and humidity)
h <- 27 #canopy height
zd <- 2/3 * h #zd is zero plane displacement height 
zo <- 0.1 * h #roughness length governing momentum transfer and governing transfer of heat and vapour 


#Step 4
#Calculate ET

#Convert LE to ET
Lv <- 2.500 * 10^6 - 2.386 *10^3*df_PM$Tem  #J/kg   #this is the latent heat of vaporization, Allen 1998 2.45 MJ/m^3
ETlv_mmday <- 86400 * df_PM$LE / Lv #kg/m2/s = 86400mm/day ET calculated directly from LE
ETlv_inweek <- ETlv_mmday * 7 * 0.03937 #inch/week, 7 days a week
df_PM$ETlv_mmday <- ETlv_mmday
df_PM$ETlv_inweek <- ETlv_inweek 

#Define Constants for ET calculation
#define some constants necessary for the Penman-Monteith Equation, a few of which depend on temperature
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


#Correction parameter for ga
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


#ga (m/s), Gs (m/s) calculation
#atmospheric stability correction
ga <- df_PM$U * k^2 /((log((zm-zd)/zo) + psiH)^2) #m/s

#Finally, find the reference surface conductance by inverting the penman monteith equation
Gs <- gamma*df_PM$LE*ga / ( delta*df_PM$Rn + rho_a*cp*ga*df_PM$VPD - df_PM$LE*(delta+gamma) )                     
df_PM$Gs <- Gs
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
dfref <- df_PM[abs(df_PM$VPD - 1) < 0.05,]
if (nrow(dfref) == 0) {
  print("check condition setting,no data at VPD~1kpa")
} else {
  Gsref <- mean(dfref$Gs,na.rm = TRUE)
}
df_PM$Gsref <- Gsref

#daily PET kg/m2/s = 86400mm/day
PETpm_mmday <- 86400 * ( delta*df_PM$Rn + rho_a*cp*ga*df_PM$VPD ) / (( delta + gamma*(1+(ga/Gsref))) * Lv)                             
df_PM$PETpm_mmday <- PETpm_mmday #P-M PET mm/day

#weekly PET in inch (1mm = 0.0393701 inch)
PETpm_inweek <- PETpm_mmday * 7 * 0.03937 
df_PM$PETpm_inweek <- PETpm_inweek #P-M PET inch/week

#Prestley-Tylor PET
PETpt_mmday <- 1.26 * delta*df_PM$Rn / (Lv * (delta + gamma)) * 86400
df_PM$PETpt_mmday <- PETpt_mmday # Priestley-Taylor PET mm/day
PETpt_inweek <- PETpt_mmday * 7 * 0.03937
df_PM$PETpt_inweek <- PETpt_inweek # Priestley-Taylor PET inch/week

#Calculate Penman-Monteith ET
ETpm_mmday <- 86400 * (delta*df_PM$Rn + rho_a*cp*ga*df_PM$VPD) / (( delta + gamma*(1+(ga/Gs))) * Lv)
ETpm_inweek <- ETpm_mmday * 7 * 0.03937 #inch/week
df_PM$ETpm_mmday <- ETpm_mmday
df_PM$ETpm_inweek <- ETpm_inweek 

#Save Dataframe
write.csv(df_PM,file = "D:/Research/R_Files/Morgan_Monroe_Flux_Tower/FluxNet_US_MMS/US_MMS_ET.csv",row.names = FALSE)

US_MMS_ET <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/FluxNet_US_MMS/US_MMS_ET.csv")

#Step 5
#graph it!

library(ggplot2)
ggplot(US_MMS_ET, aes(x = Month, y = ETpm_inweek)) +
  geom_line() +
  labs(title = "ET Weekly Averages from 1990-2020",
       x = "Month",
       y = "ET Averaged (in/wk)")

#Shows overall trendline
library(ggplot2)
library(scales)
ggplot(US_MMS_ET, aes(x = Month, y = ETpm_inweek)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth() +
  labs(title = "US-MMS ET Weekly Averages",
       subtitle = "1990 to 2020",
       x = "Month",
       y = "ET Averaged (in/wk)") +
  theme_minimal()

#Average weeks over all years, months, weeks for average ET
YearlyETAverages <- US_MMS_ET %>% group_by(Year) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)

str(YearlyETAverages)

#Remove 1998 since it has very little data and scews the graph
YearlyETAverages1 = YearlyETAverages[-c(1),]

View(YearlyETAverages1)

# Plot
# Approach 2:
library(ggfortify)
library(reshape2)
#axisyears <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")       

ggplot(YearlyETAverages1) + geom_line(aes(x=Year, y=ETpm_inweek, color="ETpm_inweek")) + 
  geom_line(aes(x=Year, y=VPD, col="VPD")) + 
  scale_color_discrete(name="Legend") + 
  labs(title="Annual Average ET (calculated from in/week)", subtitle = "US-MMS Site (1999-2014)", x="Year", y="ET Averaged from inches per week") +
  
  
  #scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"),
  #  axisyears <- c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")       
  #scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
  #                labels=  c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014"))
  #theme(axis.text.x = element_text(angle = 90)).
  
  #Each year has very different average ET and VPD rates - why is this?
  
  MonthlyETAverages <- US_MMS_ET %>% group_by(Month) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)


WeeklyETAverages <- US_MMS_ET %>% group_by(WeekID) %>%
  summarise_if(is.numeric,mean,na.rm=TRUE)

View(WeeklyETAverages)