#bo1 is the Ameriflux data for US-Bo1 (link: https://ameriflux.lbl.gov/sites/siteinfo/US-Bo1)
#Data from 1996 to present
bo1 <- read.csv("D:\\Research\\US_BO1_2_FluxTower\\AMF_US-Bo1_BASE-BADM_2-1\\AMF_US-Bo1_BASE_HH_2-1.csv")
head(bo1)

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
#VPD        (hPa): Vapor Pressure Deficit

#-- MET_SOIL
#SWC        (%): Soil water content (volumetric), range 0-100
#TS          (deg C): Soil temperature
#WTD        (m): Water table depth

#-- MET_RAD
#NETRAD       (W m-2): Net radiation
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
#GPP        (umolCO2 m-2 s-1): Gross Primary Productivity

#Packages: Raster, GDal
install.packages("raster")
install.packages("rgdal")
library("raster")
library("rgdal")

#lots of -9999 in the data; what is this saying?
