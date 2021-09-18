##############################################
#Procedures to calculate DATRAS data products
#
# With this script and the WKSAE_algorithms.csv file, it is
# possible to reproduce the FLEXfiles as available in DATRAS 
# download page.
# As much as possible, the intention is that changes in the algorithms 
# only affect the csv file, whereas the R script remains valid.
# In case of change to the algorithms, year ranges should be reviewed.
# Also, if a new algorithm starts differentiating sweep lengths when it did not before, 
# or the opposite, stops using sweep lengths, the called function in utilities should be reviewed

#############################################
# Authors: Cecilia Kvaavik, Vaishav Soni and Adriana Villamor 
# September 2021

library(icesDatras)
library(dplyr)
library(dbplyr)
library(tidyr)
#library(tibble)

WKSAE_algorithms <- read.csv("WKSAE_algorithms.csv")


quarters <- 1:4
cal_date <- "20211609"

survey_list <- c("FR-CGFS","IE-IAMS", "NIGFS","ROCKALL", "SCOROC", "SWC-IBTS", "SP-PORC",
                 "SP-NORTH", "NS-IBTS", "EVHOE", "SP-ARSA", "IE-IGFS", "SCOWCGFS")

for(n in 1:8){
  survey <- survey_list[[n]]

if (survey == "FR-CGFS") {
  years <- 2015:2021
  country <- "FR"
} else if (survey == "IE-IAMS") {
years <- 2016:2021
country <- "IE"
}else if (survey == "NIGFS") {
years <- 2005:2021
country <- "GB-NIR"
}else if (survey == "ROCKALL") {
  years <- 2005:2009
  country <- "GB-SCT"
} else if (survey == "SCOROC") {
  years <- 2016:2021
  country <- "GB-SCT"
} else if (survey == "SWC-IBTS") {
  years <- 2004:2010
  country <- "GB-SCT"
} else if (survey == "SP-PORC") {
  years <- 2016:2021
  country <- "ES"
}else if (survey == "SP-NORTH") {
  years <- 2016:2021
  country <- "ES"
}

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values

deg2rad <- function(deg) {(deg * pi) / (180)}

data <- HH %>%
  filter(HaulVal == "V") %>%
  mutate(
  Cal_Distance = ifelse(!is.na(Distance), 
                         Distance,
                         ifelse(!is.na(GroundSpeed),
                                HaulDur/60*1852*GroundSpeed,
                                1.852 * 360 * 60 / 2 * pi*(acos(cos(deg2rad(ShootLat)) * cos(deg2rad(HaulLat)) * cos(deg2rad(HaulLong) - deg2rad(ShootLong)) + sin(deg2rad(ShootLat)) * sin(deg2rad(HaulLat)))))))

#calculating missing DoorSpread and Wingspread

source("utilities.R")

data <- calculate_DS_WS(data)

file_prefix <- paste0(survey, "_Flexfile",cal_date)
write.csv(data,paste0(file_prefix, ".csv")) 
}

for(n in 9){
  survey <- survey_list[[n]]
  years <- 2003:2021
  
  #Download HH exchange data
  HH <- getDATRAS(record = "HH", survey, years, quarters)
  
  #Make -9/0 into NA
  HH[HH == 0] <- NA
  HH[HH == -9] <- NA
  
  #Make a call_distance column that include calculations for NA values
  
  deg2rad <- function(deg) {(deg * pi) / (180)}
  
  HH <- HH %>%
    filter(HaulVal == "V") %>%
    mutate(
      Cal_Distance = ifelse(!is.na(Distance), 
                            Distance,
                            ifelse(!is.na(GroundSpeed),
                                   HaulDur/60*1852*GroundSpeed,
                                   1.852 * 360 * 60 / 2 * pi*(acos(cos(deg2rad(ShootLat)) * cos(deg2rad(HaulLat)) * cos(deg2rad(HaulLong) - deg2rad(ShootLong)) + sin(deg2rad(ShootLat)) * sin(deg2rad(HaulLat)))))))
  
  data <- HH %>% filter(Country == "FR", Year == 2004:2021)
  country <- "FR"
  
  source("utilities.R")
  
  data <- calculate_DS_WS(data)
  
  file_prefix <- paste0(survey,"FR_Flexfile",cal_date)
  write.csv(data,paste0(file_prefix, ".csv")) 
  
  data <- HH %>% filter(Country =="GB-SCT", Year == 2005:2021)
  country <- "GB-SCT"
  
  source("utilities.R")
  
  data <- calculate_DS_WS(data)
  
  file_prefix <- paste0(survey,"SCT_Flexfile",cal_date)
  write.csv(data,paste0(file_prefix, ".csv")) 
  
  data <- HH %>% filter(Country == "DK", Year == 2004:2021)
  country <- "DK"
  
  source("utilities.R")
  
  data <- calculate_DS_WS_sweeps(data)
  
  file_prefix <- paste0(survey,"DK_Flexfile",cal_date)
  write.csv(data,paste0(file_prefix, ".csv")) 
  
  data <- HH %>% filter(Country == "SE", Year == 2004:2021)
  
  source("utilities.R")
  
  data <- calculate_DS_WS_sweeps(data)
  
  file_prefix <- paste0(survey,"SE_Flexfile",cal_date)
  write.csv(data,paste0(file_prefix, ".csv")) 
  
  data_all <- HH
  
  source("utilities.R")
  
  data_by <- by(data_all, data_all$Country, calculate_DS_WS_nsibts)
  
  data_combined <-
    do.call(
      rbind,
      data_by
    )
  
  file_prefix <- paste0(survey,"NL_GB_DE_Flexfile",cal_date)
  write.csv(data_combined,paste0(file_prefix, ".csv")) 
  
}


# Here I will try to standardize surveys with similar rules with SweepLength

for(n in 10:13){
  survey <- survey_list[[n]]
  years <- 2016:2021
  #Download HH exchange data
  HH <- getDATRAS(record = "HH", survey, years, quarters)
  
  #Make -9/0 into NA
  HH[HH == 0] <- NA
  HH[HH == -9] <- NA
  
  #Make a call_distance column that include calculations for NA values
  
  deg2rad <- function(deg) {(deg * pi) / (180)}
  
  HH <- HH %>%
    filter(HaulVal == "V") %>%
    mutate(
      Cal_Distance = ifelse(!is.na(Distance), 
                            Distance,
                            ifelse(!is.na(GroundSpeed),
                                   HaulDur/60*1852*GroundSpeed,
                                   1.852 * 360 * 60 / 2 * pi*(acos(cos(deg2rad(ShootLat)) * cos(deg2rad(HaulLat)) * cos(deg2rad(HaulLong) - deg2rad(ShootLong)) + sin(deg2rad(ShootLat)) * sin(deg2rad(HaulLat)))))))
  data <- HH
  
  source("utilities.R")
  
  data <- calculate_DS_WS_sweeps(data)
  
  file_prefix <- paste0(survey,"Flexfile",cal_date)
  write.csv(data,paste0(file_prefix, ".csv")) 
}
  
