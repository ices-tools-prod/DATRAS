# This function mimics the one used internally to update swept area whenever
# a new submission happens in DATRAS. The main difference is that the input parameters are not given
# and the HH data should also be defined by the user
# A file with the most updated algorithms is attached to this script
#############################################
# Authors: Adriana Villamor, Vaishav Soni and Cecilia Kvaavik 
# August 2023

library(icesDatras)
library(dplyr)
library(dbplyr)
library(tidyr)
# library(RODBC)

#### Define the Submissions to be updated. In the internal procedure this comes
# from the info in the database

# It is a dataframe with variables: Survey, Quarter, Year, Ship and Country.
# Attached one example for info

params <- params_march23

####Define the most updated algorithms. In the internal procedure this are hosted in the database

algorithms <- algorithms_work_revNov22
algorithms <- algorithms %>% separate_rows(Quarter)

## Start loop for each set of params

res <- data.frame()

for(i in 1:nrow(params)) { 
# for(i in 4){        
        survey <- noquote(params$Survey[i])
        country <- noquote(params$Country[i])
        country <- gsub(" ", "", country)
        year <- as.numeric(params$Year[i])
        ship <- noquote(params$Ship[i])
        ship <- gsub(" ", "", ship)
        ship <- as.character(ship)
        quarter <- params$Quarter[i]

HH <- icesDatras::getHHdata(survey, year, quarter)
HH <- HH %>% filter(Ship %in%ship)
HH <- HH %>% filter(Country %in%country)
# HH$Ship <- ship
HH[HH == -9] <- NA

##Make a Cal_distance column that include calculations for NA values

deg2rad <- function(deg) {(deg * pi) / (180)}

##Vaishav, we get a space after V, can we remove it from the database?
# No, but from web service there is no space
data <- HH %>%
  filter(HaulVal == "V") %>%
  mutate(
  Cal_Distance = ifelse(!is.na(Distance), 
                         Distance,
                         ifelse(!is.na(GroundSpeed),
                                HaulDur/60*1852*GroundSpeed,
                                1.852 * 360 * 60 / 2 * pi*(acos(cos(deg2rad(ShootLat)) * cos(deg2rad(HaulLat)) * cos(deg2rad(HaulLong) - deg2rad(ShootLong)) + sin(deg2rad(ShootLat)) * sin(deg2rad(HaulLat)))))))


## Two options, Beam Trawl surveys and the rest:

if(survey %in% c("BTS-GSA17", "BTS", "DYFS", "BTS-VIII", "SNS")){
        
        data$Cal_DoorSpread <- NA
        data$Cal_WingSpread <- NA
        data$BeamWidth <- substr(data$Gear, start = 3, stop = 3)
        # unique(df$Beam_width)
        
        #Calculate swept area as Cal_Distance* Beam_width
        data <- data %>% mutate(SweptArea_m2 = as.numeric(Cal_Distance) * as.numeric(BeamWidth))
        
        data$SweptArea_m2 <- as.integer(data$SweptArea_m2)
        
        #Multiply the swept area when there is a double beam
        
        data <- mutate(data, SweptArea_m2bis = ifelse(GearExp != "DB", NA, (SweptArea_m2*2)))
        data <- mutate(data,SweptArea_m2 = ifelse(!is.na(SweptArea_m2bis), SweptArea_m2bis,SweptArea_m2))
        
        #Remove the temporal variable, check the number of variable
        data <- data[, -75]
        
        #To kilometers 
        data <- data %>% mutate(SweptArea_km2 = SweptArea_m2/1000000)
        data$SweptAreaDSKM2 <- NA
        data$SweptAreaWSKM2 <- NA
        data$SweptAreaBWKM2 <- data$SweptArea_km2 
        data <- data[, -c(74:75)]
        
}

#for non beam surveys

else{

##Calculating missing DoorSpread and Wingspread

#first filter the rows of formulas for that survey, country, quarter, year

  # need to put ship logic and current year function
  
        subset <- algorithms %>% filter(Country== country, Quarter == quarter, Survey == survey)
        subset$FinalYear[which(subset$FinalYear == "current year")] <- noquote(substr(Sys.Date(),1,4))
        # subset$year <- year
        subset<- subset %>% filter(year >= InitialYear, year <= FinalYear)
        
# select the priority 1 for DoorSpread
        formulas <- subset %>% filter(Priority == 1)
        formulas <- as.data.frame(formulas)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
        #run one of the formulas:
        
        if(nrow(formulas)> 1){
                source("utilities_v2.R")
                data <- calculate_DS_sweeps_P1(data)
        }
        
        if(nrow(formulas)== 1){
                source("utilities_v2.R")
                data <- calculate_DS_P1(data)
        }

# select the priority 2 for DoorSpread
        formulas <- subset %>% filter(Priority == 2)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
        #run one of the formulas:
        
        if(nrow(formulas)> 1){
                source("utilities_v2.R")
                data <- calculate_DS_sweeps_P2(data)
        }
        
        if(nrow(formulas)== 1){
                source("utilities_v2.R")
                data <- calculate_DS_P2(data)
        }
        # if(nrow(formulas)< 1){
        #         data <-data
        # }
        
 # select the priority 3 for DoorSpread (only in GB NS-IBTS)
        formulas <- subset %>% filter(Priority == 3)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
        if(nrow(formulas)> 0){
        source("utilities_v2.R")
        data <- calculate_DS_P2(data)
        }
        
# select the priority 1 for WingSpread
        formulas <- subset %>% filter(Priority == 1)
        formulas <- formulas %>% filter(formulas$x == "WingSpread" | formulas$x=="Wingspread")
        #run one of the formulas:
        
        if(nrow(formulas)> 1){
                source("utilities_v2.R")
                data <- calculate_WS_sweeps_P1(data)
        }
        
        if(nrow(formulas)== 1){
                source("utilities_v2.R")
                data <- calculate_WS_P1(data)
        }
        
# select the priority 2 for WingSpread
        formulas <- subset %>% filter(Priority == 2)
        formulas <- formulas %>% filter(formulas$x == "WingSpread" | formulas$x=="Wingspread")
        #run one of the formulas:
        
        if(nrow(formulas)> 1){
                source("utilities_v2.R")
                data <- calculate_WS_sweeps_P2(data)
        }
        
        if(nrow(formulas)== 1){
                source("utilities_v2.R")
                data <- calculate_WS_P2(data)
        } 
        
# select the priority 3 for WingSpread (only in GB NS-IBTS)
        formulas <- subset %>% filter(Priority == 3)
        formulas <- formulas %>% filter(formulas$x == "WingSpread" | formulas$x=="Wingspread")
        if(nrow(formulas)>0){
        source("utilities_v2.R")
        data <- calculate_WS_P2(data)
        }

## Calculate DS and WS swept area in km2
        
        if(nrow(subset)==0){
                data <- data %>%
                        mutate(
                                Cal_DoorSpread = DoorSpread,
                                Cal_WingSpread = WingSpread)
        }
        
        data$BeamWidth <- NA
        
        data <- data %>%
                mutate(
                        SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
                        SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000) 
        data$SweptAreaBWKM2 <- NA

            
}#end of function for non-beam surveys

res <- rbind(res,data)
res
#close loop for all params in the list

}

## Add Calculated and Observed flags, plus some house keeping to upload to datras

data <- res
data[is.na(data)]<- "-9" 
data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))

data <- data %>%
        mutate(DSflag = ifelse(SurveyName %in% c("BTS-GSA17", "BTS", "DYFS", "BTS-VIII"), "-9", DSflag))

data <- data %>%
        mutate(WSflag = ifelse(SurveyName %in% c("BTS-GSA17", "BTS", "DYFS", "BTS-VIII"), "-9", WSflag))

today <- format(Sys.Date(), format="%Y-%d-%m")
data$DateofCalculation <- gsub("-","",today) 
data$Cal_Distance <- as.numeric(data$Cal_Distance)
data$Cal_DoorSpread <- as.numeric(data$Cal_DoorSpread)
data$Cal_WingSpread <- as.numeric(data$Cal_WingSpread)  
data$BeamWidth <- as.numeric(data$BeamWidth)
data$SweptAreaDSKM2 <- as.numeric(data$SweptAreaDSKM2)
data$SweptAreaWSKM2 <- as.numeric(data$SweptAreaWSKM2)
data$SweptAreaBWKM2 <- as.numeric(data$SweptAreaBWKM2)

data <- data[,-1]
data$Survey <- data$SurveyName
data <- data[,-68]
data <- data %>% relocate(Survey, .before = Quarter)
data <- data %>% relocate(DSflag, .before = SweptAreaDSKM2)
data <- data %>% relocate(WSflag, .before = SweptAreaDSKM2)
data <- data %>% relocate(DistanceFlag, .before = SweptAreaDSKM2)
data <- data %>% 
        rename(
                GearEx = GearExp,
                Month = month,
                DepthStratum = Stratum,
                BySpecRecCode= BycSpecRecCode
        )

data <- unique(data)



