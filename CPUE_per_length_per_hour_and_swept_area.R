##############################################
#Procedures to calculate DATRAS data products:
# CPUE per length per hour and Swept Area
#############################################
#Authors: Vaishav Soni and Adriana Villamor
#June 2022
#Update of the procedure to take into account Gear Exception = Double Beam (DB).
# In those cases, swept area would be double

#This script mimics the SQL procedure and provides the same results.


library(icesDatras)
library(dplyr)


# Select survey, year and quarter
year <- 2014
survey <- "BTS"
quarter <- 3

#Download HH and HL
HH <- getHHdata(survey, year, quarter)
HL <- getHLdata(survey, year, quarter)


# Transform LngtClass with LngtCode "1" and "5" (in cm) to mm

HL<-rbind(HL%>%filter(LngtCode%in%c("1", "5"))%>%mutate(LngtClass=LngtClass*10),
                  HL%>%filter(!LngtCode%in%c("1", "5")))

#Join the two Record Types, only Valid Hauls

HH <- HH %>% select(-(RecordType)) %>% select(-(DateofCalculation))  
HL <- HL %>% select(-(RecordType)) %>% select(-(DateofCalculation))

df <- left_join(HL,HH)%>%
        filter(HaulVal =="V")

# Beam width is the number stated in Gear, so we extract that number into a new variable Beam_width

df$Beam_width <- substr(df$Gear, start = 3, stop = 3)
# unique(df$Beam_width)

#substitute -9 with NA
df[df == -9] <- NA


#When distance is NA, then calculate it as 1853*HaulDur/60

# sum(is.na(df$Distance))

df <- transform(df, DeriveDistance = ifelse(!is.na(Distance), Distance, (1853*HaulDur)/60)) 

#Calculate swept area as Distance* Beam_width
df <- df %>% mutate(SweptArea_m2 = as.numeric(DeriveDistance) * as.numeric(Beam_width))

df$SweptArea_m2 <- as.integer(df$SweptArea_m2)

#Multiply the swept area when there is a double beam

df <- mutate(df, SweptArea_m2bis = ifelse(GearEx != "DB", NA, (SweptArea_m2*2)))
df <- mutate(df,SweptArea_m2 = ifelse(!is.na(SweptArea_m2bis), SweptArea_m2bis,SweptArea_m2))

#Remove the temporal variable
df <- df[, -87]

#To kilometers 
df <- df %>% mutate(SweptArea_km2 = SweptArea_m2/1000000)

#Substitute NA in SubFactor with 1, for next multiplications
df$SubFactor[is.na(df$SubFactor)] <- 1

#For DataType R or S, Transform HLNoAtLngt as follows:
df1 <- df%>% filter(DataType %in% c("S", "R"))%>% mutate(NoPerHaul=HLNoAtLngt*60/HaulDur)

#For DataType C, HLNoAtLngt remains the same
df2 <- df%>% filter(DataType == "C")%>% mutate(NoPerHaul=HLNoAtLngt)

#Merge these two dataframes
df <- rbind(df1,df2)

#CPUE_numbers_per_hour
df <- transform(df, CPUE_number_per_hour = ifelse(!is.na(NoPerHaul), NoPerHaul * SubFactor, 0))

#CPUE_number_per_km2
df <- df %>% mutate(CPUE_number_per_km2 = (HLNoAtLngt * SubFactor)/SweptArea_km2)

#extract a small piece of the dataframe to check results
ned <- df%>% filter(Country == "NED", Ship=="TRI2", SpecCode == "105814", LngtClass == 620)
