##############################################
#Procedures to calculate DATRAS data products:
# CPUE per length per hour and Swept Area
#############################################
#Authors: Vaishav Soni and Adriana Villamor
#March 2019

library(icesDatras)
library(dplyr)

year <- 2018
survey <- "BTS"
quarter <- 3


HH <- getHHdata(survey, year, quarter)
HL <- getHLdata(survey, year, quarter)


# Transform LngtClass with LngtCode "." and "0" to cm

HL<-rbind(HL%>%filter(LngtCode%in%c(".", "0"))%>%mutate(LngtClass=LngtClass/10),
                  HL%>%filter(!LngtCode%in%c(".", "0")))

#Join the two Record Types, only Valid Hauls

HH <- HH %>% select(-(RecordType)) %>% select(-(DateofCalculation))  
HL <- HL %>% select(-(RecordType)) %>% select(-(DateofCalculation))

df <- left_join(HL, HH)%>%
        filter(HaulVal =="V")

# Beam width is the number stated in Gear, so we extract that number into a new variable Beam_width

df$Beam_width <- substr(df$Gear, start = 3, stop = 3)

#substitue -9
df[df == -9] <- NA

#In case distance is NA, then calculate it


sum(is.na(df$Distance))

df <- transform(df, DeriveDistance = ifelse(!is.na(Distance), Distance, (1853*HaulDur)))


df <- df %>% mutate(SweptArea_m2 = as.numeric(DeriveDistance) * as.numeric(Beam_width))
df <- df %>% mutate(SweptArea_km2 = SweptArea_m2/1000000)

df <- transform(df, CPUE_number_per_hour = ifelse(!is.na(HLNoAtLngt), HLNoAtLngt * SubFactor, 0))
# df <- df %>% mutate(CPUE_number_per_hour = HLNoAtLngt * SubFactor)

df <- df %>% mutate(CPUE_number_per_km2 = CPUE_number_per_hour/SweptArea_km2)
