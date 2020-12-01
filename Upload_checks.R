# This script will be used to do routine checks when updating the DW, so we try to 
# better assure that what has been uploaded is what can be downloaded.
# The idea would be to run this procedure after all countries have uploaded and before the
# WGs meet, in order to detect errors. 
# Could be related to the submission deadlines

# Adriana Villamor and Vaishav Soni
# October 2019

library(RODBC)
library(dplyr)
library(data.table)
library(ggplot2)
library(icesDatras)

###############################################################
# First I retrieve all info in the exchange file from DATRAS db. 
###############################################################

#### Connect to database
# settings
dbConnect_datras <- 'Driver={SQL Server};Server=SQL06;Database=DATRAS;Trusted_Connection=yes'

# connect
conn_datras <- odbcDriverConnect(connection = dbConnect_datras)

#### Input information, codes can be checkes in tblCodes
survey <- '2341'
year <- 2020
quarter <- 3

# Country will only be selected in some cases, it is preferable to check all quarter
# data in one go
# country <- '4711' 


#### Get UploadID and numbers of records for a given survey, year, quarter  

 sqlq <- sprintf("SELECT UploadID, SessionID, Datetime, Survey, Year, Quarter, Country, User, Update_Warehouse, TotalHH, TotalHL, TotalCA
                 FROM [DATRAS].[dbo].[A_Datras_tblUploadLog]
                 WHERE Survey = '%s' AND  Year = '%s' AND Quarter = '%s'
                 ORDER BY UploadID desc",
                 survey, year, quarter)

# to get all survey DYFS info
#sqlq <- sprintf("SELECT UploadID, SessionID, Datetime, Survey, Year, Quarter, Country, User, Update_Warehouse, TotalHH, TotalHL, TotalCA
#                FROM [DATRAS].[dbo].[A_Datras_tblUploadLog]
#                WHERE Survey = '%s'"
#                , survey)



#WHEN COUNTRY IS FILTERED USE THIS ONE

# sqlq <- sprintf("SELECT UploadID, SessionID, Datetime, Survey, Year, Quarter, Country, User, Update_Warehouse, TotalHH, TotalHL, TotalCA
#                 FROM [DATRAS].[dbo].[A_Datras_tblUploadLog]
#                 WHERE Survey = '%s' AND  Year = '%s' AND Quarter = '%s' AND Country = '%s'
#                 ORDER BY UploadID desc"
#                 , survey, year, quarter, country)

upload <- sqlQuery(conn_datras, sqlq)

#remove raws with empty fields

upload <- upload[complete.cases(upload), ]


# upload <- upload %>% distinct(Country, .keep_all = TRUE)

# wHEN COUNTRY IS FILTERED
# upload <- upload[which.max(upload$UploadID),]


####Get HH, HL and CA for this UploadID

uploadID <- upload$UploadID
uploadID <- as.character(uploadID)

#THIS IS THE PROBLEM, need to parse UploadID one by one, not in a range

from <- min(uploadID)
from <- as.character(from)
to <- max(uploadID)
to <- as.character(to)

#Now archive is in DATRAS_Request

# dbConnect_datras_req <- 'Driver={SQL Server};Server=SQL06;Database=DATRAS_Request;Trusted_Connection=yes'

# connect
# conn_datras_req <- odbcDriverConnect(connection = dbConnect_datras_req)

# sqlTables(conn_datras_req)


sqlq <- sprintf("SELECT UploadID
                ,RecordType
                ,Quarter
                ,Country
                ,Ship
                ,Gear
                ,SweepLngt
                ,GearExp
                ,DoorType
                ,StNo
                ,HaulNo
                ,Year
                ,SpecCodeType
                ,SpecCode
                ,AreaType
                ,AreaCode
                ,LngtCode
                ,LngtClass
                ,Sex
                ,Maturity
                ,PlusGr
                ,AgeRings
                ,CANoAtLngt
                ,IndWgt
                ,MaturityScale
                ,FishID
                ,GenSamp
                ,StomSamp       
                ,AgeSource      
                ,AgePrepMet
                ,OtGrading
                ,ParSamp
                ,LineNumber
                FROM [DATRAS].[dbo].[A_Datras_Archive_CA]
                WHERE UploadID BETWEEN '%s' AND '%s'", from, to)
CA <- sqlQuery(conn_datras, sqlq)
CA <- CA %>% filter(UploadID %in% uploadID)

sqlq <- sprintf("SELECT [UploadID]
                ,[RecordType]
                ,[Quarter]
                ,[Country]
                ,[Ship]
                ,[Gear]
                ,[SweepLngt]
                ,[GearExp]
                ,[DoorType]
                ,[StNo]
                ,[HaulNo]
                ,[Year]
                ,[Month]
                ,[Day]
                ,[TimeShot]
                ,[Stratum]
                ,[HaulDur]
                ,[DayNight]
                ,[ShootLat]
                ,[ShootLong]
                ,[HaulLat]
                ,[HaulLong]
                ,[StatRec]
                ,[Depth]
                ,[HaulVal]
                ,[HydroStNo]
                ,[StdSpecRecCode]
                ,[BycSpecRecCode]
                ,[DataType]
                ,[Netopening]
                ,[Rigging]
                ,[Tickler]
                ,[Distance]
                ,[Warplngt]
                ,[Warpdia]
                ,[WarpDen]
                ,[DoorSurface]
                ,[DoorWgt]
                ,[DoorSpread]
                ,[WingSpread]
                ,[Buoyancy]
                ,[KiteDim]
                ,[WgtGroundRope]
                ,[TowDir]
                ,[GroundSpeed]
                ,[SpeedWater]
                ,[SurCurDir]
                ,[SurCurSpeed]
                ,[BotCurDir]
                ,[BotCurSpeed]
                ,[WindDir]
                ,[WindSpeed]
                ,[SwellDir]
                ,[SwellHeight]
                ,[SurTemp]
                ,[BotTemp]
                ,[SurSal]
                ,[BotSal]
                ,[ThermoCline]
                ,[CodendMesh]
                ,[SecchiDepth]
                ,[Turbidity]
                ,[TidePhase]
                ,[TideSpeed]
                ,[PelSampType]
                ,[MinTrawlDepth]
                ,[MaxTrawlDepth]
                ,[ThClineDepth]
                ,[LineNumber]
                FROM [DATRAS].[dbo].[A_Datras_Archive_HH]
                WHERE UploadID BETWEEN '%s' AND '%s'", from, to)
HH <- sqlQuery(conn_datras, sqlq)
HH <- HH %>% filter(UploadID %in% uploadID)

sqlq <- sprintf("SELECT [UploadID]
      ,[RecordType]
                ,[Quarter]
                ,[Country]
                ,[Ship]
                ,[Gear]
                ,[SweepLngt]
                ,[GearExp]
                ,[DoorType]
                ,[StNo]
                ,[HaulNo]
                ,[Year]
                ,[SpecCodeType]
                ,[SpecCode]
                ,[SpecVal]
                ,[Sex]
                ,[TotalNo]
                ,[CatIdentifier]
                ,[NoMeas]
                ,[SubFactor]
                ,[SubWgt]
                ,[CatCatchWgt]
                ,[LngtCode]
                ,[LngtClass]
                ,[HLNoAtLngt]
                ,[DevStage]
                ,[LenMeasType]
                ,[LineNumber]
                FROM [DATRAS].[dbo].[A_Datras_Archive_HL]
                WHERE UploadID BETWEEN '%s' AND '%s'", from, to)
HL <- sqlQuery(conn_datras, sqlq)
HL <- HL %>% filter(UploadID %in% uploadID)
names(HL)

HL <- HL[, -grep("LineNumber", colnames(HL))]
HL <- HL[, -grep("UploadID", colnames(HL))]

names(HL)

#### In Archive_HH only the standard fields are stored, we need to get also

# In tblGearDetail
# CodendMesh

# In tblHaul:
# PelSampType
# MinTrawlDepth
# MaxTrawlDepth

# In tblAncillary
# SecchiDepth
# Turbidity
# TidePhase
# TideSpeed


#### Get TrawlCruiseID for this upload

sqlq <- sprintf("SELECT [tblTrawlCruiseID]
                ,[Survey]
                ,[Quarter]
                ,[Year]
                ,[Country]
                ,[Insert_Date]
                FROM [DATRAS].[dbo].[tblTrawlCruise]
                WHERE Survey = '%s' AND  Year = '%s' AND Quarter = '%s'", survey, year, quarter)

#WHEN COUNTRY IS FILTERED USE THIS ONE

# sqlq <- sprintf("SELECT [tblTrawlCruiseID]
#                 ,[Survey]
#                 ,[Quarter]
#                 ,[Year]
#                 ,[Country]
#                 ,[Insert_Date]
#                 FROM [DATRAS].[dbo].[tblTrawlCruise]
#                 WHERE Survey = '%s' AND  Year = '%s' AND Quarter = '%s' AND Country = '%s'"
#                 , survey, year, quarter, country)

TrawlCruise <- sqlQuery(conn_datras, sqlq)

trawlcruiseID <- TrawlCruise$tblTrawlCruiseID
trawlcruiseID <- as.character(trawlcruiseID)

from <- min(trawlcruiseID)
from <- as.character(from)
to <- max(trawlcruiseID)
to <- as.character(to)

#### With that TrawlCruiseID we get the HaulIDs keys, in tblHaulID (and some new fields)

sqlq <- sprintf("SELECT [tblHaulID]
                ,[tblTrawlCruiseID]
                ,[ShootLat]
                ,[Depth]
                ,[HaulLat]
                ,[HaulNo]
                ,[PelSampType]
                ,[MinTrawlDepth]
                ,[MaxTrawlDepth]
                FROM [DATRAS].[dbo].[tblHaul]
                WHERE tblTrawlCruiseID BETWEEN '%s' AND '%s'", from, to)

## Second piece of HH
HH_2 <- sqlQuery(conn_datras, sqlq)
HH_2 <- HH_2 %>% filter(tblTrawlCruiseID %in% trawlcruiseID)

# Define range of HaulIDs
haulID <- unique(HH_2$tblHaulID)
from <- min(haulID)
from <- as.character(from)
to <- max(haulID)
to <- as.character(to)

#### With those HaulIDs, get other variable for HH in tblGearDetail
sqlq <- sprintf("SELECT [tblGearDetailID]
                ,[tblHaulID]
                ,[CodendMesh]
                FROM [DATRAS].[dbo].[tblGearDetail]
                WHERE tblHaulID BETWEEN '%s' AND '%s'", from, to)

## Third piece of HH
HH_3 <- sqlQuery(conn_datras, sqlq)
HH_3 <- HH_3 %>% filter(tblHaulID %in% haulID)

#### With the same HaulIDs, get the remaining variables for HH
sqlq <- sprintf("SELECT [tblAncillaryID]
                ,[tblHaulID]
                ,[SecchiDepth]
                ,[Turbidity]
                ,[TidePhase]
                ,[TideSpeed]
                FROM [DATRAS].[dbo].[tblAncillary]
                WHERE tblHaulID BETWEEN '%s' AND '%s'", from, to)

## Fourth piece of HH
HH_4 <- sqlQuery(conn_datras, sqlq)
HH_4 <- HH_4 %>% filter(tblHaulID %in% haulID)

HH <- left_join(HH, HH_2)
HH <- left_join(HH,HH_3)
HH <- left_join(HH,HH_4)

names(HH)
HH <- HH[, -grep("tbl", colnames(HH))]
HH <- HH[, -grep("LineNumber", colnames(HH))]
HH <- HH[, -grep("UploadID", colnames(HH))]
names(HH)
#67

#### In Archive_CA only the standard fields are stored, I have to retrieve also
# In tblALK:
# FishID
# GenSamp
# StomSamp
# AgeSource
# AgePrepMet
# OtGrading

# First we need the SpeciesID linked to the HaulID 

sqlq <- sprintf("SELECT [tblSpeciesID]
                ,[tblHaulID]
                ,[SpecCode]
                FROM [DATRAS].[dbo].[tblSpecies]
                WHERE tblHaulID BETWEEN '%s' AND '%s'", from, to)

SpeciesID <- sqlQuery(conn_datras, sqlq)
SpeciesID <- SpeciesID %>% filter(tblHaulID %in% haulID)

sqlq <- sprintf("SELECT [tblCodeID]
                ,[Code]
                FROM [DATRAS].[dbo].[tblCode] where tblCodeGroupID = '247'")

specCode <- sqlQuery(conn_datras, sqlq)

# SpeciesID <- left_join(SpeciesID, specCode)
colnames(SpeciesID)[3] <- "tblCodeID"
SpeciesID <- left_join(SpeciesID, specCode)
colnames(SpeciesID)[3] <- "SpecCode"

# Define range of SpeciesIDs
speciesID <- unique(SpeciesID$tblSpeciesID)
#speciesID <- as.character(speciesID)
from <- min(speciesID)
from <- as.character(from)
to <- max(speciesID)
to <- as.character(to)

sqlq <- sprintf("SELECT [tblALKID]
                ,[tblSpeciesID]
                ,[LngtClasMM]
                ,[Sex]
                ,[Maturity]
                ,[Age]
                ,[IndividualWeight]
                ,[LngtCode]
                ,[PlusGr]
                ,[NoAtALK]
                ,[LngtClas]
                ,[FishID]
                ,[GenSamp]
                ,[StomSamp]
                ,[AgeSource]
                ,[AgePrepMet]
                ,[OtGrading]
                FROM [DATRAS].[dbo].[tblALK]
                WHERE tblSpeciesID BETWEEN '%s' AND '%s'", from, to)

CA_2 <- sqlQuery(conn_datras, sqlq)
CA_2 <- CA_2 %>% filter (tblSpeciesID %in% speciesID)
CA_2 <- left_join(CA_2, SpeciesID)
CA_2 <- CA_2 %>% select(-SpecCode)
names(CA_2)
colnames(CA_2)[19] <- "SpecCode"

CA_2$Sex <- as.factor(CA_2$Sex)
CA_2$Maturity <- as.factor(CA_2$Maturity)
CA$Maturity <- as.factor(CA$Maturity)
CA_2$LngtCode <- as.factor(CA_2$LngtCode)
CA_2$PlusGr <- as.factor(CA_2$PlusGr)
CA$FishID <- as.character(CA$FishID)
CA_2$GenSamp <- as.character(CA_2$GenSamp)
CA_2$StomSamp <- as.character(CA_2$StomSamp)
CA_2$AgeSource <- as.character(CA_2$AgeSource)
str(CA)
str(CA_2)
CA_2$LngtCode <- as.factor(CA_2$LngtCode)
CA_2$AgePrepMet <- as.character(CA_2$AgePrepMet)
CA_2$FishID <- as.character(CA_2$FishID)
CA$PlusGr <- as.character(CA$PlusGr)
CA$GenSamp <- as.character(CA$GenSamp)
CA <- left_join(CA, CA_2)
names(CA)
CA <- CA[, -grep("tbl", colnames(CA))]
CA <- CA[, -grep("LineNumber", colnames(CA))]
CA <- CA[, -grep("UploadID", colnames(CA))]
CA <- CA[, -grep("LngtClasMM", colnames(CA))]
# CA <- CA[, -grep("LngtClas", colnames(CA))]
CA <- CA[, -grep("NoAtALK", colnames(CA))]
CA <- CA[, -grep("IndividualWeight", colnames(CA))]
names(CA)
CA <- CA[, -c(32:33)]
names(CA)
#31 variables
## perfectooo

###############################################################
# Now we get the same file from the download, DW_DATRAS. 
###############################################################

dbConnect_dwdatras <- 'Driver={SQL Server};Server=SQL06;Database=DW_DATRAS;Trusted_Connection=yes'
conn_dwdatras <- odbcDriverConnect(connection = dbConnect_dwdatras)

haulID <- unique(HH_2$tblHaulID)
from <- min(haulID)
from <- as.character(from)
to <- max(haulID)
to <- as.character(to)

sqlq <- sprintf("SELECT [tbltrawlcruiseID]
      ,[tblHaulID]
                ,[Survey]
                ,[RecordType]
                ,[Quarter]
                ,[Country]
                ,[Ship]
                ,[ShipID]
                ,[Gear]
                ,[SweepLngt]
                ,[GearExp]
                ,[DoorType]
                ,[StNo]
                ,[HaulNo]
                ,[Year]
                ,[month]
                ,[Day]
                ,[TimeShot]
                ,[Stratum]
                ,[HaulDur]
                ,[DayNight]
                ,[ShootLat]
                ,[ShootLong]
                ,[HaulLat]
                ,[HaulLong]
                ,[StatRec]
                ,[Depth]
                ,[HaulVal]
                ,[HydroStNo]
                ,[StdSpecRecCode]
                ,[BycSpecRecCode]
                ,[DataType]
                ,[Netopening]
                ,[Rigging]
                ,[Tickler]
                ,[Distance]
                ,[Warplngt]
                ,[Warpdia]
                ,[WarpDen]
                ,[DoorSurface]
                ,[DoorWgt]
                ,[DoorSpread]
                ,[WingSpread]
                ,[Buoyancy]
                ,[KiteDim]
                ,[WgtGroundRope]
                ,[TowDir]
                ,[GroundSpeed]
                ,[SpeedWater]
                ,[SurCurDir]
                ,[SurCurSpeed]
                ,[BotCurDir]
                ,[BotCurSpeed]
                ,[WindDir]
                ,[WindSpeed]
                ,[SwellDir]
                ,[SwellHeight]
                ,[SurTemp]
                ,[BotTemp]
                ,[SurSal]
                ,[BotSal]
                ,[ThermoCline]
                ,[ThClineDepth]
                ,[Cal_DateID]
                ,[CodendMesh]
                ,[SecchiDepth]
                ,[Turbidity]
                ,[TidePhase]
                ,[TideSpeed]
                ,[PelSampType]
                ,[MinTrawlDepth]
                ,[MaxTrawlDepth]
                ,[ICESArea]
                FROM [DW_DATRAS].[dbo].[A_DatrasExchange_HH]
                WHERE tblHaulID BETWEEN '%s' AND '%s'", from, to)

DW_HH <- sqlQuery(conn_dwdatras, sqlq)
DW_HH <- DW_HH %>% filter(tblHaulID %in% haulID)
# names(DW_HH)
# names(HH)

DW_HH <- DW_HH[, -grep("tbl", colnames(DW_HH))]
DW_HH <- DW_HH[, -grep("Survey", colnames(DW_HH))]
DW_HH <- DW_HH[, -grep("ShipID", colnames(DW_HH))]
DW_HH <- DW_HH[, -grep("Cal_DateID", colnames(DW_HH))]
DW_HH <- DW_HH[, -grep("ICESArea", colnames(DW_HH))]
names(DW_HH)
colnames(DW_HH)[12] <- "Month"

data.frame(table(unlist(HH$Year)))
data.frame(table(unlist(DW_HH$Year)))

data.frame(table(unlist(HH$Country)))
data.frame(table(unlist(DW_HH$Country)))



#############
# Same for CA
#############

sqlq <- sprintf("SELECT [tblHaulID]
                ,[Survey]
                ,[RecordType]
                ,[Quarter]
                ,[Country]
                ,[Ship]
                ,[ShipID]
                ,[Gear]
                ,[SweepLngt]
                ,[GearExp]
                ,[DoorType]
                ,[StNo]
                ,[HaulNo]
                ,[Year]
                ,[SpecCodeType]
                ,[SpecCode]
                ,[AreaType]
                ,[AreaCode]
                ,[LngtCode]
                ,[LngtClas]
                ,[Sex]
                ,[Maturity]
                ,[PlusGr]
                ,[Age]
                ,[NoAtALK]
                ,[IndWgt]
                ,[Cal_DateID]
                ,[tblcodeidAphiaFn]
                ,[tblcodeidAphia]
                ,[MaturityScale]
                ,[FishID]
                ,[GenSamp]
                ,[StomSamp]
                ,[AgeSource]
                ,[AgePrepMet]
                ,[OtGrading]
                ,[Valid_Aphia]
                ,[ScientificName_WoRMS]
                ,[ParSamp]
                FROM [DW_DATRAS].[dbo].[A_DatrasExchange_CA]
                WHERE tblHaulID BETWEEN '%s' AND '%s'", from, to)

DW_CA <- sqlQuery(conn_dwdatras, sqlq) 
DW_CA <- DW_CA %>% filter(tblHaulID %in% haulID)
names(DW_CA)
# names(CA)
DW_CA <- DW_CA[, -grep("tbl", colnames(DW_CA))]
DW_CA <- DW_CA[, -grep("Survey", colnames(DW_CA))]
DW_CA <- DW_CA[, -grep("ShipID", colnames(DW_CA))]
DW_CA <- DW_CA[, -grep("Cal_DateID", colnames(DW_CA))]
DW_CA <- DW_CA[, -grep("Valid_Aphia", colnames(DW_CA))]
DW_CA <- DW_CA[, -grep("ScientificName", colnames(DW_CA))]
names(DW_CA)

colnames(DW_CA)[21] <- "AgeRings"
colnames(DW_CA)[17] <- "LngtClass"
colnames(DW_CA)[22] <- "CANoAtLngt"
# colnames(DW_CA)[24] <- "MaturityScale"

data.frame(table(unlist(CA$Year)))
data.frame(table(unlist(DW_CA$Year)))

data.frame(table(unlist(CA$Country)))
data.frame(table(unlist(DW_CA$Country)))



#############
# Same for HL
#############

sqlq <- sprintf("SELECT [tblHaulID]
                ,[Survey]
                ,[RecordType]
                ,[Quarter]
                ,[Country]
                ,[Ship]
                ,[ShipID]
                ,[Gear]
                ,[SweepLngt]
                ,[GearExp]
                ,[DoorType]
                ,[StNo]
                ,[HaulNo]
                ,[Year]
                ,[SpecCodeType]
                ,[SpecCode]
                ,[SpecVal]
                ,[Sex]
                ,[TotalNo]
                ,[CatIdentifier]
                ,[NoMeas]
                ,[SubFactor]
                ,[SubWgt]
                ,[CatCatchWgt]
                ,[LngtCode]
                ,[LngtClass]
                ,[HLNoAtLngt]
                ,[Cal_DateID]
                ,[tblcodeidAphiaFn]
                ,[tblcodeidAphia]
                ,[DevStage]
                ,[Valid_Aphia]
                ,[ScientificName_WoRMS]
                ,[LenMeasType]
                FROM [DW_DATRAS].[dbo].[A_DatrasExchange_HL]
                WHERE tblHaulID BETWEEN '%s' AND '%s'", from, to)

DW_HL <- sqlQuery(conn_dwdatras, sqlq) 
DW_HL <- DW_HL %>% filter(tblHaulID %in% haulID)
names(DW_HL)
# names(HL)
DW_HL <- DW_HL[, -grep("tbl", colnames(DW_HL))]
DW_HL <- DW_HL[, -grep("Survey", colnames(DW_HL))]
DW_HL <- DW_HL[, -grep("ShipID", colnames(DW_HL))]
DW_HL <- DW_HL[, -grep("Cal_DateID", colnames(DW_HL))]
DW_HL <- DW_HL[, -grep("Valid_Aphia", colnames(DW_HL))]
DW_HL <- DW_HL[, -grep("ScientificName", colnames(DW_HL))]
names(DW_HL)
# DW_HL <- DW_HL[,-(25:26)]
DW_HL$SpecCode <- as.numeric(DW_HL$SpecCode)

data.frame(table(unlist(HL$Year)))
data.frame(table(unlist(DW_HL$Year)))

data.frame(table(unlist(HL$Country)))
data.frame(table(unlist(DW_HL$Country)))



######################
# Now, we have 6 files HH, HL, CA and DW_HH, DW_CA and DW_HL
# We can compare them
# Let´s quickly check column names

names(HH)
names(DW_HH)
names(CA)
names(DW_CA)
names(HL)
names(DW_HL)

# Lets check number of records by eye for each Record Type

# DW_HL has two more rows, with StNo that do not exist in the upload!! 196 and 214!!
# because those hauls exist in HH
# Will try to describe this:

HL$ID <- paste0(HL$Ship, HL$HaulNo)
DW_HL$ID2 <- paste0(DW_HL$Ship, DW_HL$HaulNo)
HL$ID <- gsub(" ", "", HL$ID)
DW_HL$ID2 <- gsub(" ", "", DW_HL$ID2)

n <- unique(HL[c("Ship", "HaulNo")])
n2 <- unique(DW_HL[c("Ship", "HaulNo")])

# That is why, because in HL there are less hauls than in HH,
# no need for more

# check <- HL$ID
# check2 <- DW_HL$ID2
# check <- gsub(" ", "", check)
# check2 <- gsub(" ", "", check2)

# bla <- setdiff(check2,check)

# Find those hauls in HL

# subset <- DW_HL %>% filter(ID2 %in%bla)


data.frame(table(unlist(HH$Country)))
data.frame(table(unlist(DW_HH$Country)))

df1 <- data.frame(table(unlist(HL$SpecCode)))
df2 <- data.frame(table(unlist(DW_HL$SpecCode)))
colnames(df2) <- c("Var1","Freq2")
df3 <- left_join(df1,df2)

df1 <- data.frame(table(unlist(CA$SpecCode)))
df2 <- data.frame(table(unlist(DW_CA$SpecCode)))
colnames(df2) <- c("Var1","Freq2")
df3 <- left_join(df1,df2)

# some differences between CAs, upload has some more species
# NEED TO CHECK WHY!!

bla <- CA %>% group_by(SpecCode) %>% summarise(tot = sum(CANoAtLngt))
blu <- DW_CA %>% group_by(SpecCode) %>% summarise(tot2 = sum(CANoAtLngt))
bli <- left_join(bla, blu)

bla <- HL %>% group_by(SpecCode) %>% summarise(tot = sum(HLNoAtLngt))
blu <- DW_HL %>% group_by(SpecCode) %>% summarise(tot2 = sum(HLNoAtLngt))
bli <- left_join(bla, blu)


# sum of CANoAtLngt per species and HLNoAtLngt should be the same at least

library(purrr)

## here I check empty columns in the Warehouse, and compare them with the empty 
#columns in the upload

DW_empty <- DW_HH %>% keep(~all(is.na(.x))) %>% names
DW_empty
empty <- HH %>% keep(~all(is.na(.x))) %>% names
empty
# In DW_HH there are 19 empty columns, whereas only 8 in the upload!! 

check <- setdiff(DW_empty, empty)

## I check if those columns correspond to all -9 values (if not it would be more urgent!)

check <- HH[,names(HH) %in% check]


DW_empty <- DW_CA %>% keep(~all(is.na(.x))) %>% names
DW_empty
empty <- CA %>% keep(~all(is.na(.x))) %>% names
empty

DW_empty <- DW_HL %>% keep(~all(is.na(.x))) %>% names
DW_empty
empty <- HL %>% keep(~all(is.na(.x))) %>% names
empty

#############################
#Now I should check data products
#############################

# 1.CPUE per length
# per haul              
# tblCHL_IBTS, tblCHL_Non_IBTS
# I will compare number of species per haul with HL 

# 2.CPUE per age     
# per haul
# tblCHA_IBTS, tblCHA_Non_IBTS
# I will compare number of hauls with CPUE per length
# 
# These two would correspond to WGWIDE recommendation

# 3.Indices
# tblDWIndices
# I will compare that all species are there


Survey = "NS-IBTS"

#################
# 1: cpue per length vs HL
#################

sqlq <- sprintf("SELECT  [Survey]
                ,[SurveyID]
                ,[Year]
                ,[Quarter]
                ,[Ship]
                ,[ShipID]
                ,[Gear]
                ,[GearID]
                ,[HaulNo]
                ,[ShootLat]
                ,[ShootLon]
                ,[DateTime]
                ,[Depth]
                ,[Area]
                ,[AreaID]
                ,[Subarea]
                ,[DayNight]
                ,[SpecCode]
                ,[Species]
                ,[Sex]
                ,[LngtClas]
                ,[CPUE]
                ,[tblcodeidAphiaFn]
                ,[tblcodeidAphia]
                ,[Cal_DateID]
                ,[ICESArea]
                ,[Hauldur]
                FROM [DW_DATRAS].[dbo].[A_Datras_tblCHL_IBTS]
                WHERE SurveyID = '%s' AND  Year = '%s' AND Quarter = '%s'
                ORDER BY Cal_dateID desc" , survey, year, quarter)

CHL <- sqlQuery(conn_dwdatras, sqlq)

names(CHL)
# transform zeros to NA


# Remove rows with all NAs 
# delete.na <- function(DF, n=0) {
#         DF[rowSums(is.na(DF)) <= n,]
# }

CHL <- CHL %>% rename(tblCodeID = SpecCode)

CHL <- left_join(CHL, specCode)

# Number of species in each ship and haul combination

CHL <- CHL %>% filter(LngtClas > 0)

# CPUE == 0

dat <- CHL %>% group_by(Ship,HaulNo) %>% summarise(sps = length(unique(Code)))

# bru <- CHL %>% filter(Ship == "DANG")
# bru <- bru %>% filter(HaulNo == 6)
# unique(bru$Code)
# bra <- dplyr::filter(DW_HL,Ship == 'TRI2')


# DW_HL <- DW_HL %>% filter(SpecCode %in% StandarsSpecies)
# DW_CA2 <- DW_CA2 %>% filter(AgeRings > -9, Ship == "58UO", HaulNo == 582)
DW_HL <- DW_HL %>% filter(LngtClass > 0)
dat2 <- DW_HL %>% group_by(Ship,HaulNo) %>% summarise(sps = length(unique(SpecCode)))
dat2$ID <- "DW"
dat$ID <- "Product"

dat <- tidyr::spread(dat, ID, sps)
dat2 <- tidyr::spread(dat2, ID, sps)

dat$ID <- paste0(dat$Ship, dat$HaulNo)
dat2$ID <- paste0(dat2$Ship, dat2$HaulNo)
dat2$ID <- gsub(" ", "", dat2$ID)
dat$ID <- gsub(" ", "", dat$ID)

dat <- dat[,-(1:2)]
dat2 <- dat2[,-(1:2)]

# dat <- tidyr::spread(dat, ID, sps)
# bli <- dat$ID
# blo <- dat2$ID

# # dat2 <- tidyr::spread(dat2, ID, sps)
# dat2 <- droplevels(dat2)
# dat2$Ship <- factor(dat2$Ship)
# levels(droplevels(dat2$Ship))

bla <- left_join(dat,dat2)
bla <- bla %>% filter(DW != Product)

# only 4 hauls with one species differemce. Quite good but still need to understand why


# Ship names have spaces that mess up the filtering!
DW_HL$Ship <- gsub(" ", "", DW_HL$Ship)

check <- DW_HL %>% filter(Ship == "TRI2")
check <- check %>% filter (HaulNo == 48)
# check <- DW_HL %>% filter(ID2 %in% bla$ID)
# check <- droplevels(check$SpecCode)

# 

check2 <- CHL %>% filter(Ship == "TRI2")
check2 <- check2 %>% filter(HaulNo == 48)
a <-unique(check$SpecCode)
b <-unique(check2$Code)
setdiff(a,b)

# 138483

check <- DW_HL %>% filter(Ship == "SCO3"| HaulNo == 239)
check2 <- CHL %>% filter(Ship == "SCO3")
check2 <- check2 %>% filter(HaulNo == 239)
a <-unique(check$SpecCode)
b <-unique(check2$Code)
setdiff(a,b)
#711846

# Need to check with Vaishav this tiny mismatches.

# In the DW there are more species than in the product

#############
# 2: cpue per length vs cpue per age, will compare number of hauls in each
######################

# I can´t compare number of species per haul because in the CPUE per age there is
# substitution, so all standard species are there.

StandardSpecies <- c(126436,126437,126417,127023,126444,127143,126441,126425,
                     127136,126438)

sqlq <- sprintf("SELECT  [Survey]
                ,[SurveyID]
                ,[Year]
                ,[Quarter]
                ,[Ship]
                ,[ShipID]
                ,[Gear]
                ,[GearID]
                ,[HaulNo]
                ,[ShootLat]
                ,[ShootLon]
                ,[DateTime]
                ,[Depth]
                ,[Area]
                ,[SubArea]
                ,[DayNight]
                ,[SpecCode]
                ,[Species]
                ,[Sex]
                ,[Age_0]
                ,[Age_1]
                ,[Age_2]
                ,[Age_3]
                ,[Age_4]
                ,[Age_5]
                ,[Age_6]
                ,[Age_7]
                ,[Age_8]
                ,[Age_9]
                ,[Age_10]
                ,[Age_11]
                ,[Age_12]
                ,[Age_13]
                ,[Age_14]
                ,[Age_15]
                ,[tblcodeidAphiaFn]
                ,[tblcodeidAphia]
                ,[Cal_DateID]
                ,[ICESArea]
                FROM [DW_DATRAS].[dbo].[A_Datras_tblCHA_IBTS]
                WHERE SurveyID = '%s' AND  Year = '%s' AND Quarter = '%s'
                ORDER BY Cal_dateID desc", survey, year, quarter)

CHA <- sqlQuery(conn_dwdatras, sqlq)

CHA <- CHA %>% rename(tblCodeID = SpecCode)

CHA <- left_join(CHA, specCode)

dat2 <- CHA %>% group_by(Ship) %>% summarise(hauls2 = length(unique(HaulNo)))
# dat2$ID <- paste0(dat2$Ship, dat2$HaulNo)
# dat2$ID <- gsub(" ", "", dat2$ID)
# dat2 <- dat2[,-(1:2)]

# CHL2 <- CHL %>% filter(Code %in% StandardSpecies)

dat <- CHL %>% group_by(Ship) %>% summarise(hauls = length(unique(HaulNo)))
# dat$ID <- paste0(dat$Ship, dat$HaulNo)
# dat$ID <- gsub(" ", "", dat$ID)
# dat <- dat[,-(1:2)]
# colnames(dat) <- c("sps2", "ID")

bla <- left_join(dat,dat2)

# same number of hauls by ship in both products

#################
# 3: Indices
#################


sqlq <- sprintf("SELECT  [Survey]
                ,[Year]
                ,[Quarter]
                ,[SpecCode]
                ,[Genus]
                ,[Family]
                ,[IndexArea]
                ,[Sex]
                ,[PlusGr]
                ,[Age_0]
                ,[Age_1]
                ,[Age_2]
                ,[Age_3]
                ,[Age_4]
                ,[Age_5]
                ,[Age_6]
                ,[Age_7]
                ,[Age_8]
                ,[Age_9]
                ,[Age_10]
                ,[Age_11]
                ,[Age_12]
                ,[Age_13]
                ,[Age_14]
                ,[Age_15]
                ,[Ship]
                ,[tblcodeidAphiaFn]
                ,[tblcodeidAphia]
                ,[Cal_DateID]
                FROM [DW_DATRAS].[dbo].[A_Datras_tblDWindices]
                WHERE Survey = '%s' AND  Year = '%s' AND Quarter = '%s'
                ORDER BY Cal_dateID desc", Survey, year, quarter)

indices <- sqlQuery(conn_dwdatras, sqlq)

unique(indices$SpecCode)

# 9 species, I think that is enough




### While running the shiny app, we detected a few coordinates outliers
# will try to extract them to check with submitters

dbConnect_dwdatras <- 'Driver={SQL Server};Server=SQL06;Database=DW_DATRAS;Trusted_Connection=yes'
conn_dwdatras <- odbcDriverConnect(connection = dbConnect_dwdatras)

# haulID <- unique(HH_2$tblHaulID)
# from <- min(haulID)
# from <- as.character(from)
# to <- max(haulID)
# to <- as.character(to)

sqlq <- sprintf("SELECT [tbltrawlcruiseID]
                ,[tblHaulID]
                ,[Survey]
                ,[RecordType]
                ,[Quarter]
                ,[Country]
                ,[Ship]
                ,[ShipID]
                ,[Gear]
                ,[SweepLngt]
                ,[GearExp]
                ,[DoorType]
                ,[StNo]
                ,[HaulNo]
                ,[Year]
                ,[month]
                ,[Day]
                ,[TimeShot]
                ,[Stratum]
                ,[HaulDur]
                ,[DayNight]
                ,[ShootLat]
                ,[ShootLong]
                ,[HaulLat]
                ,[HaulLong]
                ,[StatRec]
                ,[Depth]
                ,[HaulVal]
                ,[HydroStNo]
                ,[StdSpecRecCode]
                ,[BycSpecRecCode]
                ,[DataType]
                ,[Netopening]
                ,[Rigging]
                ,[Tickler]
                ,[Distance]
                ,[Warplngt]
                ,[Warpdia]
                ,[WarpDen]
                ,[DoorSurface]
                ,[DoorWgt]
                ,[DoorSpread]
                ,[WingSpread]
                ,[Buoyancy]
                ,[KiteDim]
                ,[WgtGroundRope]
                ,[TowDir]
                ,[GroundSpeed]
                ,[SpeedWater]
                ,[SurCurDir]
                ,[SurCurSpeed]
                ,[BotCurDir]
                ,[BotCurSpeed]
                ,[WindDir]
                ,[WindSpeed]
                ,[SwellDir]
                ,[SwellHeight]
                ,[SurTemp]
                ,[BotTemp]
                ,[SurSal]
                ,[BotSal]
                ,[ThermoCline]
                ,[ThClineDepth]
                ,[Cal_DateID]
                ,[CodendMesh]
                ,[SecchiDepth]
                ,[Turbidity]
                ,[TidePhase]
                ,[TideSpeed]
                ,[PelSampType]
                ,[MinTrawlDepth]
                ,[MaxTrawlDepth]
                ,[ICESArea]
                FROM [DW_DATRAS].[dbo].[A_DatrasExchange_HH]")

DW_HH <- sqlQuery(conn_dwdatras, sqlq)
names(DW_HH)
# names(HH)

DW_HH <- select(DW_HH, -contains("tbl"))
DW_HH <- select(DW_HH, -contains("Survey"))
DW_HH <- select(DW_HH, -contains("ShipID"))
DW_HH <- select(DW_HH, -contains("Cal_DateID"))
DW_HH <- select(DW_HH, -contains("ICESArea"))
colnames(DW_HH)[12] <- "Month"

min(DW_HH$HaulLat, na.rm = TRUE)
min(DW_HH$HaulLong, na.rm = TRUE)

min(DW_HH$ShootLat, na.rm = TRUE)
min(DW_HH$ShootLong, na.rm = TRUE)

max(DW_HH$HaulLat, na.rm = TRUE)
max(DW_HH$HaulLong, na.rm = TRUE)

max(DW_HH$ShootLat, na.rm = TRUE)
max(DW_HH$ShootLong, na.rm = TRUE)

