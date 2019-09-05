#### Functions for ALK substitution ###
#### Signe/2017/12/14, first part of connection to database ###
## Adriana, September 2019
## Colin and Vaishav, last part putting back table into DB


#The aim of this script is:
# 1st, to compare original ALK as submitted to Datras with the resulting ALK following
#  i) the manual procedure and 
# ii) the automated procedure
#
# 2nd, to upload the supplemented ALK with the automated procedure to the database
# 3rd, to compare the resulting indexes to those calculated with the ALK with manual substitution procedure.



library(RODBC)
library(dplyr)
library(data.table)
library(ggplot2)

#### Input information
survey <- 'NS-IBTS'
year <- 2018
quarter <- 1


#### Connect to database
# settings
dbConnect_datras <- 'Driver={SQL Server};Server=SQL06;Database=DATRAS;Trusted_Connection=yes'
dbConnect_dwdatras <- 'Driver={SQL Server};Server=SQL06;Database=DW_DATRAS;Trusted_Connection=yes'
# connect
conn_datras <- odbcDriverConnect(connection = dbConnect_datras)
conn_dwdatras <- odbcDriverConnect(connection = dbConnect_dwdatras)

#### Set up SQL command to get species ID

sqlq <- sprintf("SELECT [ScientificName_WoRMS], [tblcodeid_Aphia]
                FROM [DATRAS].[dbo].[A_Datras_WORMS]")

sps <- sqlQuery(conn_datras, sqlq)
sps <- unique(sps)
colnames(sps) <- c("Species", "tblcodeidAphia")

#### Set up SQL command to get ALK for given survey/year/quarter/species

sqlq <- sprintf("SELECT Survey, Year, Quarter, Area, tblcodeidAphia, LngtClasMM, Age_0, Age_1, Age_2, Age_3, Age_4, Age_5, Age_6, Age_7, Age_8, Age_9, Age_10
                FROM [DW_DATRAS].[dbo].[A_Datras_tblWebALK]
                WHERE Survey = '%s' AND  Year = '%s' AND Quarter = '%s' AND Area > 0
                GROUP BY Survey, Year, Quarter, Area, tblcodeidAphia, LngtClasMM, Age_0, Age_1, Age_2, Age_3, Age_4, Age_5, Age_6, Age_7, Age_8, Age_9, Age_10"
                , survey, year, quarter)
df <- sqlQuery(conn_dwdatras, sqlq)

list <- unique(df$tblcodeidAphia)
sps <- sps %>% filter(tblcodeidAphia %in% list)

df <- merge(df,sps)

names(df)
colnames(df)[colnames(df)=="tblcodeidAphia"] <- "AphiaID"
colnames(df)[colnames(df)=="LngtClasMM"] <- "LngtClass"

unique(df$Species)

df$Species <- droplevels(df$Species)
unique(df$Species)

# Load Borrowing areas scheme

BorrowArea <- read.csv("BorrowArea.csv")

# Separate by species

sp.l <- split(df,df$Species)

res <- data.frame()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Create ALK with the original data as submitted to datras
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

#Loop by species

 for(n in names(sp.l)){   
        dat <- sp.l[[n]]   #Extract the data from the list
        dat$Area <- as.factor(dat$Area)
        
        dat[is.na(dat)] <- 0
        
        dat <- dat %>% select (Species, Area, LngtClass, Age_0, Age_1, Age_2, Age_3, Age_4, Age_5, Age_6, Age_7, Age_8, Age_9, Age_10)
        
        # Little trick to include areas which are not in original data
        null <- subset(BorrowArea, !(Area %in% dat$Area))
        null <- null %>% select(Area)
        
        sub <- dat %>% select(c(Species, LngtClass))
        sub <- unique(sub)
        
        null <- merge(null,sub, all = TRUE)
        if(length(null$LngtClass) > 0){
        null <- null[c("Species", "Area", "LngtClass")]
        null$Area <- as.factor(null$Area)
        
        dat <- merge(dat, null, all = TRUE)}
        dat[is.na(dat)] <- 0
        
        dat_long <- tidyr::gather(dat, Age, Value, -Area, -LngtClass, -Species)
        
        dat_long$Area <- as.factor(dat_long$Area)
        
        dat_long[is.na(dat_long)] <- 0
        sum <- dat_long %>% group_by(Area, Age)%>% summarise(sum(Value))
        colnames(sum) <- c("Area","Age","Value")
        sum <- sum %>% mutate(label= ifelse(Value < 25, "red", "blue"))
        sum <- sum%>% select("Area","Age", "label")
        dat_long <- dat_long%>% left_join(sum)
        
        dat_long[dat_long == 0] <- NA
        dat_long$Area <- factor(dat_long$Area, levels = c(1,2,3,4,5,6,7,8,9,10))
        res <- rbind(res,dat_long)
        res
 }
res <- unique(res)
dat_long <- res  
dat_long$Age <- factor(dat_long$Age, levels = c("Age_0", "Age_1", "Age_2", "Age_3", "Age_4", "Age_5", "Age_6", "Age_7", "Age_8", "Age_9", "Age_10"))
        
ggplot(dat_long, aes(x=Age, y=LngtClass, size=Value)) +
        geom_point(alpha=0.2)+ facet_grid(Species~Area, scales = "free")+
        theme(text = element_text(size=6), axis.text.x = element_text(angle = 45,hjust = 1))
        
ggsave("NSIBTS2017q1_ALKnoSubstitution.tiff", units= "mm", width = 350, height = 175)

ggplot(dat_long, aes(x=Age, y=LngtClass, size=Value, colour = label)) +
        geom_point(alpha=0.2)+ facet_grid(Species~Area, scales = "free")+
        theme(text = element_text(size=6), axis.text.x = element_text(angle = 45,hjust = 1))+
        scale_color_manual(values = c("blue", "red"))

ggsave("NSIBTS2017q1_ALKnoSubstitutionCOLOR.tiff", units= "mm", width = 350, height = 175)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
# Now we will do the same plots of ALK with the manual substitution
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# There is a table in the database that stores the substitutions performed
        
sqlq <- sprintf("select A.survey,a.quarter,A.year,a.ScientificName_WoRMS,A.Area, A.BorrowedArea from ( 
SELECT     tblCode_1.Code AS Survey, tblIndexSettings.Quarter ,tblSubKeyInfo.year,tblCode_2.Code as Area, 
                tblCode.Code AS BorrowedArea,  A_Datras_WORMS.WoRMS_AphiaID_Valid, A_Datras_WORMS.ScientificName_WoRMS
                FROM       A_Datras_WORMS INNER JOIN
                tblIndexSettings ON A_Datras_WORMS.tblcodeid_Aphia = tblIndexSettings.SpecCodeAphia INNER JOIN
                tblSubKeyInfo ON tblIndexSettings.tblIndexSettingsID = tblSubKeyInfo.tblIndexSettingsID INNER JOIN
                tblSubArea ON tblSubKeyInfo.tblSubKeyInfoID = tblSubArea.tblSubKeyInfoID INNER JOIN
                tblCode AS tblCode_2 ON tblSubKeyInfo.Area = tblCode_2.tblCodeID INNER JOIN
                tblCode ON tblSubArea.SubArea = tblCode.tblCodeID INNER JOIN
                tblCode AS tblCode_1 ON tblIndexSettings.Survey = tblCode_1.tblCodeID
                group by tblCode_1.Code, tblIndexSettings.Quarter ,tblSubKeyInfo.year,tblCode_2.Code, 
                tblCode.Code,  A_Datras_WORMS.WoRMS_AphiaID_Valid, A_Datras_WORMS.ScientificName_WoRMS
) A 
                where A.survey = 'NS-IBTS' and a.year = 2017 and a.Quarter = 1 
                group by  a.survey, a.Quarter,a.year, a.ScientificName_WoRMS,A.Area , A.BorrowedArea
                order by a.ScientificName_WoRMS,A.Area")

#Download the substitution scheme

subst <- sqlQuery(conn_datras, sqlq)

res2 <- data.frame()
res3 <- data.frame()

# Have a look to the substitutions done, if some species have not been supplemented, 
# they have to be out of the next loop
# This could be automated, but for demonstration purposes is enough as is now 

# Create a vector with the species numbers that were supplemented

species <- c(1,2,4,6,7,8,9,10)

#loop accross each species
for(n in species){   
        dat <- sp.l[[n]]   #Extract the data from the list
        dat[is.na(dat)] <- 0
        subst_sp <- subst %>% filter(ScientificName_WoRMS %in% dat$Species)
        dat$Area <- as.factor(dat$Area)
        list <- subst_sp %>% select(Area,6)
        # loop accross all areas 
        for(i in 1:10){
                list_i <- list%>%filter(Area ==i)
                bla <- unique(list_i$Area)
                blu <- unique(list_i$BorrowedArea)
                list_i <- append(bla,blu)
                if(length(list_i) == 0){ai <- dat %>% filter(Area %in% i)%>% select(-Area, -AphiaID, -Survey, -Year, -Quarter)}
                if(length(list_i) != 0){ai <- dat %>% filter(Area %in% list_i)%>% select(-Area, -AphiaID, -Survey, -Year, -Quarter) %>% group_by(LngtClass, Species) %>% summarise_each(list(~sum))}
                ai$Area <- i
                res3 <- dplyr::bind_rows(res3,ai)
        }
        res2 <- dplyr::bind_rows(res2,res3)
        res2
}


#For those species which were not supplemented at all:
dat <- sp.l[[3]]   #Extract the data from the list
dat[is.na(dat)] <- 0
names(dat)
names(res2)
dat <- dat[,5:18]

res2 <- dplyr::bind_rows(res2,dat)

dat <- sp.l[[5]]   #Extract the data from the list
dat[is.na(dat)] <- 0
names(dat)
names(res2)
dat <- dat[,5:18]

res2 <- dplyr::bind_rows(res2,dat)

# Check

unique(res2$Species)
unique(res2$Area)

proc1_dat <- unique(res2)

proc1_dat_long <- tidyr::gather(proc1_dat, Age, Value, -Area, -Species, -LngtClass)


proc1_dat_long$Area <- factor(proc1_dat_long$Area, levels = c(1,2,3,4,5,6,7,8,9,10))

# here I create a flag to colour data that were supplemented and not, according
# to the original numbers in dat_long

dat_long[is.na(dat_long)] <- 0
sum <- dat_long %>% group_by(Species, Area, Age)%>% summarise(sum(Value))
colnames(sum) <- c("Species", "Area","Age","Value_initial")

proc1_dat_long[is.na(proc1_dat_long)] <- 0
sum2 <- proc1_dat_long %>% group_by(Species, Area, Age)%>% summarise(sum(Value))
colnames(sum2) <- c("Species", "Area","Age","Value_after")

proc1_dat_long <- proc1_dat_long %>% left_join(sum)
proc1_dat_long <- proc1_dat_long %>% left_join(sum2)

proc1_dat_long$Age <- factor(proc1_dat_long$Age, levels = c("Age_0", "Age_1", "Age_2", "Age_3", "Age_4", "Age_5", "Age_6", "Age_7", "Age_8", "Age_9", "Age_10"))

proc1_dat_long <- proc1_dat_long%>%mutate(label=ifelse(Value_after>Value_initial, "subst", "original"))
proc1_dat_long[proc1_dat_long == 0] <- NA

ggplot(proc1_dat_long, aes(x=Age, y=LngtClass, size=Value)) +
        geom_point(alpha=0.2)+ facet_grid(Species~Area, scales = "free")+
        theme(text = element_text(size=6), axis.text.x = element_text(angle = 45,hjust = 1))

ggsave("NSIBTS2017q1_ALKManualSubstitution.tiff", units= "mm", width = 350, height = 175)

ggplot(res2_long, aes(x=Age, y=LngtClass, size=Value, colour = label)) +
        geom_point(alpha=0.2)+ facet_grid(Species~Area, scales = "free")+
        theme(text = element_text(size=6), axis.text.x = element_text(angle = 45,hjust = 1))+
        scale_color_manual(values = c("black", "red"))

ggsave("NSIBTS2017q1_ALKManualSubstitutionCOLOR.tiff", units= "mm", width = 350, height = 175)



      
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Now the automated substitution:
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Starting from the original data

dat_long <- subset(dat_long, select= -label)

dat_long$Area <- as.factor(dat_long$Area)

dat_long[is.na(dat_long)] <- 0

dat_long$Species <- as.character(dat_long$Species)

blu <-dat_long

species <- unique(dat_long$Species)

res2 <- data.frame()
res3 <- data.frame()

# Loop accross all species

for(i in 1:10){
        value <- species[[i]]
        
        dat_long <- blu %>% filter(Species == value)

list <- BorrowArea %>% select(c(Area,2))

#First I go through the i substitution in each RFarea, from a to j
a <- dat_long %>% filter(Area %in% c(list[1,]))
supl <- a %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
a <- a %>% filter(Area %in% c(list[1,1]))
a <- a %>% left_join(supl)

b <- dat_long %>% filter(Area %in% c(list[2,]))
supl <- b %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
b <- b %>% filter(Area %in% c(list[2,1]))
b <- b %>% left_join(supl)

c <- dat_long %>% filter(Area %in% c(list[3,])) 
supl <- c %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
c <- c %>% filter(Area %in% c(list[3,1]))
c <- c %>% left_join(supl)

d <- dat_long %>% filter(Area %in% c(list[4,]))
supl <- d %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
d <- d %>% filter(Area %in% c(list[4,1]))
d <- d %>% left_join(supl)

e <- dat_long %>% filter(Area %in% c(list[5,]))
supl <- e %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
e <- e %>% filter(Area %in% c(list[5,1]))
e <- e %>% left_join(supl)

f <- dat_long %>% filter(Area %in% c(list[6,]))
supl <- f %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
f <- f %>% filter(Area %in% c(list[6,1]))
f <- f %>% left_join(supl)

g <- dat_long %>% filter(Area %in% c(list[7,]))
supl <- g %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
g <- g %>% filter(Area %in% c(list[7,1]))
g <- g %>% left_join(supl)

h <- dat_long %>% filter(Area %in% c(list[8,]))
supl <- h %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
h <- h %>% filter(Area %in% c(list[8,1]))
h <- h %>% left_join(supl)

i <- dat_long %>% filter(Area %in% c(list[9,]))
supl <- i %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
i <- i %>% filter(Area %in% c(list[9,1]))
i <- i %>% left_join(supl)

j <- dat_long %>% filter(Area %in% c(list[10,]))
supl <- j %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
j <- j %>% filter(Area %in% c(list[10,1]))
j <- j %>% left_join(supl)

# then I merge all this i substitutions  
bla <- rbind(a,b,c,d,e,f,g,h,i,j)
names(bla) <- c("Species", "Area","LngtClass","Age","Value2","supl")
# now I calculate the number per age and area of the initial dataset
n_age_area <- bla %>% group_by(Area, Age) %>% summarise(n_age_area = sum(Value2))
dat_long <- dat_long %>% left_join(n_age_area)
dat_long <- dat_long %>% left_join(bla)

# if that total number per age and area is less than 25, then I take the substituted value per age, area and length
dat_long$Value2 <- ifelse(dat_long$n_age_area > 24, dat_long$Value, dat_long$supl)
dat_long <- dat_long %>% select(c(Species, Area, LngtClass, Age, Value, Value2))

# I can´t put the first loop n = 2 together with the rest :-(
 for(n in 3:6){
        list <- BorrowArea %>% select(c(Area,n))
        
        #First I go through the i substitution in each RFarea, from a to j
        a <- dat_long %>% filter(Area %in% c(list[1,]))
        a$Value <- ifelse(a$Area == "1", a$Value2, a$Value)
        a <- subset(a,select = -Value2)
        supl <- a %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        a <- a %>% filter(Area %in% c(list[1,1]))
        a <- a %>% left_join(supl)
        
        b <- dat_long %>% filter(Area %in% c(list[2,]))
        b$Value <- ifelse(b$Area == "2", b$Value2, b$Value)
        b <- subset(b,select = -Value2)
        supl <- b %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        b <- b %>% filter(Area %in% c(list[2,1]))
        b <- b %>% left_join(supl)
        
        c <- dat_long %>% filter(Area %in% c(list[3,])) 
        c$Value <- ifelse(c$Area == "3", c$Value2, c$Value)
        c <- subset(c,select = -Value2)
        supl <- c %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        c <- c %>% filter(Area %in% c(list[3,1]))
        c <- c %>% left_join(supl)
        
        d <- dat_long %>% filter(Area %in% c(list[4,]))
        d$Value <- ifelse(d$Area == "4", d$Value2, d$Value)
        d <- subset(d,select = -Value2)
        supl <- d %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        d <- d %>% filter(Area %in% c(list[4,1]))
        d <- d %>% left_join(supl)
        
        e <- dat_long %>% filter(Area %in% c(list[5,]))
        e$Value <- ifelse(e$Area == "5", e$Value2, e$Value)
        e <- subset(e,select = -Value2)
        supl <- e %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        e <- e %>% filter(Area %in% c(list[5,1]))
        e <- e %>% left_join(supl)
        
        f <- dat_long %>% filter(Area %in% c(list[6,]))
        f$Value <- ifelse(f$Area == "6", f$Value2, f$Value)
        f <- subset(f,select = -Value2)
        supl <- f %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        f <- f %>% filter(Area %in% c(list[6,1]))
        f <- f %>% left_join(supl)
        
        g <- dat_long %>% filter(Area %in% c(list[7,]))
        g$Value <- ifelse(g$Area == "7", g$Value2, g$Value)
        g <- subset(g,select = -Value2)
        supl <- g %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        g <- g %>% filter(Area %in% c(list[7,1]))
        g <- g %>% left_join(supl)
        
        h <- dat_long %>% filter(Area %in% c(list[8,]))
        h$Value <- ifelse(h$Area == "8", h$Value2, h$Value)
        h <- subset(h,select = -Value2)
        supl <- h %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        h <- h %>% filter(Area %in% c(list[8,1]))
        h <- h %>% left_join(supl)
        
        i <- dat_long %>% filter(Area %in% c(list[9,]))
        i$Value <- ifelse(i$Area == "9", i$Value2, i$Value)
        i <- subset(i,select = -Value2)
        supl <- i %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        i <- i %>% filter(Area %in% c(list[9,1]))
        i <- i %>% left_join(supl)

        j <- dat_long %>% filter(Area %in% c(list[10,]))
        j$Value <- ifelse(j$Area == "10", j$Value2, j$Value)
        j <- subset(j,select = -Value2)
        supl <- j %>% group_by(LngtClass, Age) %>% summarise(sum(Value))
        j <- j %>% filter(Area %in% c(list[10,1]))
        j <- j %>% left_join(supl)
        
        # then I merge all this i substitutions  
        bla <- rbind(a,b,c,d,e,f,g,h,i,j)
        names(bla) <- c("Species", "Area","LngtClass","Age","Value2","supl")
        # now I calculate the number per age and area of the initial dataset
        n_age_area <- bla %>% group_by(Area, Age) %>% summarise(n_age_area = sum(Value2))
        dat_long <- dat_long %>% left_join(n_age_area)
        dat_long <- dat_long %>% left_join(bla)
        
        # if that total number per age and area is less than 25, then I take the substituted value per age, area and length
        dat_long$Value2 <- ifelse(dat_long$n_age_area > 24, dat_long$Value2, dat_long$supl)
        dat_long <- dat_long %>% select(c(Species, Area, LngtClass, Age, Value, Value2))
        dat_long2 <- dat_long %>% select(c(Species, Area, LngtClass, Age, Value2))
        dat_long2
        res2 <- rbind(res2, dat_long2)
         }
res3 <- rbind(res3, res2)
}

res3 <- unique(res3)
proc2_dat_long <- res3

proc2_dat_long[is.na(proc2_dat_long)] <- 0
sum3 <- proc2_dat_long %>% group_by(Species, Area, Age)%>% summarise(sum(Value2))
colnames(sum3) <- c("Species", "Area","Age","Value_afterbis")

proc2_dat_long <- proc2_dat_long %>% left_join(sum)
proc2_dat_long <- proc2_dat_long %>% left_join(sum3)

proc2_dat_long$Age <- factor(proc2_dat_long$Age, levels = c("Age_0", "Age_1", "Age_2", "Age_3", "Age_4", "Age_5", "Age_6", "Age_7", "Age_8", "Age_9", "Age_10"))

proc2_dat_long <- proc2_dat_long%>%mutate(label=ifelse(Value_initial<Value_afterbis, "red", "blue"))

proc2_dat_long$Area <- factor(proc2_dat_long$Area, levels = c(1,2,3,4,5,6,7,8,9,10))

proc2_dat_long[proc2_dat_long == 0] <- NA

ggplot(proc2_dat_long, aes(x=Age, y=LngtClass, size=Value2, colour = label)) +
        geom_point(alpha=0.2)+ facet_grid(Species~Area, scales = "free")+
        theme(text = element_text(size=6), axis.text.x = element_text(angle = 45,hjust = 1))+
        scale_color_manual(values = c("black", "red"))


ggsave("NSIBTS2017q1_ALKAutSubstCOLOR.tiff", units= "mm", width = 350, height = 175)    

        
############
#~~~~~~~~~~~
# Prepare this new ALK and upload it to the database
#~~~~~~~~~~~

names(res3)
unique(res3$Age)
res3 <- res3 %>% filter(Age != "x[FALSE, ]")
res3$Survey <- "NS-IBTS"
res3$Year <- 2017
res3$Quarter <- 1       
res3$Lngtclas <- res3$LngtClass

bla <- df %>% select(c(AphiaID, Species))
bla <- unique(bla)

col.order <- names(df)

res3 <- merge(bla,res3, all = TRUE)

res3$Speccode <-res3$AphiaID
res3$NoAtALK <- res3$Value2

res3 <- subset(res3, select = c("Survey", "Year", "Quarter", "Area", "Speccode", "Lngtclas", "Age", "NoAtALK"))

#Check the plus group, needed for indeces calculation
# However it is set per species elsewhere, so this step could easily be removed

ca <- icesDatras::getCAdata("NS-IBTS", 2017, 1)

plus_gr <- ca %>% select(Valid_Aphia, PlusGr)

plus_gr <- unique(plus_gr)

# If all are NAs
res3$PlusGr <- "-9"

res3$LngtCode <- "."

#Fix names
res3$Age <- gsub("Age_", "", res3$Age)

res3$Year <- as.integer(res3$Year)
res3$Quarter <- as.integer(res3$Quarter)
res3$Area <- as.integer(res3$Area) 
res3$Speccode <- as.integer(res3$Speccode)
res3$Lngtclas <- as.integer(res3$Lngtclas)
res3$Age <- as.integer(res3$Age)
res3$NoAtALK <- as.integer(res3$NoAtALK)
res3$PlusGr <- as.integer(res3$PlusGr) 
unique(res3$Age)

#I connect to the test server for thew time being

dbConnect_datras <- 'Driver={SQL Server Native Client 11.0};Server=SQL2016Dev;Database=DATRAS;Trusted_Connection=yes'

conn_datras <- odbcDriverConnect(connection = dbConnect_datras)

sqlSave(conn_datras, res3, tablename = "A_DATRAS_tblSubKeyInfo", rownames= T, append = T)

##########
## After indexes calculation I plot results 
##########

index <- Indices_2019_08_30_15_50_58
# index <- gsub(NULL, NA, index)
index[index == 0] <- NA

index <- tidyr::gather(index, Age, value, Age_0:Age_15, factor_key=TRUE)

library(ggplot2)
index[is.na(index)] <- 0
ggplot(index, aes(x=Age, y=value, group = method, colour = method)) +
        geom_line()+ facet_wrap(~IndexArea, scales = "free")

ggsave("Indexes_comp_2018q1_ALKold_newSubst.tiff", units= "mm", width = 350, height = 175)

