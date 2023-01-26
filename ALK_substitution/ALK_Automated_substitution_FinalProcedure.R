#### Procedure to be called from SQL ###
#### Signe/2017/12/14, first part of connection to database ###
## Adriana, September 2019
## Colin and Vaishav, last part putting back table into DB
## clean up in June 2020 and then November 2020

#The aim of this script is:
# 1st to perform the automated substitution
# make some plots to check by eye that everything seems correct
# 2nd, to upload the supplemented ALK with the automated procedure to the database

library(dplyr)
library(ggplot2)
library(data.table)
library(RODBC)

#### Input information
survey <- 'NS-IBTS'
year <- 2022
quarter <- 3

file_root <- paste0(survey, year,"q", quarter,"_")

#### Connect to database
# settings
dbConnect_datras <- 'Driver={SQL Server};Server=SQL10;Database=DATRAS;Trusted_Connection=yes'
dbConnect_dwdatras <- 'Driver={SQL Server};Server=SQL10;Database=DW_DATRAS;Trusted_Connection=yes'
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
                GROUP BY Survey, Year, Quarter, Area, tblcodeidAphia, LngtClasMM, Age_0, Age_1, Age_2, Age_3, Age_4, Age_5, Age_6, Age_7, Age_8, Age_9, Age_10", survey, year, quarter)
df <- sqlQuery(conn_dwdatras, sqlq)

list <- unique(df$tblcodeidAphia)
sps <- sps %>% filter(tblcodeidAphia %in% list)

# sps <- sps[5,]

df <- merge(df,sps)

names(df)
colnames(df)[colnames(df)=="tblcodeidAphia"] <- "AphiaID"
colnames(df)[colnames(df)=="LngtClasMM"] <- "LngtClass"

unique(df$Species)

df$Species <- as.factor(df$Species)
df$Species <- droplevels(df$Species)
unique(df$Species)

# Load Borrowing areas scheme

BorrowArea <- read.csv("ALK_substitution/BorrowArea.csv")



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
        
 # ggplot(dat_long, aes(x=Age, y=LngtClass, size=Value, colour = label)) +
 #       geom_point(alpha=0.2)+ facet_grid(Species~Area, scales = "free")+
 #       theme(text = element_text(size=6), axis.text.x = element_text(angle = 45,hjust = 1))+
 #       scale_color_manual(values = c("blue", "red"))

# blue shows points with more than 25 measurements and red those with less
 # ggsave(paste0(file_root,"ALKnoSubstitution_COLOR.tiff"), units= "mm", width = 350, height = 175)

# not sure if I need this sum df later
dat_long[is.na(dat_long)] <- 0
sum <- dat_long %>% group_by(Species, Area, Age)%>% summarise(sum(Value))
colnames(sum) <- c("Species", "Area","Age","Value_initial")


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

# something wrong about these colours, check

# ggplot(proc2_dat_long, aes(x=Age, y=LngtClass, size=Value2, colour = label)) +
#         geom_point(alpha=0.2)+ facet_grid(Species~Area, scales = "free")+
#         theme(text = element_text(size=6), axis.text.x = element_text(angle = 45,hjust = 1))+
#         scale_color_manual(values = c("black", "red"))

# Black means that is been kept the same, red means that it has been supplemented

# ggsave(paste0(file_root,"ALKAutSubst_color.tiff"), units= "mm", width = 350, height = 175)

############
#~~~~~~~~~~~
# Prepare this new ALK and upload it to the database
#~~~~~~~~~~~

names(res3)
unique(res3$Age)
res3 <- res3 %>% filter(Age != "x[FALSE, ]")
res3$Survey <- survey
res3$Year <- year
res3$Quarter <- quarter      
res3$Lngtclas <- res3$LngtClass


bla <- df %>% select(c(AphiaID, Species))
bla <- unique(bla)

col.order <- names(df)

res3 <- merge(bla,res3, all = TRUE)

res3$Speccode <-res3$AphiaID
res3$NoAtALK <- res3$Value2
names(res3)
#res3 <- subset(res3, select = c("Survey", "Year", "Quarter", "Area", "Speccode", "Lngtclas", "Age", "NoAtALK", "Sex"))

#Check the plus group, needed for indeces calculation
# However it is set per species elsewhere, so this step could easily be removed

ca <- icesDatras::getCAdata(survey, year, quarter)
# 
plus_gr <- ca %>% select(Valid_Aphia, PlusGr)
# 
plus_gr <- unique(plus_gr)
plus_gr

# If all are NAs
res3$PlusGr <- "-9"

res3$LngtCode <- "."
res3$Sex <- 2297

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
res3$Sex <- as.integer(res3$Sex)

unique(res3$Age)
str(res3)
res3 <- subset(res3, select = c("Survey", "Year", "Quarter", "Area", "Speccode", "Lngtclas", "Age", "NoAtALK", "PlusGr", "LngtCode","Sex"))
# col.order(res3 <- subset(res3, select = c("Survey", "Year", "Quarter", "Area", "Speccode", "Lngtclas", "Age", "NoAtALK", "PlusGr", "LngtCode","Sex"))


q3_2022 <- res3

# res3 <- rbind(q12020, q32020, q12021)

#I connect to the test server for thew time being

# dbConnect_datras <- 'Driver={SQL Server Native Client 11.0};Server=SQL2019Dev;Database=DATRAS;Trusted_Connection=yes'
# 
# conn_datras <- odbcDriverConnect(connection = dbConnect_datras)

#dbWriteTable(conn_datras, name = "A_DATRAS_tblSubKeyInfotris", value = res3, row.names = TRUE)


#res3 <- res3[, c("A", "B", "C")]

# Have to make the right order of fields before uploading to db!!


dbConnect_datras <- 'Driver={SQL Server};Server=SQL10;Database=DATRAS;Trusted_Connection=yes'
# dbConnect_dwdatras <- 'Driver={SQL Server};Server=SQL10;Database=DW_DATRAS;Trusted_Connection=yes'
# connect
conn_datras <- odbcDriverConnect(connection = dbConnect_datras)
# conn_dwdatras <- odbcDriverConnect(connection = dbConnect_dwdatras)



# you need to rename the table, i can't get it to overwrite the existing one
sqlSave(conn_datras, q3_2022, tablename = "A_DATRAS_tblSubKeyInfo_q3_2022_v2", rownames= T, append = F)

# Inform Vaishav about this table, DW and indices calculations can be done then

