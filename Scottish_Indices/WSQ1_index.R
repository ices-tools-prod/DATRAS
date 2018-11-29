
#######################################################
#######################################################
##
##  Calculation of abundance index in IBTS WS Q1 
##
#######################################################
#######################################################

##### R version 3.0.2 (2015-02-12)
##### (it runs also in R version 3.3.2)

######################################
# Main specifications
######################################

# rm(list=ls())

##### If packages 'splancs' and 'sp' are not installed, install them first from the attached zip files
library(splancs)  ##### The package uses the function 'inout'

path <- "C:/My files/Surveys/Abundance indices/Q1 indices/2016/"

year <- 2016
trip <- "S20160003"

##### Function to format the coordinates
lonlat2xy <- function(coordinates) { names(coordinates) <- c("x", "y"); return(coordinates) }


##########################################
# Read survey data for the year
##########################################

hauls <- read.table(paste(path, "hauls.txt", sep=""), header=T, colClasses=c("character", "numeric", 
  rep("character", 5), rep("numeric", 8), "logical", "character", rep("numeric", 3)), as.is=T)
fish <- read.table(paste(path, "fish.txt", sep=""), header=T, as.is=T)
malks <- read.table(paste(path, "malks.txt", sep=""), header=T, as.is=T)

##### Select data for the trip
hauls <- hauls[hauls$trip==trip, ]
dim(hauls)
# head(hauls)
fish <- fish[fish$trip==trip, ]
dim(fish)
# head(fish, 20)
malks <- malks[malks$trip==trip, ]
dim(malks)
# head(malks)

standard.tow.duration <- 60

##### Note that for the calculation of abundance indices, standard tow duration is set here to 60 min,
##### although it usually is 30 min (typical tow duration)

fish[, "standnumber"] <- fish[, "number"] * standard.tow.duration / hauls[match(fish$triphaul, hauls$triphaul), "duration"]
# head(fish, 20)

###############################################
# Read haul data by stratum
###############################################

strata.names <- c("red1", "red2", "red3", "green1", "green2", "blue1", "blue2", "lightblue", "pink", "clyde", "windsock")
strata.areas <- c(7320.879, 4350.4, 12000.51, 25968.04, 7383.917, 7658.542, 8501.4, 9035.106, 11874.72, 2667.681, 5182.958) # in km2

##### Only valid hauls from the list
chrono <- hauls[hauls$valid, ]
chrono[, "stratum"] <- as.character(NA)

##### Strata coordinates
strata.coord <- read.table(paste(path, "Coordinates.txt", sep=""), header=T, as.is=T)

##### More info in which stratum the haul was taken
for(i in 1:nrow(chrono)) 
  for (k in 1:length(strata.names)) 
    if(inout(lonlat2xy(chrono[i, c("lon", "lat")]), lonlat2xy(strata.coord[strata.coord$stratum==strata.names[k], c("lon", "lat")]))) 
      chrono[i, "stratum"] <- strata.names[k]

##### Map with strata and hauls
##### (a check if all hauls are within the strata)
if (F) {
  x1lim <- -10.5; x2lim <- -4; y1lim <- 54.5; y2lim <- 60.5
  par(mar=c(3, 3, 0.5, 0.5), las=1)
  plot(NA, xlab="Longitude", ylab="Latitude", type="n", ylim=c(y1lim, y2lim) , xlim=c(x1lim, x2lim))
  for(i in 1:length(strata.names)) polygon(strata.coord[strata.coord$stratum==strata.names[i], c("lon", "lat")], 
    col=c(2, 2, 2, 3, 3, 4, 4, 5, 6, 7, 7)[i], border=NA) 
  for(j in 1:nrow(chrono)) {
    if (is.na(chrono[j, "stratum"])) points(chrono[j, c("lon", "lat")], pch="?") else
    if (substr(chrono[j, "stratum"], 1, 3)=="red") points(chrono[j, c("lon", "lat")], pch=1) else
    if (substr(chrono[j, "stratum"], 1, 5)=="green") points(chrono[j, c("lon", "lat")], pch=16) else
    if (substr(chrono[j, "stratum"], 1, 4)=="blue") points(chrono[j, c("lon", "lat")], pch=2) else
    if (chrono[j, "stratum"]=="lightblue") points(chrono[j, c("lon", "lat")], pch=17) else
    if (chrono[j, "stratum"]=="pink") points(chrono[j, c("lon", "lat")], pch=4) else
    if (chrono[j, "stratum"]=="clyde" | chrono[j, "stratum"]=="windsock") points(chrono[j, c("lon", "lat")], pch=3)
                           }
       }

##### In a few cases, the haul positions were just outside the strata => the stratum for the haul was assumed
# No such hauls in this trip

##### Sampled strata  
##### - a check if there were hauls in each substratum
in.strata <- logical(length(strata.names))
for (j in 1:length(strata.names)) 
  in.strata[j] <- is.element(strata.names[j], chrono$stratum)
print(in.strata)

##### Hauls in strata
hauls.in.strata <- list(strata.names)
for (i in 1:length(strata.names)) hauls.in.strata[[i]] <- chrono[is.element(chrono$stratum, strata.names[i]), "haul"]
names(hauls.in.strata) <- strata.names
print(hauls.in.strata)

########################################
# Which species?
########################################

##### Calculate the index for the listed species... 
##### or for any species found in "malks"...
##### or just for one species

aged.species <- c("COD", "HAD", "WHI", "SAI", "NPO", "HER", "MAC", "SPR")
# aged.species <- unique(malks[!is.na(malks$age), "species"])
# aged.species <- "HAD"

ages <- 0:21

##### Two tables to store the index and index variance
index.table <- matrix(ncol=2+length(ages), nrow=0, dimnames=list(NULL, c("species", "effort", paste("age", ages, sep=""))))
index.variance.table <- matrix(ncol=2+length(ages), nrow=0, dimnames=list(NULL, c("species", "effort", paste("age", ages, sep=""))))

##### Abundances indices are calculated per 10 hours
effort <- 10

#########################
# Calculations
#########################

for (s in 1:length(aged.species)) {

  species <- aged.species[s]

  ###################################################
  # Number of collected otoliths for species s
  ###################################################
  
  ##### By haul and age
  malks.species <- malks[malks$species==species, ]
  # head(malks.species)

  ##### !!!!! Note that the imported malks are in mm. Here, they are converted to cm.
  malks.species$length <- malks.species$length / 10
  # head(malks.species)
  # summary(malks.species)

  no.otoliths.by.haul.and.age <- matrix(numeric(), length(ages), length(chrono$haul),  
    dimnames=list(paste("age", as.character(ages), sep=""), as.character(chrono$haul)))
  for (i in 1:length(ages))
    for (j in 1:length(chrono$haul)) {
      num <- nrow(malks.species[!is.na(malks.species$age) & malks.species$age==ages[i] & malks.species$haul==chrono$haul[j], ])
      no.otoliths.by.haul.and.age[i, j] <- ifelse(length(num)==0, 0, num)
                                     }
  # print(no.otoliths.by.haul.and.age)

  ##### By stratum and age
  no.otoliths.by.stratum.and.age <- matrix(numeric(), length(ages), length(strata.names), 
    dimnames=list(paste("age", as.character(ages), sep=""), strata.names))
  for (k in 1:length(strata.names)) 
    no.otoliths.by.stratum.and.age[, k] <- 
      apply(as.matrix(no.otoliths.by.haul.and.age[, as.character(chrono[is.element(chrono$haul, hauls.in.strata[[k]]), "haul"])]), 1, sum)
  # print(no.otoliths.by.stratum.and.age)

  #########################################################
  # Raised fish numbers (per length per haul per 60 min)
  #########################################################

  fish.species <- fish[fish$species==species, ]
  # head(fish.species)

  ##### !!!!! Note that the imported length data are in mm. Here, they are converted to cm.
  fish.species$length <- fish.species$length / 10

  mix.length <- min(fish.species$length)
  max.length <- max(fish.species$length)
  lengths <- seq(mix.length, max.length, by=ifelse(species=="HER" | species=="SPR", 0.5, 1)) 
  no.at.length.by.haul <- matrix(numeric(), length(lengths), length(chrono$haul), dimnames=list(as.character(lengths), as.character(chrono$haul)))
  for (i in 1:length(lengths))
    for (j in 1:length(chrono$haul)) {
      stnum <- sum(fish.species[fish.species$length==lengths[i] & fish.species$haul==chrono$haul[j], "standnumber"])
      no.at.length.by.haul[i, j] <- ifelse(length(stnum)==0, 0, stnum)
                                     }
  # print(no.at.length.by.haul)

  #############################################
  # ALKs
  #############################################
  
  ##### By stratum
  alks.by.stratum <- list()
  for (k in 1:length(strata.names)) {
    stratum.hauls <- chrono[is.element(chrono$haul, hauls.in.strata[[k]]), ]
    malks.species.stratum <- malks.species[is.element(malks.species$haul, stratum.hauls$haul), ]
    alks.by.stratum[[k]] <- table(factor(malks.species.stratum$length, levels=as.character(lengths)), 
      factor(malks.species.stratum$age, levels=as.character(ages)))
    dimnames(alks.by.stratum[[k]])[[1]] <- paste(dimnames(alks.by.stratum[[k]])[[1]], " cm", sep="")
    dimnames(alks.by.stratum[[k]])[[2]] <- paste("age", dimnames(alks.by.stratum[[k]])[[2]], sep="")
    # print(strata.names[k]); print(alks.by.stratum[[k]]); cat("\n", "\n")
                                    }  
  names(alks.by.stratum) <- strata.names
  # print(alks.by.stratum)

  ##### Proportions by stratum  
  alks.by.stratum.prop <- alks.by.stratum
  for (k in 1:length(strata.names)) 
    alks.by.stratum.prop[[k]] <- prop.table(alks.by.stratum[[k]], margin=1)
  # print(alks.by.stratum.prop)

  #############################################
  # Numbers per age per haul per 10 hours
  #############################################

  ##### By (valid) haul 
  no.fish.by.haul.and.age <- no.otoliths.by.haul.and.age
  no.fish.by.haul.and.age[] <- NA
  for (j in 1:length(chrono$haul)) {
    haul <- as.numeric(dimnames(no.fish.by.haul.and.age)[[2]][j])
    absp <- alks.by.stratum.prop[[match(chrono[match(haul, chrono$haul), "stratum"], strata.names)]]
    no.at.length.by.age <- absp
    no.at.length.by.age[] <- NA
    for (i in 1:nrow(no.at.length.by.age)) 
      no.at.length.by.age[i, ] <- absp[i,] * no.at.length.by.haul[, as.character(haul)][i]
    no.fish.by.haul.and.age[, j] <- effort * apply(no.at.length.by.age, 2, function(x) sum(x, na.rm=T))
                                   }
  # print(no.fish.by.haul.and.age)

  ##### By stratum 
  no.fish.by.stratum.and.age <- no.otoliths.by.stratum.and.age
  no.fish.by.stratum.and.age[] <- NA
  var.by.stratum.and.age <- no.otoliths.by.stratum.and.age
  var.by.stratum.and.age[] <- NA
  for (k in 1:length(strata.names)) {
    stratum.hauls <- chrono[chrono$stratum==strata.names[k] & is.element(chrono$haul, chrono$haul), ]
    if (nrow(stratum.hauls) > 0) { 
      no.fish.by.stratum.and.age[, k] <- apply(as.matrix(no.fish.by.haul.and.age[, as.character(stratum.hauls$haul)]), 1, mean)
      var.by.stratum.and.age[, k] <- apply(as.matrix(no.fish.by.haul.and.age[, as.character(stratum.hauls$haul)]), 1, var) /
        apply(as.matrix(no.fish.by.haul.and.age[, as.character(stratum.hauls$haul)]), 1, length)
                                 }
                                    }
  # print(no.fish.by.stratum.and.age)
  # print(var.by.stratum.and.age)

  #############################################
  # Index mean number at age
  #############################################

  weight.by.surface.area <- TRUE

  if (weight.by.surface.area) {
    index <- as.data.frame(matrix(NA, 1, 2+length(ages)))	
    names(index) <- c("species", "effort", dimnames(no.fish.by.haul.and.age)[[1]])
    index$species <- species
    index$effort <- effort
    for (i in 1:length(ages)) index[1, i+2] <- sum(no.fish.by.stratum.and.age[i, ] * strata.areas) / sum(strata.areas) 
                              }

  index.table <- rbind(index.table, index)
  cat("Index for", species, "calculated", "\n") 

  if (weight.by.surface.area) {
    variance <- as.data.frame(matrix(NA, 1, 2+length(ages)))  
    names(variance) <- c("species", "effort", dimnames(no.fish.by.haul.and.age)[[1]])
    variance$species <- species
    variance$effort <- effort
    for (i in 1:length(ages)) variance[1, i+2] <- sum(var.by.stratum.and.age[i, ] * strata.areas^2) / (sum(strata.areas))^2 
                              }
  
  index.variance.table <- rbind(index.variance.table, variance)
  cat("Variance for", species, "calculated", "\n") 

                                 }  # end of "for s" 

##### Final table of index and variance
index.table[, 3:(2+length(ages))] <- round(index.table[, 3:(2+length(ages))], 3)
index.variance.table[, 3:(2+length(ages))] <- round(index.variance.table[, 3:(2+length(ages))], 3)


if (F) {
  write.table(index.table, paste(path, "Indices", year, ".txt", sep=""), sep="\t", quote=F, row.names=F)
  write.table(index.variance.table, paste(path, "Variances", year, ".txt", sep=""), sep="\t", quote=F, row.names=F)
       }







