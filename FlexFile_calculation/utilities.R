# utilities.R
# Three functions get to resume most algorithms:
# 1st calculate_DS_WS, this function deals with the most common case when no conditions on the sweep
# length or years selection happens

# 2nd calculate_DS_WS_sweeps, this function separates the data according to sweep lengths above or
# below 60 and then applies the same logic as the function before

# 3rd calculate_DS_WS_nsibts is only used for NL, GB and DE NS-IBTS algorithms,
# as these algorithms are too specific and difficult to include in a broader frame.
# Further work to depend less in the script values and more in parameters could be easily be done.

calculate_DS_WS <- function(data) {
  

  # 1st DoorSpread with the Priority 1 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
  subset <- data %>% filter(is.na(DoorSpread))
  
  a <- as.numeric(formulas$a)
  b <- as.numeric(formulas$b)
  c <- as.numeric(formulas$c)
  g <- formulas$y
  
  variable <- paste0("data$", g)
  formulas$Formula <- gsub("log", "log10", formulas$Formula)
  formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  
  data <- transform(data, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas$Formula))))
  
  # 2nd DoorSpread with the Priority 2 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
  subset <- HH %>% filter(is.na(DoorSpread))
  
  a <- as.numeric(formulas$a)
  b <- as.numeric(formulas$b)
  c <- as.numeric(formulas$c)
  g <- formulas$y
  
  variable <- paste0("data$", g)
  
  formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  formulas$Formula <- gsub("log", "log10", formulas$Formula)
  
  if(nrow(formulas) > 0){
  data <- transform(data, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas$Formula))))
  }
  # 3rd WingSpread with Priority 1 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  subset <- HH %>% filter(is.na(WingSpread))
  
  a <- as.numeric(formulas$a)
  b <- as.numeric(formulas$b)
  c <- as.numeric(formulas$c)
  g <- formulas$y
  
  variable <- paste0("data$", g)
  
  formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  formulas$Formula <- gsub("log", "log10", formulas$Formula)
  # formulas$Formula <- gsub("x=", "", formulas$Formula)
  # formulas$Formula <- gsub("subset$", "", formulas$Formula)
  
  data <- transform(data, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas$Formula))))
  
  #4th WingSpread with the Priority 2 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  subset <- HH %>% filter(is.na(WingSpread))
  
  a <- as.numeric(formulas$a)
  b <- as.numeric(formulas$b)
  c <- as.numeric(formulas$c)
  g <- formulas$y
  
  variable <- paste0("data$", g)
  
  formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  formulas$Formula <- gsub("log", "log10", formulas$Formula)
  
  if(nrow(formulas) > 0){
  data <- transform(data, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas$Formula))))
  }
  
  # In case DoorSpread is calculated from wingspread, I do a second round here from
  # Cal_WingSpread. REVIEW THIS
  # formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,x == "DoorSpread")
  # if(formulas$y == "WingSpread"){
  #   subset <- HH %>% filter(is.na(DoorSpread))
  #   
  #   a <- as.numeric(formulas$a)
  #   b <- as.numeric(formulas$b)
  #   c <- as.numeric(formulas$c)
  #   g <- formulas$y
  #   
  #   variable <- paste0("subset$", g)
  #   
  #   formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  #   
  #   if(nrow(formulas) > 0){
  #     data <- transform(data, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas$Formula))))
  #   }
  # }
  # 
  
  data[is.na(data)]<- "-9" 
  data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
  data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
  data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))
  data$DateofCalculation <- cal_date
  data$Cal_Distance <- as.numeric(data$Cal_Distance)
  data$Cal_DoorSpread <- as.numeric(data$Cal_DoorSpread)
  data$Cal_WingSpread <- as.numeric(data$Cal_WingSpread)
  
  
  
  data <- data %>%
    mutate(
      SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
      SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000) 
}



calculate_DS_WS_sweeps <- function(data) {
  #this function deals with the most common case when conditions on the sweep
  #length or years selection happens
  
  # 1st DoorSpread with the Priority 1 formula
  if(survey == "NS-IBTS"){
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  }else(formulas <- WKSAE_algorithms %>% filter(Survey == survey,Priority == 1))
  formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
  data1 <- data %>% filter(SweepLngt<= 60)
  subset <- data1 %>% filter(is.na(DoorSpread))
  formulas1 <- formulas %>% filter(SweepLengthMax ==60)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  c <- as.numeric(formulas1$c)
  g <- formulas1$y
  
  variable <- paste0("data1$", g)
  formulas1$Formula <- gsub("log", "log10", formulas1$Formula)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas1$Formula))))
  
  data2 <- data %>% filter(SweepLngt > 60)
  subset <- data2 %>% filter(is.na(DoorSpread))
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  c <- as.numeric(formulas2$c)
  g <- formulas2$y
  
  variable <- paste0("data2$", g)
  formulas2$Formula <- gsub("log", "log10", formulas2$Formula)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  
  data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas2$Formula))))
  
  
  # 2nd DoorSpread with the Priority 2 formula
  if(survey == "NS-IBTS"){
    formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  }else(formulas <- WKSAE_algorithms %>% filter(Survey == survey,Priority == 2))
  formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
  formulas1 <- formulas %>% filter(SweepLengthMax ==60)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  c <- as.numeric(formulas1$c)
  g <- formulas1$y
  
  variable <- paste0("data1$", g)
  formulas1$Formula <- gsub("log", "log10", formulas1$Formula)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas1) > 0){
  data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas1$Formula))))
  }
  
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  c <- as.numeric(formulas2$c)
  g <- formulas2$y
  
  variable <- paste0("data2$", g)
  formulas2$Formula <- gsub("log", "log10", formulas2$Formula)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas2) > 0){
  data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas2$Formula))))
  }
  
  # 3rd WingSpread with Priority 1 formula
  if(survey == "NS-IBTS"){
    formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  }else(formulas <- WKSAE_algorithms %>% filter(Survey == survey,Priority == 1))
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  subset <- data1 %>% filter(is.na(WingSpread))
  formulas1 <- formulas %>% filter(SweepLengthMax ==60)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  c <- as.numeric(formulas1$c)
  g <- formulas1$y
  
  variable <- paste0("data1$", g)
  formulas1$Formula <- gsub("log", "log10", formulas1$Formula)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas1$Formula))))
  
  subset <- data2 %>% filter(is.na(WingSpread))
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  c <- as.numeric(formulas2$c)
  g <- formulas2$y
  
  variable <- paste0("data2$", g)
  formulas2$Formula <- gsub("log", "log10", formulas2$Formula)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  
  data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas2$Formula))))
  
  #4th WingSpread with the Priority 2 formula
  if(survey == "NS-IBTS"){
    formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  }else(formulas <- WKSAE_algorithms %>% filter(Survey == survey,Priority == 2))
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  formulas1 <- formulas %>% filter(SweepLengthMax ==60)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  c <- as.numeric(formulas1$c)
  g <- formulas1$y
  
  variable <- paste0("data1$", g)
  formulas1$Formula <- gsub("log", "log10", formulas1$Formula)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas1) > 0){
    data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas1$Formula))))
  }
  
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  c <- as.numeric(formulas2$c)
  g <- formulas2$y
  
  variable <- paste0("data2$", g)
  formulas2$Formula <- gsub("log", "log10", formulas2$Formula)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas2) > 0){
  data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas2$Formula))))
  }
  data <- rbind(data1, data2)
  
  data[is.na(data)]<- "-9" 
  data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
  data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
  data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))
  data$DateofCalculation <- cal_date
  data$Cal_Distance <- as.numeric(data$Cal_Distance)
  data$Cal_DoorSpread <- as.numeric(data$Cal_DoorSpread)
  data$Cal_WingSpread <- as.numeric(data$Cal_WingSpread)
  
  
  data <- data %>%
    mutate(
      SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
      SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000) 
}

calculate_DS_WS_sweeps_de <- function(data) {
  #this function deals with the most common case when conditions on the sweep
  #length or years selection happens
  
  # 1st DoorSpread with the Priority 1 formula
  if(survey == "NS-IBTS"){
    formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  }else(formulas <- WKSAE_algorithms %>% filter(Survey == survey,Priority == 1))
  formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
  data1 <- data %>% filter(SweepLngt<= 50)
  subset <- data1 %>% filter(is.na(DoorSpread))
  formulas1 <- formulas %>% filter(SweepLengthMax ==50)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  c <- as.numeric(formulas1$c)
  g <- formulas1$y
  h <- formulas1$z
  
  variabley <- paste0("data1$", g)
  variablez <- paste0("data1$", h)
  formulas1$Formula <- gsub("log", "log10", formulas1$Formula)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variabley)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'z', replacement=variablez)
  
  data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas1$Formula))))
  
  data2 <- data %>% filter(SweepLngt > 50)
  subset <- data2 %>% filter(is.na(DoorSpread))
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  c <- as.numeric(formulas2$c)
  g <- formulas2$y
  h <- formulas2$z
  
  variabley <- paste0("data2$", g)
  variablez <- paste0("data2$", h)
  formulas2$Formula <- gsub("log", "log10", formulas2$Formula)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variabley)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'z', replacement=variablez)
  
  data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas2$Formula))))
  
  
  # 2nd DoorSpread with the Priority 2 formula
  if(survey == "NS-IBTS"){
    formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  }else(formulas <- WKSAE_algorithms %>% filter(Survey == survey,Priority == 2))
  formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
  formulas1 <- formulas %>% filter(SweepLengthMax ==50)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  c <- as.numeric(formulas1$c)
  g <- formulas1$y
  h <- formulas1$z
  
  variabley <- paste0("data1$", g)
  variablez <- paste0("data1$", h)
  formulas1$Formula <- gsub("log", "log10", formulas1$Formula)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variabley)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'z', replacement=variablez)
  
  if(nrow(formulas1) > 0){
    data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas1$Formula))))
  }
  
  # formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  # a <- as.numeric(formulas2$a)
  # b <- as.numeric(formulas2$b)
  # c <- as.numeric(formulas2$c)
  # g <- formulas2$y
  # h <- formulas2$z
  # 
  # variabley <- paste0("data2$", g)
  # variablez <- paste0("data2$", h)
  # formulas2$Formula <- gsub("log", "log10", formulas1$Formula)
  # formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variabley)
  # formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'z', replacement=variablez)
  # 
  # if(nrow(formulas2) > 0){
  #   data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas2$Formula))))
  # }
  # 
  # 3rd WingSpread with Priority 1 formula
  if(survey == "NS-IBTS"){
    formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  }else(formulas <- WKSAE_algorithms %>% filter(Survey == survey,Priority == 1))
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  subset <- data1 %>% filter(is.na(WingSpread))
  formulas1 <- formulas %>% filter(SweepLengthMax ==50)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  c <- as.numeric(formulas1$c)
  d <- as.numeric(formulas1$d)
  g <- formulas1$y
  h <- formulas1$z
  i <- formulas1$q
  
  variabley <- paste0("data1$", g)
  variablez <- paste0("data1$", h)
  variableq <- paste0("data1$", i)
  formulas1$Formula <- gsub("log", "log10", formulas1$Formula)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variabley)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'z', replacement=variablez)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'q', replacement=variableq)
  
  
  data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas1$Formula))))
  
  subset <- data2 %>% filter(is.na(WingSpread))
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  c <- as.numeric(formulas2$c)
  d <- as.numeric(formulas2$d)
  g <- formulas2$y
  h <- formulas2$z
  i <- formulas2$q
  
  variabley <- paste0("data2$", g)
  variablez <- paste0("data2$", h)
  variableq <- paste0("data2$", i)
  formulas2$Formula <- gsub("log", "log10", formulas2$Formula)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variabley)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'z', replacement=variablez)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'q', replacement=variableq)
  
  data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas2$Formula))))
  
  #4th WingSpread with the Priority 2 formula
  if(survey == "NS-IBTS"){
    formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  }else(formulas <- WKSAE_algorithms %>% filter(Survey == survey,Priority == 2))
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  formulas1 <- formulas %>% filter(SweepLengthMax ==50)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  c <- as.numeric(formulas1$c)
  g <- formulas1$y
  
  variable <- paste0("data1$", g)
  formulas1$Formula <- gsub("log", "log10", formulas1$Formula)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas1) > 0){
    data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas1$Formula))))
  }
  
  # formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  # a <- as.numeric(formulas2$a)
  # b <- as.numeric(formulas2$b)
  # c <- as.numeric(formulas2$c)
  # g <- formulas2$y
  # 
  # variable <- paste0("data2$", g)
  # formulas2$Formula <- gsub("log", "log10", formulas2$Formula)
  # formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  # 
  # if(nrow(formulas2) > 0){
  #   data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas2$Formula))))
  # }
  data <- rbind(data1, data2)
  
  data[is.na(data)]<- "-9" 
  data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
  data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
  data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))
  data$DateofCalculation <- cal_date
  data$Cal_Distance <- as.numeric(data$Cal_Distance)
  data$Cal_DoorSpread <- as.numeric(data$Cal_DoorSpread)
  data$Cal_WingSpread <- as.numeric(data$Cal_WingSpread)
  
  
  data <- data %>%
    mutate(
      SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
      SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000) 
}

calculate_DS_WS_nl <- function(data) {
  # calculating missing DoorSpread and Wingspread
  
  country <-  "NL" 
  data <- data %>%
    mutate(Cal_DoorSpread = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          case_when(
            Year <= 2004 ~ (29.544 * log10(Depth) + 14.116 * log10(Warplngt) + -3.456),
            Year %in% 2005:2014 ~ (31.165 * log10(Depth) + 0.2974 * log10(Warplngt) + 29.321),
            Year %in% 2015:2016 ~ (28.947 * log10(Depth) + 23.372 * log10(Warplngt) - 32.476),
            Year > 2016 ~ (15.842 * log10(Depth) + 30.868 * log10(Warplngt) + -24.793)
          )
        ),
        Cal_WingSpread = ifelse(
          !is.na(WingSpread),
          WingSpread,
          0.1909 * Cal_DoorSpread + 4.011
        )
      )
   
  data[is.na(data)]<- "-9" 
  data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
  data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
  data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))
  data$DateofCalculation <- cal_date
  data$Cal_Distance <- as.numeric(data$Cal_Distance)
  data$Cal_DoorSpread <- as.numeric(data$Cal_DoorSpread)
  data$Cal_WingSpread <- as.numeric(data$Cal_WingSpread)
  
  
  data %>%
    mutate(
      SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
      SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000
    )

}

calculate_DS_WS_nsibtsGB <- function(data) {
        # Separate data for 2006 and the rest
        data1 <- data %>% filter(Year == 2006)
        
        # 1st DoorSpread with the Priority 1 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1, InitialYear == 2006)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
        subset <- data1 %>% filter(is.na(DoorSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data1$", g)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        
        data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas$Formula))))
        
        # 2nd DoorSpread with the Priority 2 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2, InitialYear == 2006)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
        #subset <- HH %>% filter(is.na(DoorSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data1$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        
        if(nrow(formulas) > 0){
                data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas$Formula))))
        }
        
        # 3rd DoorSpread with the Priority 3 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 3, InitialYear == 2006)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
        #subset <- HH %>% filter(is.na(DoorSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data1$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        
        if(nrow(formulas) > 0){
                data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas$Formula))))
        }
        
        # 4th WingSpread with Priority 1 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1, InitialYear == 2006)
        formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
        # subset <- HH %>% filter(is.na(WingSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data1$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        # formulas$Formula <- gsub("x=", "", formulas$Formula)
        # formulas$Formula <- gsub("subset$", "", formulas$Formula)
        
        data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas$Formula))))
        
        #5th WingSpread with the Priority 2 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2, InitialYear == 2006)
        formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
        #subset <- HH %>% filter(is.na(WingSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data1$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        
        if(nrow(formulas) > 0){
                data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas$Formula))))
        }
        
        #6th WingSpread with the Priority 3 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 3, InitialYear == 2006)
        formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
        #subset <- HH %>% filter(is.na(WingSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data1$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        
        if(nrow(formulas) > 0){
                data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas$Formula))))
        }
        
        
        data2 <- data %>% filter(Year != 2006)
        
        # 1st DoorSpread with the Priority 1 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1, InitialYear == 2004)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
        subset <- data2 %>% filter(is.na(DoorSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data2$", g)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        
        data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas$Formula))))
        
        # 2nd DoorSpread with the Priority 2 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2, InitialYear == 2004)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
        #subset <- HH %>% filter(is.na(DoorSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data2$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        
        if(nrow(formulas) > 0){
                data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas$Formula))))
        }
        
        # 3rd DoorSpread with the Priority 3 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 3, InitialYear == 2004)
        formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
        #subset <- HH %>% filter(is.na(DoorSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data1$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        
        if(nrow(formulas) > 0){
                data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas$Formula))))
        }
        
        # 4th WingSpread with Priority 1 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1, InitialYear == 2004)
        formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
        # subset <- HH %>% filter(is.na(WingSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data2$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        # formulas$Formula <- gsub("x=", "", formulas$Formula)
        # formulas$Formula <- gsub("subset$", "", formulas$Formula)
        
        data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas$Formula))))
        
        #5th WingSpread with the Priority 2 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2, InitialYear == 2004)
        formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
        #subset <- HH %>% filter(is.na(WingSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data2$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        
        if(nrow(formulas) > 0){
                data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas$Formula))))
        }
        
        #6th WingSpread with the Priority 3 formula
        formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 3, InitialYear == 2004)
        formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
        #subset <- HH %>% filter(is.na(WingSpread))
        
        a <- as.numeric(formulas$a)
        b <- as.numeric(formulas$b)
        c <- as.numeric(formulas$c)
        g <- formulas$y
        
        variable <- paste0("data2$", g)
        
        formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        formulas$Formula <- gsub("log", "log10", formulas$Formula)
        
        if(nrow(formulas) > 0){
                data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas$Formula))))
        }
        
        # In case DoorSpread is calculated from wingspread, I do a second round here from
        # Cal_WingSpread. REVIEW THIS
        # formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,x == "DoorSpread")
        # if(formulas$y == "WingSpread"){
        #   subset <- HH %>% filter(is.na(DoorSpread))
        #   
        #   a <- as.numeric(formulas$a)
        #   b <- as.numeric(formulas$b)
        #   c <- as.numeric(formulas$c)
        #   g <- formulas$y
        #   
        #   variable <- paste0("subset$", g)
        #   
        #   formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
        #   
        #   if(nrow(formulas) > 0){
        #     data <- transform(data, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas$Formula))))
        #   }
        # }
        # 
        data <- rbind(data1, data2)
        
        data[is.na(data)]<- "-9" 
        data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
        data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
        data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))
        data$DateofCalculation <- cal_date
        data$Cal_Distance <- as.numeric(data$Cal_Distance)
        data$Cal_DoorSpread <- as.numeric(data$Cal_DoorSpread)
        data$Cal_WingSpread <- as.numeric(data$Cal_WingSpread)
        
        
        
        data <- data %>%
                mutate(
                        SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
                        SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000) 
}

