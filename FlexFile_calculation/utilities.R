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
  g <- formulas$y
  
  variable <- paste0("subset$", g)
  formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  
  data <- transform(data, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas$Formula))))
  
  # 2nd DoorSpread with the Priority 2 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
  subset <- HH %>% filter(is.na(DoorSpread))
  
  a <- as.numeric(formulas$a)
  b <- as.numeric(formulas$b)
  g <- formulas$y
  
  variable <- paste0("subset$", g)
  
  formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas) > 0){
  data <- transform(data, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas$Formula))))
  }
  # 3rd WingSpread with Priority 1 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  subset <- HH %>% filter(is.na(WingSpread))
  
  a <- as.numeric(formulas$a)
  b <- as.numeric(formulas$b)
  g <- formulas$y
  
  variable <- paste0("subset$", g)
  
  formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  
  data <- transform(data, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas$Formula))))
  
  #4th WingSpread with the Priority 2 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  subset <- HH %>% filter(is.na(WingSpread))
  
  a <- as.numeric(formulas$a)
  b <- as.numeric(formulas$b)
  g <- formulas$y
  
  variable <- paste0("subset$", g)
  
  formulas$Formula <- lapply(formulas$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas) > 0){
  data <- transform(data, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas$Formula))))
  }
  data[is.na(data)]<- "-9" 
  data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
  data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
  data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))
  data$DateofCalculation <- cal_date
  data$Cal_Distance <- as.numeric(data$Cal_Distance)
  data$Cal_DoorSpread <- as.numeric(data$Cal_DoorSpread)
  
  
  data <- data %>%
    mutate(
      SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
      SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000) 
}



calculate_DS_WS_sweeps <- function(data) {
  #this function deals with the most common case when conditions on the sweep
  #length or years selection happens
  
  # 1st DoorSpread with the Priority 1 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  formulas <- formulas %>% filter(formulas$x == "DoorSpread" | formulas$x=="Doorspread")
  data1 <- data %>% filter(SweepLngt<= 60)
  subset <- data1 %>% filter(is.na(DoorSpread))
  formulas1 <- formulas %>% filter(SweepLengthMax ==60)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  g <- formulas1$y
  
  variable <- paste0("subset$", g)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas1$Formula))))
  
  data2 <- data %>% filter(SweepLngt > 60)
  subset <- data2 %>% filter(is.na(DoorSpread))
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  g <- formulas2$y
  
  variable <- paste0("subset$", g)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  
  data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(DoorSpread), DoorSpread, eval(parse(text=formulas1$Formula))))
  
  
  # 2nd DoorSpread with the Priority 2 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  formulas <- formulas %>% filter(formulas$x == "DoorSpread"| formulas$x=="Doorspread")
  formulas1 <- formulas %>% filter(SweepLengthMax ==60)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  g <- formulas1$y
  
  variable <- paste0("subset$", g)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas1) > 0){
  data1 <- transform(data1, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas1$Formula))))
  }
  
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  g <- formulas2$y
  
  variable <- paste0("subset$", g)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas2) > 0){
  data2 <- transform(data2, Cal_DoorSpread = ifelse(!is.na(Cal_DoorSpread), Cal_DoorSpread, eval(parse(text=formulas1$Formula))))
  }
  
  # 3rd WingSpread with Priority 1 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 1)
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  subset <- data1 %>% filter(is.na(WingSpread))
  formulas1 <- formulas %>% filter(SweepLengthMax ==60)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  g <- formulas1$y
  
  variable <- paste0("subset$", g)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas1$Formula))))
  
  subset <- data2 %>% filter(is.na(WingSpread))
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  g <- formulas2$y
  
  variable <- paste0("subset$", g)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  
  data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(WingSpread), WingSpread, eval(parse(text=formulas1$Formula))))
  
  #4th WingSpread with the Priority 2 formula
  formulas <- WKSAE_algorithms %>% filter(Country== country, Survey == survey,Priority == 2)
  formulas <- formulas %>% filter(formulas$x == "WingSpread"| formulas$x=="Wingspread")
  formulas1 <- formulas %>% filter(SweepLengthMax ==60)
  a <- as.numeric(formulas1$a)
  b <- as.numeric(formulas1$b)
  g <- formulas1$y
  
  variable <- paste0("subset$", g)
  formulas1$Formula <- lapply(formulas1$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas1) > 0){
    data1 <- transform(data1, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas1$Formula))))
  }
  
  formulas2 <- formulas %>% filter(SweepLengthMax == "else")
  a <- as.numeric(formulas2$a)
  b <- as.numeric(formulas2$b)
  g <- formulas2$y
  
  variable <- paste0("subset$", g)
  formulas2$Formula <- lapply(formulas2$Formula, gsub, pattern= 'y', replacement=variable)
  
  if(nrow(formulas2) > 0){
  data2 <- transform(data2, Cal_WingSpread = ifelse(!is.na(Cal_WingSpread), Cal_WingSpread, eval(parse(text=formulas1$Formula))))
  }
  data <- rbind(data1, data2)
  
  data[is.na(data)]<- "-9" 
  data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
  data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
  data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))
  data$DateofCalculation <- cal_date
  data$Cal_Distance <- as.numeric(data$Cal_Distance)
  data$Cal_DoorSpread <- as.numeric(data$Cal_DoorSpread)
  
  
  data <- data %>%
    mutate(
      SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
      SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000) 
}


calculate_DS_WS_nsibts <- function(data) {
  # assume only one country
  country <- data$Country[1]
  cat(country, "\n")

  # calculating missing DoorSpread and Wingspread
  
  if (country == "NL") {
    data <- data %>%
      mutate(
        Cal_DoorSpread = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          case_when(
            Year <= 2004 ~ (29.544 * log10(Depth) + 14.116 * log10(Warplngt) + -3.456),
            Year %in% 2005:2014 ~ (31.165 * log10(Depth) + 0.2974 * log10(Warplngt) + 29.321),
            Year %in% 2015:2016 ~ (28.947 * log10(Depth) + 23.372 * log10(Warplngt) - 32.476),
            Year > 2016 ~ (15.842 * log10(Depth) + 30.868 * log10(Warplngt) + -24.793)
          )
        ),
        # I can't see any formula for wingspread in NL
        Cal_WingSpread = ifelse(
          !is.na(WingSpread),
          WingSpread,
          0.1909 * Cal_DoorSpread + 4.011
        )
      )
  } 
  else if (country == "GB") {
    data <- data %>%
      mutate(
        Cal_DoorSpread = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          case_when(
            Year <= 2005 & is.na(Warplngt) & is.na(WingSpread) ~ (15.0306 * log(Depth) + 12.6399),
            Year <= 2005 & !is.na(Warplngt) & is.na(WingSpread) ~ (21.78 * log(Warplngt) - 47.2),
            Year <= 2005 & is.na(Warplngt) & !is.na(WingSpread) ~ (4.616 * WingSpread - 15.966),
            Year = 2006 & is.na(Warplngt) & is.na(WingSpread) ~ (12.4680 * log(Depth) + 17.5865),
            Year = 2006 & !is.na(Warplngt) & is.na(WingSpread) ~ (16.4421 * log(Warplngt) - 24.4727),
            Year = 2006 & is.na(Warplngt) & !is.na(WingSpread) ~ (3.8182 * WingSpread - 11.9066),
            Year <= 2007 & is.na(Warplngt) & is.na(WingSpread) ~ (15.0306 * log(Depth) + 12.6399),
            Year <= 2007 & !is.na(Warplngt) & is.na(WingSpread) ~ (21.78 * log(Warplngt) - 47.2),
            Year <= 2007 & is.na(Warplngt) & !is.na(WingSpread) ~ (4.616 * WingSpread - 15.966)
          )
        ),
        Cal_WingSpread = ifelse(
          !is.na(WingSpread),
          WingSpread,
          case_when(
            Year <= 2005 & is.na(Warplngt) & is.na(DoorSpread) ~ (15.0306 * log(Depth) + 12.6399),
            Year <= 2005 & !is.na(Warplngt) & is.na(DoorSpread) ~ (21.78 * log(Warplngt) - 47.2),
            Year <= 2005 & is.na(Warplngt) & !is.na(DoorSpread) ~ (4.616 * DoorSpread - 15.966),
            Year = 2006 & is.na(Warplngt) & is.na(WingSpread) ~ (3.1495 * log(Depth) + 8.2192),
            Year = 2006 & !is.na(Warplngt) & is.na(WingSpread) ~ (4.1885 * log(Warplngt) - 2.8637),
            Year = 2006 & is.na(Warplngt) & !is.na(WingSpread) ~ (0.2242 * DoorSpread + 5.7889),
            Year <= 2007 & is.na(Warplngt) & is.na(WingSpread) ~ (15.0306 * log(Depth) + 12.6399),
            Year <= 2007 & !is.na(Warplngt) & is.na(WingSpread) ~ (21.78 * log(Warplngt) - 47.2),
            Year <= 2007 & is.na(Warplngt) & !is.na(WingSpread) ~ (4.616 * DoorSpread - 15.966)
          )
        )
      )

    } else if (country == "DE") {
    data <- data %>%
      mutate(
        Cal_DoorSpread = ifelse(
          !is.na(DoorSpread),
          DoorSpread,
          ifelse(
            SweepLngt <= 50,
            -7.456 + 3.616 * WingSpread + 3.124 * log(Depth),
            -0.441 + 10.009 * log(Warplngt) + 4.768 * log(Depth)
          )
        ),
        Cal_WingSpread = ifelse(
          !is.na(WingSpread),
          WingSpread,
          case_when(
            !is.na(DoorSpread) & SweepLngt <= 50 ~ 3.359 + 0.095 * DoorSpread + 1.391 * log(Warplngt) + 0.261 * log(Depth),
            !is.na(DoorSpread) & SweepLngt > 50 ~ 3.087 + 0.118 * DoorSpread + 0.445 * log(Warplngt) + 0.368 * log(Depth),
            is.na(DoorSpread) & SweepLngt <= 50 ~ 3.317 + 2.341 * log(Warplngt) + 0.713 * log(Depth),
            is.na(DoorSpread) & SweepLngt > 50 ~ 3.087 + 0.118 * Cal_DoorSpread + 0.445 * log(Warplngt) + 0.368 * log(Depth)
          )
        )
      ) %>%
      mutate(
        Cal_DoorSpread =
          ifelse(
            is.na(DoorSpread) & is.na(WingSpread) & SweepLngt <= 50,
            -7.935 + (5.123 * Cal_WingSpread) + 2.366 * log(Depth),
            Cal_DoorSpread
          )
      )
  } else {
    return(NULL)
  }

  data[is.na(data)]<- "-9" 
  data <- transform(data, DSflag = ifelse(Cal_DoorSpread == DoorSpread,"O", "C"))
  data <- transform(data, WSflag = ifelse(Cal_WingSpread == WingSpread,"O", "C"))
  data <- transform(data, DistanceFlag = ifelse(Cal_Distance == Distance,"O", "C"))
  data$DateofCalculation <- cal_date
  
  data %>%
    mutate(
      SweptAreaDSKM2 = Cal_Distance * Cal_DoorSpread / 1000000,
      SweptAreaWSKM2 = Cal_Distance * Cal_WingSpread / 1000000
    )

}
