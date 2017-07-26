library(tidyverse)
#library(zoo) #<--- you'll need to install this!
#library(data.table) #<--- you'll need to install this!

processData <- function(df = NULL){
  ## processData expects a dataframe as an input, preferably the one you've just created using getData(). 
  ## If a dataframe is found then user will see "Processing Data", otherwise "Data Error" will be returned.
  
  # check data is there
  if(!is.null(df)){
    if(!is.null(nrow(df))){
      
      print("Processing Data")
      
      df <- df
      
      # detect where stops have changed using data.table:::uniquelist
      stopChange <- data.table:::uniqlist(df[,c("LAST_STOP_SHORT_DESC","CONTRACT_ROUTE","REGISTRATION_NUMBER")])
      
      # add detected column in
      df[stopChange,"STOP_CHANGE"] <- "Yes"
      df$STOP_CHANGE[is.na(df$STOP_CHANGE)] <- "No"
      
      # get rows where buses are stuck
      stuck <- grep("No", df$STOP_CHANGE)
      
      # create summary stats on the stuck buses
      dfStuck <- df[(stuck-1),] %>%
        group_by(LAST_STOP_SHORT_DESC, CONTRACT_ROUTE) %>%
        summarise(COUNT = length(CONTRACT_ROUTE))
      
      # remove stuck bus data
      df <- df[-(stuck-1),]
      
      # detect change in reg and route
      regRoute <- data.table:::uniqlist(df[,c("CONTRACT_ROUTE","REGISTRATION_NUMBER")])
      
      # create index for req.d cols
      colInd <- seq(1,nrow(df),1)[!(seq(1,nrow(df),1) %in% regRoute)]
      
      # re-align columns
      df[colInd,"SD2"] <- df[colInd-1,"SCHEDULE_DEVIATION"]
      
      # calc change (reverse as +ve deviation means ahead of schedule by x)
      df$SD_CHANGE <- df$SCHEDULE_DEVIATION - df$SD2
      df$SD_CHANGE[is.na(df$SD_CHANGE)] <- 0
      df$SD2 <- NULL
      
      # remove data over 2 hours old
      pd <- df[df$TIME_OF_DATA >= max(df$TIME_OF_DATA)-120*60,]
      
      # round to minutes and create observations column
      pd$AVG_DELAY <- pd$SD_CHANGE/60
      pd$S_DEV <- pd$SCHEDULE_DEVIATION/60
      pd$OBS <- 1
      
      # add weighting for TOD and calculate weighted delay
      pd$Weight <- exp(-(as.numeric(difftime(max(pd$TIME_OF_DATA), pd$TIME_OF_DATA, units = "mins"))/120))
      pd$AVG_DELAY_W <- as.numeric(pd$AVG_DELAY * pd$Weight)
      
      # select key columns and spread the time of data column into new column headers
      tmp <- pd %>%
        select(TIME_OF_DATA, AVG_DELAY, AVG_DELAY_W, S_DEV, LAST_STOP_SHORT_DESC, CONTRACT_ROUTE, OBS, Weight) %>%
        group_by(LAST_STOP_SHORT_DESC, CONTRACT_ROUTE) %>%
        summarise(weightSum = sum(Weight, na.rm = T),
                  weightDelaySum = sum(AVG_DELAY_W, na.rm = T),
                  obs = sum(OBS, na.rm = T),
                  last_update = max(TIME_OF_DATA, na.rm = T)
        )
      
      # calculate average delay
      tmp$avgDelay <- tmp$weightDelaySum / tmp$weightSum
      
      # join 'stuck' observations
      tmp <- left_join(tmp, dfStuck, by=c("LAST_STOP_SHORT_DESC"="LAST_STOP_SHORT_DESC","CONTRACT_ROUTE"="CONTRACT_ROUTE"))
      tmp$COUNT[is.na(tmp$COUNT)] <- 0
      tmp$obs <- tmp$obs + tmp$COUNT
      tmp$COUNT <- NULL
      
      # open bus sequence sample and join to calculated data
      bus.seq <- busSeq
      bus.seq$Route <- as.character(bus.seq$Route)
      bus.seq <- left_join(bus.seq, tmp, by=c("Route"="CONTRACT_ROUTE","Stop_Code_LBSL"="LAST_STOP_SHORT_DESC"))
      
      # order bus.seq by route, run and sequence
      bus.seq <- bus.seq[order(bus.seq$Route,bus.seq$Run,bus.seq$Sequence),]
      
      # detect change in reg and route
      runRoute <- data.table:::uniqlist(bus.seq[,c("Route","Run")])
      
      # replace first NA with 0 if it occurs in position 1
      bus.seq[runRoute[is.na(bus.seq[runRoute,"avgDelay"])],"avgDelay"] <- 0
      
      # replace last NA with 0 if it occurs at the end of the run
      runRoute <- runRoute-1
      runRoute <- runRoute[runRoute > 0]
      bus.seq[runRoute[is.na(bus.seq[runRoute,"avgDelay"])],"avgDelay"] <- 0
      
      # check last entry, set to 0 if na
      pos <- length(bus.seq$avgDelay)
      if(is.na(bus.seq$avgDelay[pos])){
        bus.seq$avgDelay[pos] <- 0
      }
      
      # interpolate NA values
      bus.seq$avgDelay <- zoo::na.approx(bus.seq$avgDelay)
      
      # create cumulative sum dummy column
      bus.seq$cumSum <- NA
      
      # order bus.seq by route, run and sequence
      bus.seq <- bus.seq[order(bus.seq$Route,bus.seq$Run,bus.seq$Sequence),]
      
      # loop through each route and run to calculate cumulative sum
      for(i in unique(bus.seq$Route)){
        for(j in unique(bus.seq$Run[bus.seq$Route == i])){
          
          # calc cumsum for each route/run combo
          bus.seq$cumSum[bus.seq$Route == i & bus.seq$Run == j] <- cumsum(bus.seq$avgDelay[bus.seq$Route == i & bus.seq$Run == j])
        }
      }
      
      # drop the log
      return(df = bus.seq)
      
    } else {
      print("Data Error")
    }
  } else {
    print("Data Error")
  }
}
