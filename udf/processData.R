library(tidyverse)
#library(zoo) #<--- you'll need to install this!
#library(data.table) #<--- you'll need to install this!

processData <- function(df = NULL){
  
  # check data is there
  if(!is.null(df)){
    if(!is.null(nrow(df))){
      
      print("Processing Data")
      
      df <- df
      
      df <- left_join(df, busSeq[,c("Route","Stop_Code_LBSL","Sequence","Run")], by=c("CONTRACT_ROUTE"="Route","LAST_STOP_SHORT_DESC"="Stop_Code_LBSL"))
      
      # detect change in reg and route
      regRoute <- data.table:::uniqlist(df[,c("CONTRACT_ROUTE","REGISTRATION_NUMBER")])
      
      # create index for req.d cols
      colInd <- seq(1,nrow(df),1)[!(seq(1,nrow(df),1) %in% regRoute)]
      
      # re-align columns
      df[colInd,"SD2"] <- df[colInd-1,"SCHEDULE_DEVIATION"]
      df[colInd,"TIME2"] <- df[colInd-1,"TIME_OF_DATA"]
      df[colInd,"prevStop"] <- df[colInd-1,"Sequence"]
      df[colInd,"prevRun"] <- df[colInd-1,"Run"]
      df$TIME2 <- as.POSIXct.numeric(df$TIME2, origin = "1970-01-01", tz="GMT")
      
      # drop entries without previous run or where run has changed
      df <- df[!is.na(df$prevRun),]
      df <- df[df$Run == df$prevRun,]
      df$prevRun <- NULL
      
      # calc change (reverse as +ve deviation means ahead of schedule by x)
      df$SD_CHANGE <- df$SCHEDULE_DEVIATION - df$SD2
      df$SD2 <- NULL
      
      # calc time difference between records (for speed calcs)
      df$timeDiff <- difftime(df$TIME_OF_DATA, df$TIME2, units="mins")
      df$TIME2 <- NULL
      
      # calc number of stops between records
      df$prevStop[is.na(df$prevStop)] <- df$Sequence[is.na(df$prevStop)]
      df$noStops <- (df$Sequence - df$prevStop) + 1
      
      # calculate the step change and remove dodgy data
      df$stepChange <- df$SD_CHANGE / df$noStops
      df$stepChange[df$noStops <= 1] <- NA
      df$stepChange[df$noStops > mean(df$noStops[df$noStops >= 0], na.rm=T)+3*sd(df$noStops[df$noStops >= 0], na.rm=T)] <- NA
      df <- df[!is.na(df$stepChange),]
      
      # remove data where bus has dropped further behind than expected
      df$SD_CHANGE <- df$SD_CHANGE / 60
      df <- df[abs(df$SD_CHANGE) < df$timeDiff,]
      
      # create data frame with filled gaps
      pd <- data.frame(timeOfData = rep(df$TIME_OF_DATA, df$Sequence - df$prevStop + 1),
                       route = rep(df$CONTRACT_ROUTE, df$Sequence - df$prevStop + 1),
                       reg = rep(df$REGISTRATION_NUMBER, df$Sequence - df$prevStop + 1),
                       run = rep(df$Run, df$Sequence - df$prevStop + 1),
                       stepChange = rep(df$stepChange, df$Sequence - df$prevStop + 1),
                       sequence = unlist(mapply(seq, df$prevStop, df$Sequence)))
      
      # round to minutes
      pd$avgDelay <- pd$stepChange / 60
      
      # add weighting for TOD and calculate weighted delay
      pd$weight <- exp(-(as.numeric(difftime(max(pd$timeOfData),pd$timeOfData, units = "mins")) / 15))
      pd$avgDelayW <- as.numeric(pd$avgDelay * pd$weight)
      
      # group by unique route/run/sequence to calculate vals
      tmp <- pd %>%
        group_by(route, run, sequence) %>%
        summarise(weightSum = sum(weight, na.rm = T),
                  weightDelaySum = sum(avgDelayW, na.rm = T),
                  lastUpdate = max(timeOfData, na.rm = T))
      
      # calculate average delay
      tmp$avgDelay <- tmp$weightDelaySum / tmp$weightSum
      
      # join bus sequence sample to calculated data
      bus.seq <- left_join(busSeq, tmp, 
                           by=c("Route"="route",
                                "Run" = "run",
                                "Sequence" = "sequence"))
      
      # order bus.seq by route, run and sequence
      bus.seq <- bus.seq[order(bus.seq$Route,bus.seq$Run,bus.seq$Sequence),]
      
      bus.seq$avgDelay[is.na(bus.seq$avgDelay)] <- 0
      
      # create cumulative sum dummy column
      bus.seq$cumSum <- NA
      
      # order bus.seq by route, run and sequence
      bus.seq <- bus.seq[order(bus.seq$Route,bus.seq$Run,bus.seq$Sequence),]
      
      #start <- Sys.time()
      for(i in unique(bus.seq$Route)){
        for(j in unique(bus.seq$Run[bus.seq$Route == i])){
          
          # calc cumsum for each route/run combo
          bus.seq$cumSum[bus.seq$Route == i & bus.seq$Run == j] <- cumsum(bus.seq$avgDelay[bus.seq$Route == i & bus.seq$Run == j])
        }
      }
      
      names(bus.seq) <- c("route","run","sequence","stopCodeLBSL","busStopCode","naptanAtco",
                          "stopName","easting","northing","heading","virtualBusStop","fromStop",
                          "toStop","direction","avgDist","weightSum","avgWeight","weightDelaySum",
                          "lastUpdate","avgDelay","cumSum")
      
      # drop the log
      return(df = bus.seq)
      
    } else {
      print("Data Error")
    }
  } else {
    print("Data Error")
  }
}
