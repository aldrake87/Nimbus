libary(RJDBC)

getData <- function(timeInt = FALSE){
  ## timeInt can be TRUE or FALSE (default = FALSE). If FALSE, timeInt query builds a two hour long data frame. If TRUE, timeInt retrieves the most recent 5 minutes worth of data.
  
  # connect to the database
  conn <- dbConnect(drv, connstr)
  
  # get time of latest data entry
  MySQL <- "select max(time_of_data) from nimbus"
  df <- dbGetQuery(conn, MySQL)
  
  # check how much data is required
  if(!timeInt){
    # get previous 5 minutes of data
    MySQL <- paste0("select time_of_data, last_stop_short_desc, contract_route, trip_type, registration_number, schedule_deviation from nimbus where schedule_deviation > -1E8",
                    " and time_of_data >= '",as.POSIXct(df$max)-2*60*60,"'")
  } else {
    # get previous 2 hours of data
    MySQL <- paste0("select time_of_data, last_stop_short_desc, contract_route, trip_type, registration_number, schedule_deviation from nimbus where schedule_deviation > -1E8",
                    " and time_of_data >= '",as.POSIXct(df$max)-5*60,"'")
  }
  
  df <- dbGetQuery(conn, MySQL)
  dbDisconnect(conn) # disconnect
  
  # column name change
  names(df) <- c("TIME_OF_DATA","LAST_STOP_SHORT_DESC","CONTRACT_ROUTE","TRIP_TYPE","REGISTRATION_NUMBER","SCHEDULE_DEVIATION")
  
  # take sample of data and columns
  df$SCHEDULE_DEVIATION <- as.numeric(df$SCHEDULE_DEVIATION)
  
  # convert date to posixct
  df$TIME_OF_DATA <- as.POSIXct(df$TIME_OF_DATA, format="%Y-%m-%d %H:%M:%S.000000", tz="GMT")
  
  # limit to ~3 hours of data
  df <- df[!is.na(df$TIME_OF_DATA),]
  
  # order data
  df <- df[order(df$CONTRACT_ROUTE, df$REGISTRATION_NUMBER, df$TIME_OF_DATA),]

  # drop the log
  return(df = df)
}
