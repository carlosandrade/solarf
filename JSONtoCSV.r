library(RJSONIO)
options(warn = -1)

stations = c('SCBH1','KKRH1','KTAH1','PLHH1','KFWH1','MKRH1','SCEH1','C0875','SCSH1','WNVH1')

for (i in 1:length(stations))
{
  
  stationName = stations[i]
  jsonPath = paste("/home2/dieison/csv/",stationName,".json",sep="")
  
  dataRaw <- fromJSON(jsonPath)
  data = dataRaw[['STATION']][[1]][['OBSERVATIONS']]
  
  ## Get all variables to build the data frame
  YEAR = as.numeric(substr(data[['date_time']],1,4))
  MON  = as.numeric(substr(data[['date_time']],6,7))
  DAY  = as.numeric(substr(data[['date_time']],9,10))
  HR   = as.numeric(substr(data[['date_time']],12,13))
  
  SOLR = as.character(data[['solar_radiation_set_1']])
  SOLR = as.numeric(SOLR) 
  
  ## If added to fix the bug in the Json api
  if(length(SOLR) > length(YEAR)) 
  {
    length(SOLR) = length(YEAR)
  }
  
  ## Create data frame to get selected data
  df = data.frame(YEAR,MON,DAY,HR,SOLR)
  
  ## List to check the hours of interest
  hours = c('8','9','10','11','12','13','14','15','16','17')

  ## Select data between 8 and 17 hours and just the first min of observation
  YEAR = df$YEAR[which(is.element(df$HR,hours))]
  MON  = df$MON[which(is.element(df$HR,hours))]
  DAY  = df$DAY[which(is.element(df$HR,hours))]
  SOLR = df$SOLR[which(is.element(df$HR,hours))]
  HR   = df$HR[which(is.element(df$HR,hours))]
  
  ## cauculate the solar radiation avg per hour ##
  
  ## Get first hour,month and year to test 
  getMon  = MON[1]
  getDay  = DAY[1]
  getHr   = HR[1]
  getYear = YEAR[1]
  
  ## counter and accumulator to calculate avg
  count = 0
  solrAcc = 0
  ## temporary vectors to register the hours with avg
  tmpHr   = numeric()
  tmpSolr = numeric()
  tmpDay  = numeric()
  tmpMon  = numeric()
  tmpYear = numeric()

  for (j in 1:length(SOLR))
  {
    
    if(HR[j]==getHr)
    {
       if(is.numeric(SOLR[j]))
       {
         solrAcc = solrAcc + SOLR[j]
         count = count + 1
       }
    } 
    else if (HR[j] != getHr)
    {
      tmpHr   = append(tmpHr,getHr)
      avg     = solrAcc / count
      tmpSolr = append(tmpSolr,avg)   
      solrAcc = SOLR[j]
      count = 1
      getHr = HR[j]
      
      if (DAY[j] == getDay)
      {
        tmpDay  = append(tmpDay,DAY[j])
      } else {
        tmpDay  = append(tmpDay,DAY[j-1])
        getDay = DAY[j]
      }
      
      if (MON[j] == getMon)
      {
        tmpMon  = append(tmpMon,MON[j])
      } else {
        tmpMon  = append(tmpMon,MON[j-1])
        getMon = MON[j]
      }
      
      if (YEAR[j] == getYear)
      {
        tmpYear = append(tmpYear,YEAR[j])
      } else {
        tmpYear = append(tmpYear,YEAR[j-1])
        getYear = YEAR[j]
      }
    }
    #check last position
    if (j == length(SOLR))
    {
      tmpHr   = append(tmpHr,getHr)
      avg     = solrAcc / count
      tmpSolr = append(tmpSolr,avg)
      tmpDay  = append(tmpDay,DAY[j])
      tmpMon  = append(tmpMon,MON[j])
      tmpYear = append(tmpYear,YEAR[j])
    }    
  }

  ## Create final Data frame
  YEAR = tmpYear
  MON  = tmpMon
  DAY  = tmpDay
  SOLR = tmpSolr
  HR   = as.character(tmpHr)

  df = data.frame(YEAR,MON,DAY,HR,SOLR)
  
  ## Create CSV file
  fileName = paste("/home2/dieison/csv/",stationName,".csv",sep="")
  
  write.csv(df, file = fileName ,row.names=FALSE)
  
  #Remove all variables
  rm(YEAR,MON,DAY,SOLR,HR,MIN,df,fileName,hours,data,dataRaw,stationName,jsonPath,avg,count,getDay,getHr,getMon,getYear,j,solrAcc,tmpDay,tmpHr,tmpMon,tmpSolr,tmpYear)
}
