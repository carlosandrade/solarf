library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session,clientData) {
  
  #### Extracts all global variables ####
  
  #get path
  actualPath = reactive({paste("data/mesowest/",input$station,"/actual.csv",sep="")})
  predPath =  reactive({paste("data/mesowest/",input$station,"/probm1.csv",sep="")})
  ln2f1Path = reactive({paste("data/mesowest/",input$station,"/",input$station,"-f1.csv",sep="")})
  ln2f2Path = reactive({paste("data/mesowest/",input$station,"/",input$station,"-f2.csv",sep="")})
  ln2f3Path = reactive({paste("data/mesowest/",input$station,"/",input$station,"-f3.csv",sep="")})
  knnPath = reactive({paste("data/mesowest/",input$station,"/",input$station,"-knn.csv",sep="")})
  dhPath = reactive({paste("data/mesowest/",input$station,"/",input$station,"-f1-DES.csv",sep="")})
  
  
  #read the input files
  actual <- reactive({read.csv(actualPath())})  
  pred   <- reactive({read.csv(predPath())})
  ln2f1  <- reactive({read.csv(ln2f1Path())})
  ln2f2  <- reactive({read.csv(ln2f2Path())})
  ln2f3  <- reactive({read.csv(ln2f3Path())})
  knn    <- reactive({read.csv(knnPath())})
  dhline <- reactive({read.csv(dhPath())})
  
  #get the actual day
  day   = reactive({as(substr(input$date,9,10),"numeric")})
  month = reactive({as(substr(input$date,6,7),"numeric")})
  year  = reactive({as(substr(input$date,1,4),"numeric")})
  
  #get the last GFS image date converted to UTC
  
  checkdate <- reactive({
    if(Sys.Date() == input$date)
    {
    
      currentTime = as.POSIXlt(Sys.time(),tz="UTC")
      
      dateRange = c(paste(substr(currentTime,1,10)," ","00:00",sep="")
                    ,paste(substr(currentTime,1,10)," ","06:00",sep="")
                    ,paste(substr(currentTime,1,10)," ","12:00",sep="")
                    ,paste(substr(currentTime,1,10)," ","18:00",sep=""))
      
      dateRange = as.POSIXlt(dateRange,tz="UTC")
      
      dateUTC = dateRange[findInterval(currentTime, dateRange)]
      
    }else {
      
      dateUTC = as.POSIXct(paste(input$date," ","18:00",sep=""),tz="UTC")
      dateUTC = dateUTC + 64800
    }
    
    dateIma1 = dateUTC - 64800
    dateIma2 = dateUTC - 43200
    dateIma3 = dateUTC - 21600
    dateIma4 = dateUTC
    
    ## When the hour is 00:00:00 it doesn't show up in the variable
    ## ig. "2014-01-05 UTC" in this case it needs to be checked and
    ## has to be assigned to a variabel hour as a character "00"
    if(!is.na(as.numeric(substr(dateIma1,12,13)))) 
    {
      hourIma1 = substr(dateIma1,12,13)
    } else { hourIma1 = "00" }
    
    if(!is.na(as.numeric(substr(dateIma2,12,13)))) 
    {
      hourIma2 = substr(dateIma2,12,13)
    } else { hourIma2 = "00" }
    
    if(!is.na(as.numeric(substr(dateIma3,12,13)))) 
    {
      hourIma3 = substr(dateIma3,12,13)
    } else { hourIma3 = "00" }
    
    if(!is.na(as.numeric(substr(dateIma4,12,13)))) 
    {
      hourIma4 = substr(dateIma4,12,13)
    } else { hourIma4 = "00" }
    
    ## return ##
    list(dateUTC=dateUTC,dateIma1=dateIma1,dateIma2=dateIma2,dateIma3=dateIma3,dateIma4=dateIma4
         ,hourIma1=hourIma1,hourIma2=hourIma2,hourIma3=hourIma3,hourIma4=hourIma4)
  })
  
  
  #get range in the input
  bottom = reactive({input$range[1]})
  top    = reactive({input$range[2]})
  
  # plot the actual day
  output$actualPlot <- renderPlot({
    
    #transform all global variables in local variables
    actual = actual()
    pred   = pred()
    ln2f1  = ln2f1()
    ln2f2  = ln2f2()
    ln2f3  = ln2f3()
    knn    = knn()
    dhline = dhline()
    bottom = bottom()
    top    = top()
    day    = day()
    month  = month()
    year   = year()
    
    #removing NA values
    #pred[is.na(pred$SOLR),] <- 1
    
    #Define the graph range
    actualSolr = actual$SOLR[which(actual$HR > bottom & actual$HR < top
                              & actual$YEAR == year & actual$MON == month & actual$DAY == day)]
    
    hours = actual$HR[which(actual$HR > bottom & actual$HR < top
                            & actual$YEAR == year & actual$MON == month & actual$DAY == day)]
    
    predSolr = na.exclude(pred$SOLR[which(pred$HR > bottom & pred$HR < top
                                      & pred$YEAR == year & pred$MON == month & pred$DAY == day)])
    
    ln2f1Solr = na.exclude(ln2f1$SOLR[which(ln2f1$HR > bottom & ln2f1$HR < top
                                  & ln2f1$YEAR == year & ln2f1$MON == month & ln2f1$DAY == day)])

    ln2f2Solr = na.exclude(ln2f2$SOLR[which(ln2f2$HR > bottom & ln2f2$HR < top
                                            & ln2f2$YEAR == year & ln2f2$MON == month & ln2f2$DAY == day)])
    
    ln2f3Solr = na.exclude(ln2f3$SOLR[which(ln2f3$HR > bottom & ln2f3$HR < top
                                            & ln2f3$YEAR == year & ln2f3$MON == month & ln2f3$DAY == day)])
    
    knnSolr = na.exclude(knn$SOLR[which(knn$HR > bottom & knn$HR < top
                                            & knn$YEAR == year & knn$MON == month & knn$DAY == day)])
    
    dhSolr = na.exclude(dhline$SOLR[which(dhline$HR > bottom & dhline$HR < top
                                        & dhline$YEAR == year & dhline$MON == month & dhline$DAY == day)])
    
    # main plot
    plot(main= c("Daily Solar Radiation Profile",paste(day,"/",month,"/",year,sep="")),hours,actualSolr,
          type=input$var,xlab="Hour",ylab="Solar Radiation (W/m^2)",col="red",xaxt="n",ylim=c(0, 1200))
    
    axis(1, xaxp=c(8, 17, 9), las=1)
    
    #Add 1 Day Before Probability Model
    if(length(predSolr)>0 & input$cb1day) {
      lines(hours,predSolr,col="chartreuse4") }
    
    #Add 2 Hour Before Linear Regression Model
    if(length(ln2f1Solr)>0 & input$cb2h) {
      lines(hours,ln2f1Solr,col="blue")}
    
    #Add 2 Hour Before Linear Regression Model
    if(length(ln2f2Solr)>0 & input$cb2f2h) {
      lines(hours,ln2f2Solr,col="orange")}
    #Add 2 Hour Before Linear Regression Model
    if(length(ln2f3Solr)>0 & input$cb2f3h) {
      lines(hours,ln2f3Solr,col="gray")}
    #Add knn
    if(length(knnSolr)>0 & input$cbknn) {
      lines(hours,knnSolr,col="Violet")}
    #Add Deshourlymean
    if(length(dhSolr)>0 & input$dhline) {
      lines(hours,dhSolr,col="gold")}
    
  },height = 425, width = 470  )
  
  #plot the day before
  output$yesPlot <- renderPlot({
    
    #transform all global variables in local variables
    actual = actual()
    pred   = pred()
    ln2f1  = ln2f1()
    ln2f2  = ln2f2()
    ln2f3  = ln2f3()
    knn    = knn()
    dhline = dhline()
    bottom = bottom()
    top    = top()
    
    yestarday = as.character(as.Date(input$date) - 1)
    year  = as(substr(yestarday,1,4),"numeric")
    month = as(substr(yestarday,6,7),"numeric")
    yestarday = as(substr(yestarday,9,10),"numeric")
    
    #Define the graph range
    yesSolr = actual$SOLR[which(actual$HR > bottom & actual$HR < top
                                   & actual$YEAR == year & actual$MON == month & actual$DAY == yestarday)]
    
    hours = actual$HR[which(actual$HR > bottom & actual$HR < top
                            & actual$YEAR == year & actual$MON == month & actual$DAY == yestarday)]
    
    yesPredSolr = na.exclude(pred$SOLR[which(pred$HR > bottom & pred$HR < top
                                          & pred$YEAR == year & pred$MON == month & pred$DAY == yestarday)])
    
    yesln2f1Solr = na.exclude(ln2f1$SOLR[which(ln2f1$HR > bottom & ln2f1$HR < top
                                            & ln2f1$YEAR == year & ln2f1$MON == month & ln2f1$DAY == yestarday)])
    
    yesln2f2Solr = na.exclude(ln2f2$SOLR[which(ln2f2$HR > bottom & ln2f2$HR < top
                                            & ln2f2$YEAR == year & ln2f2$MON == month & ln2f2$DAY == yestarday)])
    
    yesln2f3Solr = na.exclude(ln2f3$SOLR[which(ln2f3$HR > bottom & ln2f3$HR < top
                                            & ln2f3$YEAR == year & ln2f3$MON == month & ln2f3$DAY == yestarday)])
    
    yesknnSolr = na.exclude(knn$SOLR[which(knn$HR > bottom & knn$HR < top
                                        & knn$YEAR == year & knn$MON == month & knn$DAY == yestarday)])
    
    yesdhSolr = na.exclude(dhline$SOLR[which(dhline$HR > bottom & dhline$HR < top
                                          & dhline$YEAR == year & dhline$MON == month & dhline$DAY == yestarday)])
    
    # main plot
    plot(main="-1 Day",hours,yesSolr,
         type=input$var,xlab="",ylab="",col="red",xaxt="n",ylim=c(0, 1200))
    
    axis(1, xaxp=c(8, 17, 9), las=1)
    
    # Add 1 Day Before Probability Model
    if(length(yesPredSolr)>0 & input$cb1day) {
      lines(hours,yesPredSolr,col="chartreuse4") }
    
    #Add 2 Hour Before Linear Regression Model
    if(length(yesln2f1Solr)>0 & input$cb2h) {
      lines(hours,yesln2f1Solr,col="blue")}
    
    #Add 2 Hour Before Linear Regression Model
    if(length(yesln2f2Solr)>0 & input$cb2f2h) {
      lines(hours,yesln2f2Solr,col="orange")}
    #Add 2 Hour Before Linear Regression Model
    if(length(yesln2f3Solr)>0 & input$cb2f3h) {
      lines(hours,yesln2f3Solr,col="gray")}
    #Add knn
    if(length(yesknnSolr)>0 & input$cbknn) {
      lines(hours,yesknnSolr,col="Violet")}
    #Add Deshourlymean
    if(length(yesdhSolr)>0 & input$dhline) {
      lines(hours,yesdhSolr,col="gold")}
    
  },height = 250, width = 250 )
  
  #plot 2 day before
  output$befYesPlot <- renderPlot({
    
    #transform all global variables in local variables
    actual = actual()
    pred   = pred()
    ln2f1  = ln2f1()
    ln2f2  = ln2f2()
    ln2f3  = ln2f3()
    knn    = knn()
    dhline = dhline()
    bottom = bottom()
    top    = top()

    beforeYestarday = as.character(as.Date(input$date) - 2)
    year  = as(substr(beforeYestarday,1,4),"numeric")
    month = as(substr(beforeYestarday,6,7),"numeric")
    beforeYestarday = as(substr(beforeYestarday,9,10),"numeric")
    
    #Define the graph range
    befYesSolr = actual$SOLR[which(actual$HR > bottom & actual$HR < top
                                & actual$YEAR == year & actual$MON == month & actual$DAY == beforeYestarday)]
    
    hours = actual$HR[which(actual$HR > bottom & actual$HR < top
                            & actual$YEAR == year & actual$MON == month & actual$DAY == beforeYestarday)]
    
    befYesPredSolr = na.exclude(pred$SOLR[which(pred$HR > bottom & pred$HR < top
                                             & pred$YEAR == year & pred$MON == month & pred$DAY == beforeYestarday)])
    
    befYesln2f1Solr = na.exclude(ln2f1$SOLR[which(ln2f1$HR > bottom & ln2f1$HR < top
                                            & ln2f1$YEAR == year & ln2f1$MON == month & ln2f1$DAY == beforeYestarday)])
    
    befYesln2f2Solr = na.exclude(ln2f2$SOLR[which(ln2f2$HR > bottom & ln2f2$HR < top
                                            & ln2f2$YEAR == year & ln2f2$MON == month & ln2f2$DAY == beforeYestarday)])
    
    befYesln2f3Solr = na.exclude(ln2f3$SOLR[which(ln2f3$HR > bottom & ln2f3$HR < top
                                            & ln2f3$YEAR == year & ln2f3$MON == month & ln2f3$DAY == beforeYestarday)])
    
    befYesknnSolr = na.exclude(knn$SOLR[which(knn$HR > bottom & knn$HR < top
                                        & knn$YEAR == year & knn$MON == month & knn$DAY == beforeYestarday)])
    
    befYesdhSolr = na.exclude(dhline$SOLR[which(dhline$HR > bottom & dhline$HR < top
                                          & dhline$YEAR == year & dhline$MON == month & dhline$DAY == beforeYestarday)])
    
    # main plot
    plot(main="-2 Day",hours,befYesSolr,
         type=input$var,xlab="",ylab="",col="red",xaxt="n",ylim=c(0, 1200))
    
    axis(1, xaxp=c(8, 17, 9), las=1)
    
    # Add 1 Day Before Probability Model
    if(length(befYesPredSolr)>0 & input$cb1day) {
      lines(hours,befYesPredSolr,col="chartreuse4") }
    
    #Add 2 Hour Before Linear Regression Model
    if(length(befYesln2f1Solr)>0 & input$cb2h) {
      lines(hours,befYesln2f1Solr,col="blue")}
    
    #Add 2 Hour Before Linear Regression Model
    if(length(befYesln2f2Solr)>0 & input$cb2f2h) {
      lines(hours,befYesln2f2Solr,col="orange")}
    #Add 2 Hour Before Linear Regression Model
    if(length(befYesln2f3Solr)>0 & input$cb2f3h) {
      lines(hours,befYesln2f3Solr,col="gray")}
    #Add knn
    if(length(befYesknnSolr)>0 & input$cbknn) {
      lines(hours,befYesknnSolr,col="Violet")}
    #Add Deshourlymean
    if(length(befYesdhSolr)>0 & input$dhline) {
      lines(hours,befYesdhSolr,col="gold")}
    
  },height = 250, width = 250)  

  #plot day after
  output$tomPlot <- renderPlot({
    
    #transform all global variables in local variables
    actual = actual()
    pred   = pred()
    ln2f1  = ln2f1()
    ln2f2  = ln2f2()
    ln2f3  = ln2f3()
    knn    = knn()
    dhline = dhline()
    bottom = bottom()
    top    = top()

    tomorrow = as.character(as.Date(input$date) + 1)
    year  = as(substr(tomorrow,1,4),"numeric")
    month = as(substr(tomorrow,6,7),"numeric")
    tomorrow = as(substr(tomorrow,9,10),"numeric")    
    
    #Define the graph range
    tomSolr = actual$SOLR[which(actual$HR > bottom & actual$HR < top
                                   & actual$YEAR == year & actual$MON == month & actual$DAY == tomorrow)]
    
    hours = actual$HR[which(actual$HR > bottom & actual$HR < top
                            & actual$YEAR == year & actual$MON == month & actual$DAY == tomorrow)]
    
    tomPredSolr = na.exclude(pred$SOLR[which(pred$HR > bottom & pred$HR < top
                                                & pred$YEAR == year & pred$MON == month & pred$DAY == tomorrow)])
    
    tomln2f1Solr = na.exclude(ln2f1$SOLR[which(ln2f1$HR > bottom & ln2f1$HR < top
                                            & ln2f1$YEAR == year & ln2f1$MON == month & ln2f1$DAY == tomorrow)])
        
    tomln2f2Solr = na.exclude(ln2f2$SOLR[which(ln2f2$HR > bottom & ln2f2$HR < top
                                                  & ln2f2$YEAR == year & ln2f2$MON == month & ln2f2$DAY == tomorrow)])
    
    tomln2f3Solr = na.exclude(ln2f3$SOLR[which(ln2f3$HR > bottom & ln2f3$HR < top
                                                  & ln2f3$YEAR == year & ln2f3$MON == month & ln2f3$DAY == tomorrow)])
    
    tomknnSolr = na.exclude(knn$SOLR[which(knn$HR > bottom & knn$HR < top
                                              & knn$YEAR == year & knn$MON == month & knn$DAY == tomorrow)])
    
    tomdhSolr = na.exclude(dhline$SOLR[which(dhline$HR > bottom & dhline$HR < top
                                          & dhline$YEAR == year & dhline$MON == month & dhline$DAY == tomorrow)])
    
    # main plot
    plot(main="+1 Day",hours,tomSolr,
         type=input$var,xlab="",ylab="",col="red",xaxt="n",ylim=c(0, 1200))
    
    axis(1, xaxp=c(8, 17, 9), las=1)
    
    # Add 1 Day Before Probability Model
    if(length(tomPredSolr)>0 & input$cb1day) {
      lines(hours,tomPredSolr,col="chartreuse4") }
    
    #Add 2 Hour Before Linear Regression Model
    if(length(tomln2f1Solr)>0 & input$cb2h) {
      lines(hours,tomln2f1Solr,col="blue")}
    
    #Add 2 Hour Before Linear Regression Model
    if(length(tomln2f2Solr)>0 & input$cb2f2h) {
      lines(hours,tomln2f2Solr,col="orange")}
    #Add 2 Hour Before Linear Regression Model
    if(length(tomln2f3Solr)>0 & input$cb2f3h) {
      lines(hours,tomln2f3Solr,col="gray")}
    #Add knn
    if(length(tomknnSolr)>0 & input$cbknn) {
      lines(hours,tomknnSolr,col="Violet")}
    #Add Deshourlymean
    if(length(tomdhSolr)>0 & input$dhline) {
      lines(hours,tomdhSolr,col="gold")}
    
  },height = 250, width = 250)   

  #plot 2 day after
  output$aftTomPlot <- renderPlot({
    
    #transform all global variables in local variables
    actual = actual()
    pred   = pred()
    ln2f1  = ln2f1()
    ln2f2  = ln2f2()
    ln2f3  = ln2f3()
    knn    = knn()
    dhline = dhline()
    bottom = bottom()
    top    = top()
    
    afterTom = as.character(as.Date(input$date) + 2)
    year  = as(substr(afterTom,1,4),"numeric")
    month = as(substr(afterTom,6,7),"numeric")
    afterTom = as(substr(afterTom,9,10),"numeric")
    
    #Define the graph range
    aftTomSolr = actual$SOLR[which(actual$HR > bottom & actual$HR < top
                                & actual$YEAR == year & actual$MON == month & actual$DAY == afterTom)]
    
    hours = actual$HR[which(actual$HR > bottom & actual$HR < top
                            & actual$YEAR == year & actual$MON == month & actual$DAY == afterTom)]
    
    aftTomPredSolr = na.exclude(pred$SOLR[which(pred$HR > bottom & pred$HR < top
                                             & pred$YEAR == year & pred$MON == month & pred$DAY == afterTom)])
    
    aftTomln2f1Solr = na.exclude(ln2f1$SOLR[which(ln2f1$HR > bottom & ln2f1$HR < top
                                         & ln2f1$YEAR == year & ln2f1$MON == month & ln2f1$DAY == afterTom)])
    
    aftTomln2f2Solr = na.exclude(ln2f2$SOLR[which(ln2f2$HR > bottom & ln2f2$HR < top
                                               & ln2f2$YEAR == year & ln2f2$MON == month & ln2f2$DAY == afterTom)])
    
    aftTomln2f3Solr = na.exclude(ln2f3$SOLR[which(ln2f3$HR > bottom & ln2f3$HR < top
                                               & ln2f3$YEAR == year & ln2f3$MON == month & ln2f3$DAY == afterTom)])
    
    aftTomknnSolr = na.exclude(knn$SOLR[which(knn$HR > bottom & knn$HR < top
                                           & knn$YEAR == year & knn$MON == month & knn$DAY == afterTom)])
    
    aftTomdhSolr = na.exclude(dhline$SOLR[which(dhline$HR > bottom & dhline$HR < top
                                          & dhline$YEAR == year & dhline$MON == month & dhline$DAY == afterTom)])
    
    # main plot
    plot(main="+2 Day",hours,aftTomSolr,
         type=input$var,xlab="",ylab="",col="red",xaxt="n",ylim=c(0, 1200))
    
    axis(1, xaxp=c(8, 17, 9), las=1)
    
    # Add 1 Day Before Probability Model
    if(length(aftTomPredSolr)>0 & input$cb1day) {
      lines(hours,aftTomPredSolr,col="chartreuse4") }
    
    #Add 2 Hour Before Linear Regression Model
    if(length(aftTomln2f1Solr)>0 & input$cb2h) {
      lines(hours,aftTomln2f1Solr,col="blue")}
    
    #Add 2 Hour Before Linear Regression Model
    if(length(aftTomln2f2Solr)>0 & input$cb2f2h) {
      lines(hours,aftTomln2f2Solr,col="orange")}
    #Add 2 Hour Before Linear Regression Model
    if(length(aftTomln2f3Solr)>0 & input$cb2f3h) {
      lines(hours,aftTomln2f3Solr,col="gray")}
    #Add knn
    if(length(aftTomknnSolr)>0 & input$cbknn) {
      lines(hours,aftTomknnSolr,col="Violet")}
    #Add Deshourlymean
    if(length(aftTomdhSolr)>0 & input$dhline) {
      lines(hours,aftTomdhSolr,col="gold")}
    
  },height = 250, width = 250)  
  
  ################imagens#########################
  
  #### VVEL ####
  output$vvELImage00 <- renderImage({

    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma1,1,4)),"/",as.character(substr(checkdate()$dateIma1,6,7)),"/VVEL_850mb/",
                                        paste("VVEL_850mb_",as.character(substr(checkdate()$dateIma1,1,4)),as.character(substr(checkdate()$dateIma1,6,7)),
                                              as.character(substr(checkdate()$dateIma1,9,10)),"_",checkdate()$hourIma1,"00_000.png",sep="")))
    
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$vvELImage06 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma2,1,4)),"/",as.character(substr(checkdate()$dateIma2,6,7)),"/VVEL_850mb/",
                                        paste("VVEL_850mb_",as.character(substr(checkdate()$dateIma2,1,4)),as.character(substr(checkdate()$dateIma2,6,7)),
                                              as.character(substr(checkdate()$dateIma2,9,10)),"_",checkdate()$hourIma2,"00_000.png",sep="")))
    
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$vvELImage12 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma3,1,4)),"/",as.character(substr(checkdate()$dateIma3,6,7)),"/VVEL_850mb/",
                                        paste("VVEL_850mb_",as.character(substr(checkdate()$dateIma3,1,4)),as.character(substr(checkdate()$dateIma3,6,7)),
                                              as.character(substr(checkdate()$dateIma3,9,10)),"_",checkdate()$hourIma3,"00_000.png",sep="")))
    
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$vvELImage18 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma4,1,4)),"/",as.character(substr(checkdate()$dateIma4,6,7)),"/VVEL_850mb/",
                                        paste("VVEL_850mb_",as.character(substr(checkdate()$dateIma4,1,4)),as.character(substr(checkdate()$dateIma4,6,7)),
                                              as.character(substr(checkdate()$dateIma4,9,10)),"_",checkdate()$hourIma4,"00_000.png",sep="")))
    
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  #### WIND ####
  output$windImage00 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma1,1,4)),"/",as.character(substr(checkdate()$dateIma1,6,7)),"/Wind_850mb/",
                                         paste("Wind_850mb_",as.character(substr(checkdate()$dateIma1,1,4)),as.character(substr(checkdate()$dateIma1,6,7)),
                                               as.character(substr(checkdate()$dateIma1,9,10)),"_",checkdate()$hourIma1,"00_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$windImage06 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma2,1,4)),"/",as.character(substr(checkdate()$dateIma2,6,7)),"/Wind_850mb/",
                                         paste("Wind_850mb_",as.character(substr(checkdate()$dateIma2,1,4)),as.character(substr(checkdate()$dateIma2,6,7)),
                                               as.character(substr(checkdate()$dateIma2,9,10)),"_",checkdate()$hourIma2,"00_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$windImage12 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma3,1,4)),"/",as.character(substr(checkdate()$dateIma3,6,7)),"/Wind_850mb/",
                                         paste("Wind_850mb_",as.character(substr(checkdate()$dateIma3,1,4)),as.character(substr(checkdate()$dateIma3,6,7)),
                                               as.character(substr(checkdate()$dateIma3,9,10)),"_",checkdate()$hourIma3,"00_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$windImage18 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma4,1,4)),"/",as.character(substr(checkdate()$dateIma4,6,7)),"/Wind_850mb/",
                                         paste("Wind_850mb_",as.character(substr(checkdate()$dateIma4,1,4)),as.character(substr(checkdate()$dateIma4,6,7)),
                                               as.character(substr(checkdate()$dateIma4,9,10)),"_",checkdate()$hourIma4,"00_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  
  #### Pwat ####
  output$pwatImage00 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma1,1,4)),"/",as.character(substr(checkdate()$dateIma1,6,7)),"/PWAT_atmoscol/",
                                         paste("PWAT_atmoscol_",as.character(substr(checkdate()$dateIma1,1,4)),as.character(substr(checkdate()$dateIma1,6,7)),
                                               as.character(substr(checkdate()$dateIma1,9,10)),"_",checkdate()$hourIma1,"00_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$pwatImage06 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma2,1,4)),"/",as.character(substr(checkdate()$dateIma2,6,7)),"/PWAT_atmoscol/",
                                         paste("PWAT_atmoscol_",as.character(substr(checkdate()$dateIma2,1,4)),as.character(substr(checkdate()$dateIma2,6,7)),
                                               as.character(substr(checkdate()$dateIma2,9,10)),"_",checkdate()$hourIma2,"00_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$pwatImage12 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma3,1,4)),"/",as.character(substr(checkdate()$dateIma3,6,7)),"/PWAT_atmoscol/",
                                         paste("PWAT_atmoscol_",as.character(substr(checkdate()$dateIma3,1,4)),as.character(substr(checkdate()$dateIma3,6,7)),
                                               as.character(substr(checkdate()$dateIma3,9,10)),"_",checkdate()$hourIma3,"00_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$pwatImage18 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma4,1,4)),"/",as.character(substr(checkdate()$dateIma4,6,7)),"/PWAT_atmoscol/",
                                         paste("PWAT_atmoscol_",as.character(substr(checkdate()$dateIma4,1,4)),as.character(substr(checkdate()$dateIma4,6,7)),
                                               as.character(substr(checkdate()$dateIma4,9,10)),"_",checkdate()$hourIma4,"00_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  ### GHI IMAGES ####
  
  output$ghiImage00 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma1,1,4)),"/",as.character(substr(checkdate()$dateIma1,6,7)),"/GHI/",
                                         paste("GHI_",as.character(substr(checkdate()$dateIma1,1,4)),as.character(substr(checkdate()$dateIma1,6,7)),
                                               as.character(substr(checkdate()$dateIma1,9,10)),"_1800_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  output$ghiImage18 <- renderImage({
    
    filename <- normalizePath(file.path('./www/gfs/',as.character(substr(checkdate()$dateIma4,1,4)),"/",as.character(substr(checkdate()$dateIma4,6,7)),"/GHI/",
                                         paste("GHI_",as.character(substr(checkdate()$dateIma4,1,4)),as.character(substr(checkdate()$dateIma4,6,7)),
                                               as.character(substr(checkdate()$dateIma4,9,10)),"_0000_000.png",sep="")))
    # Return a list containing the filename
    list(src = filename,
         height = 275,
         weidth = 275,
         alt = "Image number")
    
  }, deleteFile = FALSE)
  
  
})
