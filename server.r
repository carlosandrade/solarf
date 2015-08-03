library(shiny)
library(googleVis)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session,clientData) {
  
  #### Extracts all global variables ####

  #get the file path
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
  
  datasetInput <- reactive({
   
    actual = actual()
    pred   = pred()
    ln2f1  = ln2f1()
    ln2f2  = ln2f2()
    ln2f3  = ln2f3()
    knn    = knn()
    dhline = dhline()

    YEAR <- actual$YEAR[which(actual$HR > 7 & actual$HR < 18)]
    MON <- actual$MON[which(actual$HR > 7 & actual$HR < 18)]
    DAY <- actual$DAY[which(actual$HR > 7 & actual$HR < 18)]
    HR <- actual$HR[which(actual$HR > 7 & actual$HR < 18)]
    SOLR <- actual$SOLR[which(actual$HR > 7 & actual$HR < 18)]
    
    TYPE <- rep("Actual",length(YEAR))
    actual <- data.frame(YEAR,MON,DAY,HR,TYPE,SOLR)
    colnames(actual)<- c("YEAR","MON","DAY","HR","TYPE","SOLR")
    
    newVector = actual

    if(length(pred)>0 & input$cb1day) {
      TYPE <- rep("pred",length(pred$YEAR))
      pred <- data.frame(pred$YEAR,pred$MON,pred$DAY,pred$HR,TYPE,pred$SOLR)
      colnames(pred)<- c("YEAR","MON","DAY","HR","TYPE","SOLR")
      newVector = rbind(newVector,pred)
    }
    
    if(length(ln2f1)>0 & input$cb2h) {
      TYPE <- rep("ln2f1",length(ln2f1$YEAR))
      ln2f1 <- data.frame(ln2f1$YEAR,ln2f1$MON,ln2f1$DAY,ln2f1$HR,TYPE,ln2f1$SOLR)
      colnames(ln2f1)<- c("YEAR","MON","DAY","HR","TYPE","SOLR")
      newVector = rbind(newVector,ln2f1)
    }
    
    if(length(ln2f2)>0 & input$cb2f2h) {
      TYPE <- rep("ln2f2",length(ln2f2$YEAR))
      ln2f2 <- data.frame(ln2f2$YEAR,ln2f2$MON,ln2f2$DAY,ln2f2$HR,TYPE,ln2f2$SOLR)
      colnames(ln2f2)<- c("YEAR","MON","DAY","HR","TYPE","SOLR")
      newVector = rbind(newVector,ln2f2)
    }
     
    if(length(ln2f3)>0 & input$cb2f3h) {
      TYPE <- rep("ln2f3",length(ln2f3$YEAR))
      ln2f3 <- data.frame(ln2f3$YEAR,ln2f3$MON,ln2f3$DAY,ln2f3$HR,TYPE,ln2f3$SOLR)
      colnames(ln2f3)<- c("YEAR","MON","DAY","HR","TYPE","SOLR")
      newVector = rbind(newVector,ln2f3)
    }

    if(length(knn)>0 & input$cbknn) {
      TYPE <- rep("KNN",length(knn$YEAR))
      knn <- data.frame(knn$YEAR,knn$MON,knn$DAY,knn$HR,TYPE,knn$SOLR)
      colnames(knn)<- c("YEAR","MON","DAY","HR","TYPE","SOLR")
      newVector = rbind(newVector,knn)
    }
    
    if(length(dhline)>0 & input$dhline) {
      TYPE <- rep("dhline",length(dhline$YEAR))
      dhline <- data.frame(dhline$YEAR,dhline$MON,dhline$DAY,dhline$HR,TYPE,dhline$SOLR)
      colnames(dhline)<- c("YEAR","MON","DAY","HR","TYPE","SOLR")
      newVector = rbind(newVector,dhline)
    }
    
    newVector = data.frame(
      as.POSIXct(paste(newVector$YEAR,"-",newVector$MON,"-",newVector$DAY," ",newVector$HR,":","00",sep="")),
        newVector$SOLR,newVector$TYPE)

    colnames(newVector)<- c("DATE","SOLR","TYPE")
    
    list(newVector=newVector) 
  })
  
  # plot Annotated Time Line 
  output$actualPlot <- renderGvis({

    startDate = as.POSIXct(as.Date(input$date)-60)
    endDate   = as.POSIXct(as.Date(input$date)+60)
    
    data = datasetInput()$newVector
    data = data[which(data$DATE > startDate & data$DATE < endDate),]
    
    gvisAnnotatedTimeLine(data, 
                          datevar="DATE",
                          numvar="SOLR",
                          idvar="TYPE",
                          options=list(displayAnnotations=FALSE,
                                       width="1500px", height="900px",
                                       zoomStartTime=as.Date(input$date)-2,
                                       zoomEndTime=as.Date(input$date)+2, 
                                       scaleType='allmaximized',
                                       colors="['red','ForestGreen','blue','orange','gray','Violet','Gold']",
                                       allowRedraw = TRUE))
    
  })
  
  #### Plot Absolute Mean Error ####
 
 output$absErrorPlot <- renderPlot({
   
   startDate = as.Date(input$date) - 14
   endDate   = as.Date(input$date)
   
   data = datasetInput()$newVector
   
   actualDate = data$SOLR[which(as.Date(data$DATE) > startDate
                            & as.Date(data$DATE) > endDate
                            & data$TYPE == 'Actual')]

   predDate = data$SOLR[which(as.Date(data$DATE) > startDate
                          & as.Date(data$DATE) > endDate
                          & data$TYPE == 'pred')]

   ln2f1Date = data$SOLR[which(as.Date(data$DATE) > startDate
                          & as.Date(data$DATE) > endDate
                          & data$TYPE == 'ln2f1')]

   ln2f2Date = data$SOLR[which(as.Date(data$DATE) > startDate
                          & as.Date(data$DATE) > endDate
                          & data$TYPE == 'ln2f2')]

   ln2f3Date = data$SOLR[which(as.Date(data$DATE) > startDate
                          & as.Date(data$DATE) > endDate
                          & data$TYPE == 'ln2f3')]

   knnDate = data$SOLR[which(as.Date(data$DATE) > startDate
                          & as.Date(data$DATE) > endDate
                          & data$TYPE == 'KNN')]

   dhlineDate = data$SOLR[which(as.Date(data$DATE) > startDate
                          & as.Date(data$DATE) > endDate
                          & data$TYPE == 'dhline')]
   ###### Calculate the mean error  ######
   
   absErropred   = mean(abs(predDate - actualDate),na.rm=TRUE)
   absErroLn2f1  = mean(abs(ln2f1Date - actualDate),na.rm=TRUE)
   absErroLn2f2  = mean(abs(ln2f2Date - actualDate),na.rm=TRUE)
   absErroLn2f3  = mean(abs(ln2f3Date - actualDate),na.rm=TRUE)
   absErroKnn    = mean(abs(knnDate - actualDate),na.rm=TRUE)
   absErroDhline = mean(abs(dhlineDate - actualDate),na.rm=TRUE)
   
   absErropred   = c(absErropred,'Pred')
   absErroLn2f1  = c(absErroLn2f1,'Ln2f1')
   absErroLn2f2  = c(absErroLn2f2,'Ln2f2')
   absErroLn2f3  = c(absErroLn2f3,'Ln2f3')
   absErroKnn    = c(absErroKnn,'Knn')
   absErroDhline = c(absErroDhline,'Dhline')
   
   histData = rbind(absErropred,absErroLn2f1,absErroLn2f2,absErroLn2f3,absErroKnn,absErroDhline)
   histData = data.frame(as.numeric(histData[,1]),histData[,2])
   colnames(histData) <- c("ERROR","MODEL")
   
   qplot(MODEL,ERROR,data=histData,
         geom="histogram",
         binwidth = 0.5,
         stat="identity",
         main = "Mean Error", 
         xlab = "Model",  
         fill=I("blue"), 
         col=I("red"), 
         alpha=I(.1))
   
 }, height = 275, width = 800)
  
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
