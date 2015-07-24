shinyUI(fluidPage(
  titlePanel("Oahu's Schofield Barracks Station"),
    
sidebarLayout(
  sidebarPanel(
    fluidRow(
      column(4,
          selectInput("station", 
                      label = "Station",
                      choices = c("SCBH1" = "SCBH1" , "KKRH1" = "KKRH1", "KTAH1" = "KTAH1",
                                  "PLHH1" = "PLHH1" , "C0875" = "C0875", "KFWH1" = "KFWH1", 
                                  "MKRH1" = "MKRH1" , "SCEH1" = "SCEH1", "SCSH1" = "SCSH1",
                                  "WNVH1" = "WNVH1" ),
                      selected = "SCBH1"),
          
          dateInput('date',
                    label = 'Select Day',
                    value = "2014-01-05")),
          
      column(4 ,  
             selectInput("var", 
                         label = "Choose a line type",
                         choices = c("Lines" = "l", "Points" = "p",
                                     "Lines And Points" = "b", "Lines Alone" = "c",
                                     "Overplotted" = "o","Histogram" = "h",
                                     "steps" = "s", "No Plotting" = "n"),
                         selected = "l"),
             
             sliderInput("range", 
                         label = "Hour Interval:",
                         min = 7, max = 18, value = c(7, 18),ticks = FALSE,step=1)),
      
        column(4,          
          #checkboxInput("cbActual", label = HTML('<p style="color: red"> Actual </p>'), value = TRUE),
          helpText(HTML('<strong style="color: red"> Actual</strong>')),
          checkboxInput("cb1day", label = HTML('<strong style="color: ForestGreen"> 1 Day Before Probability Model </strong>'), value = TRUE),
          checkboxInput("cb2h", label = HTML('<strong style="color: blue"> 1h Lag Forecast  </strong>'), value = TRUE),
          checkboxInput("cb2f2h", label = HTML('<strong style="color: orange"> 2h Lag Forecast  </strong>'), value = TRUE),
          checkboxInput("cb2f3h", label = HTML('<strong style="color: gray"> 3h Lag Forecast  </strong>'), value = TRUE),
          checkboxInput("cbknn", label = HTML('<strong style="color: Violet"> Euclidian 3 Nearest Neighbors </strong>'), value = TRUE),
          checkboxInput("dhline", label = HTML('<strong style="color: Gold"> Deshourlymean </strong>'), value = TRUE))
          
    )
    ,width = 12),
  
    fluidRow (  
      #mainPanel(
        #Print plot called in server file
        column(2,plotOutput("befYesPlot")),
        column(2,plotOutput("yesPlot")),
        column(4,plotOutput("actualPlot")),
        column(2,plotOutput("tomPlot")),
        column(2,plotOutput("aftTomPlot")) 
        #)
    )
  ),

  fluidRow(
    column(3,imageOutput("vvELImage00")),
    column(3,imageOutput("vvELImage06")),
    column(3,imageOutput("vvELImage12")),
    column(3,imageOutput("vvELImage18"))
  ),

  fluidRow(
    column(3,imageOutput("windImage00")),
    column(3,imageOutput("windImage06")),
    column(3,imageOutput("windImage12")),
    column(3,imageOutput("windImage18"))
  ),

  fluidRow(
    column(3,imageOutput("pwatImage00")),
    column(3,imageOutput("pwatImage06")),
    column(3,imageOutput("pwatImage12")),
    column(3,imageOutput("pwatImage18"))
  ),

  fluidRow(
    column(6,imageOutput("ghiImage00")),
    column(6,imageOutput("ghiImage18"))
  )
))
