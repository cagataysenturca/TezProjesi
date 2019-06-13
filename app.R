library(shiny)
library(reprex)
library(googleVis)
library(tidyverse)
library(leaflet)



mydat <- data.table(year = df2$YEAR,
                     month=df2$MONTH,
                     hour=df2$HOUR,
                     day_of_week=df2$DAY_OF_WEEK,
                     offense_code_group=df2$OFFENSE_CODE_GROUP
                    )
                     

ui <- fluidPage( tags$head(
  tags$style(HTML("
                  
                  .well{
                  
                  
                  min-height: 20px;
                  padding: 19px;
                  margin-bottom: 19px;
                  background-color: rgba(175, 205, 133, 0.31);
                  
                  border: 1px solid #e31f16;
                  border-radius: 4px;
                  
                  
                  }
                  
                  .col-sm-4{
                  width: 49.33333333%
                  
                  }
                  
                  .container-fluid {
                  
                  padding-right: 20px;
                  padding-left: 20px;
                  margin-bottom: 15px;
                  margin-right: auto;
                  margin-left: auto
                  
                  }
                  
                  
                  
                  
                  label {
                  
                  display: inline-block;
                  max-width: 100%;
                  margin-bottom: 5px;
                  font-weight: 500;
                  color: #632E67;
                  
                  }
                  
                  
                  
                  h1 {
                  font-family: 'Lobster', cursive;
                  font-weight: 500;
                  line-height: 1.1;
                  color: #632E67;
                  margin-bottom: 20px;
                  font-style: italic; 
                  }
                  
                  h2 {
                  font-family: 'Lobster', cursive;
                  font-weight: 500;
                  line-height: 1.1;
                  color:#632E67;
                  margin-bottom: 13px;
                  font-style: italic; 
                  }

                  h3 {
                  font-size : 25px
                  
                  color: #632E67;
                  font-style: italic; 
                  }
                  
                  
                  
                  
                  body {
                    
                  background-color:rgb(252, 233, 253    );
                  }
                  
                  "))
  
  
  
  ),
  
  
  
  
  
  
  title = "BOSTON CITY CRIME DATASET ANALYSIS",
  br(),
  
  
  
 
  
  
  fluidRow(
    column(3,
           dateRangeInput(inputId = "date_time",  
                          label = "CHOOSE DATE RANGE", 
                          start = "2015-01-01",
                          end = "2018-01-01"
           )
           
    ),    
    
    
    column(8, 
           h3("BOSTON CITY CRIME DATASET ANALYSIS"),
           tabsetPanel( 
             tabPanel("Data Table", dataTableOutput("table")),
             tabPanel("Summary", h4(verbatimTextOutput("dim")), h4("Structure of Dataset"), verbatimTextOutput("structure"), h4("Summary of Dataset"), 
                      verbatimTextOutput("summary"), h4("Crime Categories"), verbatimTextOutput("categoryTable")),
             tabPanel("Crime Plot", radioButtons("colour", label = "1. Select the color of plot",
                                                 choices = c(c("Green", "Red",
                                                               "Yellow")), selected = "Green"), 
                      selectInput("region", "Region:", 
                                  choices=colnames(select(crimes,3,4,5,13))),
                  
                    
                      
                      
                      
                      plotOutput("phonePlot"),radioButtons("colours", label = "2. Select the color of plot",
                                                           choices = c(blue ,red , yellow)),  column(6, htmlOutput("crimePlot"))),
             
             tabPanel("Crime MAP"
                      
                      
                      ,leafletOutput(outputId="lmap"),
                      numericInput(inputId="Month",label = "Enter the month ",value =1,max = 12) 
                      , numericInput(inputId="Day",label = " Enter the day",value =1,max=7)    
                      ,sliderInput(inputId="Hour", label = "Enter the hour ", min = 0 , max = 24,  value = c(12,4))
                      , textInput(inputId="City",label = "Enter the city")
                      , textInput(inputId="District",label = "Enter the district"),
                      
                      verbatimTextOutput("ccc" )
                      
               
     
                      
             )
             
             
           )
           
           
    )
  )
  
  )



server <- function(input, output) {
 
   output$ccc <-reactive({
    
     
  })
  

    

  
  
  
  #filter data
  crime_data <- reactive({   
    
    
    #subset data frame by date range
    crimes <- crimes[crimes$OCCURRED_ON_DATE %in% seq.Date(input$date_time[1], input$date_time[2], by = "days"),]
    crimes
    
  })
  
  
  
  
  ############# SUMMARY SECTION ###############
  
  #number of row and colomn
  output$table <- renderDataTable({
    crime_data()
  }, options = list(sScrollX = "100%", bFilter=1), searchDelay = 500)  
  
  
  
  output$dim <- renderPrint({
    crime_data_stats <- paste("There are", nrow(crime_data), "rows and", ncol(crime_data()), "columns for boston_crime_data.")
    print(crimes)
  })
  
  #This will print the structure
  output$structure <- renderPrint({
    str(crime_data())
  })
  
  #This will print the summaru
  output$summary <- renderPrint({
    summary(crime_data())
  })
  
  #This will print the different crime categories and their count
  output$categoryTable <- renderPrint({
    table(crime_data()$OFFENSE_CODE_GROUP)
    
    
  })  
  
  
  
  
  
  ############ CRIME PLOT SECTION ###################################
  
  output$phonePlot <- renderPlot({
    
    plot(crimes[,input$region], 
         main=input$region,col =input$colour, ordered = TRUE)
    
  })
  
  
  #googlevis column chart
  
  output$crimePlot <- renderGvis({
    
    #crime categories
    crime_count <- aggregate(crime_data()$OFFENSE_CODE_GROUP, by = list(crime_data()$OFFENSE_CODE_GROUP), FUN = length)
    names(crime_count) <- c("Crime_Category", "Count")
   
    
    
    crime_count_plot <- gvisColumnChart(data = crime_count, xvar="Crime_Category", yvar="Count",
                                        options=list(title="# of Crimes",
                                                     chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out',showTextEvery:8}", vAxis= "{textPosition: 'out'}",colors =input$colours,width=600, height=300 ))
    
    #by different time in a day
    crime_count_time_tag <- aggregate(crime_data()$OFFENSE_CODE_GROUP, by = list(crime_data()$HOUR), FUN = length)
    names(crime_count_time_tag) <- c("Incident Time", "Count")
    
    crime_count_time_tag_plot <- gvisColumnChart(data = crime_count_time_tag, xvar="Incident Time", yvar=("Count"), 
                                                 options=list(title="# of Crimes By Hour", 
                                                              chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}",colors =input$colours, width=600, height=300))
    
    #by different days in a week
    crime_count_dayofweek <- aggregate(crime_data()$OFFENSE_CODE_GROUP, by = list(crime_data()$DAY_OF_WEEK), FUN = length)
    names(crime_count_dayofweek) <- c("Day of Week", "Count")
    
    crime_count_dayofweek_plot <- gvisColumnChart(data = crime_count_dayofweek, xvar="Day of Week", yvar=("Count"),
                                                  options=list(title="# of Crimes By Day of Week", 
                                                               chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}",colors =input$colours, width=600, height=300))
    
    #by different months in a year
    crime_count_monthofyear <- aggregate(crime_data()$OFFENSE_CODE_GROUP, by = list(crime_data()$DISTRICT), FUN = length)
    names(crime_count_monthofyear) <- c("Incident Month", "Count")
    
    crime_count_monthofyear_plot <- gvisColumnChart(data = crime_count_monthofyear, xvar="Incident Month", yvar=("Count"),
                                                    options=list(title="# of Crimes By District", 
                                                                 chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}",colors =input$colours, width=600, height=300)) 
   
    
    ######### month
    
    crime_count_month <- aggregate(crime_data()$OFFENSE_CODE_GROUP, by = list(crime_data()$MONTH), FUN = length)
    names(crime_count_month) <- c("Incident Month", "Count")
    
    crime_count_month_plot <- gvisColumnChart(data = crime_count_month, xvar="Incident Month", yvar=("Count"),
                                                    options=list(title="# of Crimes By Month", 
                                                                 chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}",colors =input$colours, width=600, height=300)) 
    
    
    ###year
    crime_count_year <- aggregate(crime_data()$OFFENSE_CODE_GROUP, by = list(crime_data()$YEAR), FUN = length)
    names(crime_count_year) <- c("Incident Month", "Count")
    
    crime_count_year_plot <- gvisColumnChart(data = crime_count_year, xvar="Incident Month", yvar=("Count"),
                                              options=list(title="# of Crimes By Year", 
                                                           chartArea="{left:50,top:50,width: '75%', height: '75%'}",  hAxis="{textPosition: 'out'}", vAxis= "{textPosition: 'out'}",colors =input$colours, width=600, height=300)) 
    #Merge multiple column chart 
    p <- Reduce(gvisMerge, list(crime_count_plot, crime_count_time_tag_plot, crime_count_dayofweek_plot, crime_count_monthofyear_plot,crime_count_month_plot,crime_count_year_plot))
   })
  
  
  
  
  ##############CRIME MAP SECTION############################ 
  output$lmap <- renderLeaflet( { 
    
    
    m <- leaflet() %>%
      
      addTiles() %>%
      
      
      
      addProviderTiles("Esri.WorldTopoMap")%>%
      addMarkers(data = crimes, lng = ~Long, lat=~ Lat, popup = ~paste(OFFENSE_CODE_GROUP),
                 labelOptions = labelOptions(noHide = F, direction = 'auto'),
                 options = markerOptions(riseOnHover = TRUE),
                 clusterOptions = markerClusterOptions()
      )
    
    m
    
    
    
    
  })
  
  
  
}
  shinyApp(ui = ui, server = server)