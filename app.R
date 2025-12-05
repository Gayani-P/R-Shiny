# Resources: 
#https://github.com/ashleighlatter/shiny-basics/blob/main/filtering/filter_instantly.R
#https://www.youtube.com/watch?v=mvB1FvAfaH0

# Load packages ----
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(shinydashboard)
library(fresh)
library(plotly)
library(DT)
library(bs4Dash)




# Load the data ----
ridership<- read_csv("data/Ridership.csv")%>%
  
  #Convert to DF
  as.data.frame()%>% 
  
  #Select Variables of Interest
  select(Line = "route_name",
         Direction = "dir_id",
         DayType = "day_type_name",
         TimePeriod = "time_period_name",
         Stop = "stop_name",
         AverageOn = "average_ons",
         AverageOff = "average_offs",
         AverageFlow = "average_flow")%>%
  
  #Make Variables More Readable
  mutate(Direction = case_when(
    Direction == "EB" ~ "Eastbound",
    Direction == "WB" ~ "Westbound",
    Direction == "SB" ~ "Southbound",
    Direction == "NB" ~ "Northbound"), 
    
    TimePeriod = case_when(
      TimePeriod == "VERY_EARLY_MORNING" ~ "(3am - 5:59am)",
      TimePeriod == "EARLY_AM" ~ "(6am - 6:59am)",
      TimePeriod == "AM_PEAK" ~ "(7am - 7:59am)",
      TimePeriod == "MIDDAY_BASE" ~ "(9am - 1:29pm)",
      TimePeriod == "MIDDAY_SCHOOL" ~ "(1:30pm - 3:59pm)",
      TimePeriod == "PM_PEAK" ~ "(4pm - 6:29pm)",
      TimePeriod == "EVENING" ~ "(6:30pm - 9:59pm)",
      TimePeriod == "LATE_EVENING" ~ "(10pm - 11:59pm)",
      TimePeriod == "NIGHT" ~ "(12am - 2:59am)", 
      TimePeriod == "OFF_PEAK" ~ "Off Peak"))%>%
  
  #making time period show up in order
  mutate(
    TimePeriod_sort = TimePeriod, 
    TimePeriod_sort = case_when(
      TimePeriod_sort == "(3am - 5:59am)" ~ "1",
      TimePeriod_sort == "(6am - 6:59am)" ~ "2",
      TimePeriod_sort == "(7am - 7:59am)" ~ "3",
      TimePeriod_sort == "(9am - 1:29pm)" ~ "4",
      TimePeriod_sort == "(1:30pm - 3:59pm)" ~ "5",
      TimePeriod_sort == "(4pm - 6:29pm)" ~ "6",
      TimePeriod_sort == "(6:30pm - 9:59pm)" ~ "7",
      TimePeriod_sort == "(10pm - 11:59pm)" ~ "8",
      TimePeriod_sort == "(12am - 2:59am)" ~ "9")) %>%
  
  arrange((TimePeriod_sort))

#Sub-setting ridership data by train line 
ridership_red<-ridership%>%filter(Line == "Red Line")
ridership_orange<-ridership%>%filter(Line == "Orange Line")
ridership_green<-ridership%>%filter(Line == "Green Line")
ridership_blue<-ridership%>%filter(Line == "Blue Line")


# Define UI ----

ui <- dashboardPage(
    

    
#HEADER: 
    header = dashboardHeader(
      title = dashboardBrand(
        title = "MBTA Fall 2023 Ridership",
        color = "gray-dark")),
  
#SIDEBAR: 
    sidebar = dashboardSidebar(
    
    #List Tabs in Sidebar  
    sidebarMenu(
      menuItem("Red Line", tabName = "redline"),
      menuItem("Orange Line", tabName = "orangeline"),
      menuItem("Green Line", tabName = "greenline"),
      menuItem("Blue Line", tabName = "blueline"))), 
    
#BODY: 
      
    body = dashboardBody(
      
     
      tabItems(
        
        #Red Line Tab Content 
        tabItem(tabName = "redline",
                fluidRow(
                column(width = 6,
 
                       #Direction
                       selectInput(
                         inputId = "direction_red",
                         label = "Direction:",
                         choices = c("Northbound", "Southbound")),
                       
                       #Day Type
                       selectInput(
                         inputId = "daytype_red",
                         label = "Day Type:",
                         choices = unique(ridership$DayType)),
                       
                       #Time of Day
                       selectInput(
                         inputId = "timeperiod_red",
                         label = "Time of Day:",
                         choices = unique(ridership$TimePeriod)),
                       
                       #Stop
                       selectInput(
                         inputId = "stop_red",
                         label = "Stop Name:",
                         choices = unique(ridership_red$Stop))),
                
                column(width = 3,
                       imageOutput("redline_image")),
                
                box(title = "Ridership", 
                    width = 12, 
                    plotlyOutput("redline_plot")))),
        
        #Orange Line Tab Content 
        tabItem(tabName = "orangeline",
                fluidRow(
                  column(width = 6,
                         
                         #Direction
                         selectInput(
                           inputId = "direction_orange",
                           label = "Direction:",
                           choices = c("Northbound", "Southbound")),
                         
                         #Day Type
                         selectInput(
                           inputId = "daytype_orange",
                           label = "Day Type:",
                           choices = unique(ridership$DayType)),
                         
                         #Time of Day
                         selectInput(
                           inputId = "timeperiod_orange",
                           label = "Time of Day:",
                           choices = unique(ridership$TimePeriod)),
                         
                         #Stop
                         selectInput(
                           inputId = "stop_orange",
                           label = "Stop Name:",
                           choices = unique(ridership_orange$Stop))),
                  
                  column(width = 3,
                         imageOutput("orangeline_image")),
                  
                  box(title = "Ridership", 
                      width = 12, 
                      plotlyOutput("orangeline_plot")))), 
        
        #Green Line Tab Content 
        tabItem(tabName = "greenline",
                fluidRow(
                  column(width = 6,
                         
                         #Direction
                         selectInput(
                           inputId = "direction_green",
                           label = "Direction:",
                           choices = c("Eastbound", "Westbound")),
                         
                         #Day Type
                         selectInput(
                           inputId = "daytype_green",
                           label = "Day Type:",
                           choices = unique(ridership$DayType)),
                         
                         #Time of Day
                         selectInput(
                           inputId = "timeperiod_green",
                           label = "Time of Day:",
                           choices = unique(ridership$TimePeriod)),
                         
                         #Stop
                         selectInput(
                           inputId = "stop_green",
                           label = "Stop Name:",
                           choices = unique(ridership_green$Stop))),
                  
                  column(width = 3,
                         imageOutput("greenline_image")),
                  
                  box(title = "Ridership", 
                      width = 12, 
                      plotlyOutput("greenline_plot")))), 
        
        #Blue Line Tab Content 
        tabItem(tabName = "blueline",
                fluidRow(
                  column(width = 6,
                         
                         #Direction
                         selectInput(
                           inputId = "direction_blue",
                           label = "Direction:",
                           choices = c("Eastbound", "Westbound")),
                         
                         #Day Type
                         selectInput(
                           inputId = "daytype_blue",
                           label = "Day Type:",
                           choices = unique(ridership$DayType)),
                         
                         #Time of Day
                         selectInput(
                           inputId = "timeperiod_blue",
                           label = "Time of Day:",
                           choices = unique(ridership$TimePeriod)),
                         
                         #Stop
                         selectInput(
                           inputId = "stop_blue",
                           label = "Stop Name:",
                           choices = unique(ridership_blue$Stop))),
                  
                  column(width = 3,
                         imageOutput("blueline_image")),
                  
                  box(title = "Ridership", 
                      width = 12, 
                      plotlyOutput("blueline_plot"))))

      
      
    )))

# Server Function ----

server<- function(input, output) {
  
  #Redline Ridership Plot
  output$redline_plot <- renderPlotly({
    
    #reactive dataframe
    filtered_data <- reactive ({
                     ridership %>% 
                     filter (Direction == input$direction_red, 
                             DayType == input$daytype_red, 
                             TimePeriod == input$timeperiod_red, 
                             Stop == input$stop_red)})
    
    #plot characteristics
    plot_data_on <- filtered_data() 
    plot_data_off <- filtered_data() 
    plot_data_flow <- filtered_data() 
    
    x = c("Average On", "Average Off", "Average Through")
    y = c(plot_data_on$AverageOn[1],
          plot_data_off$AverageOff[1], 
          plot_data_flow$AverageFlow[1]) 
    
    df<-data.frame(x, y)
    
    plot_ly(
      data = df,
      y = ~y,
      x = x,
      type = "bar",
      marker = list(color = "red"),
      orientation = "v") %>% 
      
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Number of Passengers")) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  #Orangeline Ridership Plot
  output$orangeline_plot <- renderPlotly({
    
    #reactive dataframe
    filtered_data <- reactive ({
      ridership %>% 
        filter (Direction == input$direction_orange, 
                DayType == input$daytype_orange, 
                TimePeriod == input$timeperiod_orange, 
                Stop == input$stop_orange)})
    
    
    plot_data_on <- filtered_data() 
    plot_data_off <- filtered_data() 
    plot_data_flow <- filtered_data() 
    
    x = c("Average On", "Average Off", "Average Through")
    y = c(plot_data_on$AverageOn[1],
          plot_data_off$AverageOff[1], 
          plot_data_flow$AverageFlow[1]) 
    
    df<-data.frame(x, y)
    
    plot_ly(
      data = df,
      y = ~y,
      x = x,
      type = "bar",
      marker = list(color = "orange"),
      orientation = "v") %>% 
      
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Number of Passengers")) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  #Greenline Ridership Plot
  output$greenline_plot <- renderPlotly({
    
    #reactive dataframe
    filtered_data <- reactive ({
      ridership %>% 
        filter (Direction == input$direction_green, 
                DayType == input$daytype_green, 
                TimePeriod == input$timeperiod_green, 
                Stop == input$stop_green)})
    
    #plot characteristics
    plot_data_on <- filtered_data() 
    plot_data_off <- filtered_data() 
    plot_data_flow <- filtered_data() 
    
    x = c("Average On", "Average Off", "Average Through")
    y = c(plot_data_on$AverageOn[1],
          plot_data_off$AverageOff[1], 
          plot_data_flow$AverageFlow[1]) 
    
    df<-data.frame(x, y)
    
    plot_ly(
      data = df,
      y = ~y,
      x = x,
      type = "bar",
      marker = list(color = "green"),
      orientation = "v") %>% 
      
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Number of Passengers")) %>%
      
      config(displayModeBar = FALSE)
    
  })
  
  #Blueline Ridership Plot
  output$blueline_plot <- renderPlotly({
    
    #reactive dataframe
    filtered_data <- reactive ({
      ridership %>% 
        filter (Direction == input$direction_blue, 
                DayType == input$daytype_blue, 
                TimePeriod == input$timeperiod_blue, 
                Stop == input$stop_blue)})
    
    #plot characteristics
    plot_data_on <- filtered_data() 
    plot_data_off <- filtered_data() 
    plot_data_flow <- filtered_data() 
    
    x = c("Average On", "Average Off", "Average Through")
    y = c(plot_data_on$AverageOn[1],
          plot_data_off$AverageOff[1], 
          plot_data_flow$AverageFlow[1]) 
    
    df<-data.frame(x, y)
    
    plot_ly(
      data = df,
      y = ~y,
      x = x,
      type = "bar",
      marker = list(color = "blue"),
      orientation = "v") %>% 
      
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Number of Passengers")) %>%
      
      config(displayModeBar = FALSE)
    

  })
  
  #Loading Images for Each Tab
  output$redline_image<-renderImage(
    list(src = "images/redline.png",
         contentType = "image/png",
         width = 350,
         height = 350, 
         alt = "This is a dynamically generated plot"))
  
  output$orangeline_image<-renderImage(
    list(src = "images/orangeline.png",
         contentType = "image/png",
         width = 350,
         height = 350, 
         alt = "This is a dynamically generated plot"))
  
  output$greenline_image<-renderImage(
    list(src = "images/greenline.png",
         contentType = "image/png",
         width = 350,
         height = 350, 
         alt = "This is a dynamically generated plot"))
  
  output$blueline_image<-renderImage(
    list(src = "images/blueline.png",
         contentType = "image/png",
         width = 350,
         height = 350, 
         alt = "This is a dynamically generated plot"))
  
  
  #DISPLAY TABLE
  output$table_data <- renderDT({
    
    filtered_data()
    
  })
  
  
  
}

shinyApp(ui, server)













