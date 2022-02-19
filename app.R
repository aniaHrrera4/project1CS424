#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)



##import data for uic
uicData <- read.csv("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_UIC-Halsted.csv", header=TRUE, stringsAsFactors=FALSE)
ohareData <- read.csv("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_O'Hare_Airport.csv", header=TRUE, stringsAsFactors=FALSE)
chinatownData <- read.csv("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_Cermak-Chinatown.csv", header=TRUE, stringsAsFactors=FALSE)

# convert the dates to the internal format
uicData$newDate <- as.Date(uicData$date, "%m/%d/%Y")
uicData$Month_ <- as.Date(uicData$newDate, "%m")
uicData$Year_ <- as.Date(uicData$newDate, "%Y")


ohareData$newDate <- as.Date(ohareData$date, "%m/%d/%Y")
ohareData$Month_ <- as.Date(ohareData$newDate, "%m")
ohareData$Year_ <- as.Date(ohareData$newDate, "%Y")

chinatownData$newDate <- as.Date(chinatownData$date, "%m/%d/%Y")
chinatownData$Month_ <- as.Date(chinatownData$newDate, "%m")
chinatownData$Year_ <- as.Date(chinatownData$newDate, "%Y")

# convert the rides from strings to numbers
uicData$Rides2 <- as.numeric(gsub(",", "", uicData$rides))
ohareData$Rides2 <- as.numeric(gsub(",", "", ohareData$rides))
chinatownData$Rides2 <- as.numeric(gsub(",", "", chinatownData$rides))

#get list of stations
station <- c(uicData$stationname[1],ohareData$stationname[1],chinatownData$stationname[1])

#aggregates all the rides data by month and year
uicData$year_month <- floor_date(uicData$newDate, "month")
uicData$yearss <- floor_date(uicData$newDate, "year")
data_aggrUIC <- uicData %>%                         # Aggregate data by month
    group_by(year_month) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()
year_aggrUIC <- uicData %>%                         # Aggregate data by year
    group_by(yearss) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()


ohareData$year_month <- floor_date(ohareData$newDate, "month")
ohareData$yearss <- floor_date(ohareData$newDate, "year")
data_aggrOhare <- ohareData %>%                         # Aggregate data by month
    group_by(year_month) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()
year_aggrOhare <- ohareData %>%                         # Aggregate data by year
    group_by(yearss) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()

chinatownData$year_month <- floor_date(chinatownData$newDate, "month")
chinatownData$yearss <- floor_date(chinatownData$newDate, "year")
data_aggrChina <- chinatownData %>%                         # Aggregate data by month
    group_by(year_month) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()
year_aggrChinatownData <- chinatownData %>%                 # Aggregate data by year
    group_by(yearss) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()

listNames <- "Rides2"
years<-c(2001:2021)
months<-c(1:12)

# Define UI for application that draws a histogram

#make side bar with all menu items and input selections
#two tabs for dash board and about me 
sidebar <- dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
                     sidebarMenu(
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL), 
                         menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                         menuItem("About", icon = icon("th"), tabName = "About")),
                     selectInput("Vizualize", "Select the column to visualize", listNames, selected = "Rides2",  selectize = FALSE),
                
                     
                     selectInput("Station1", "Select #1 Station", station, selected = "UIC-Halsted"),
                     selectInput("Year1", "Select the year to visualize", years, selected = 2021),
                     selectInput("Station2", "Select #2 Station", station, selected = "O'Hare Airport"),
                     selectInput("Year2", "Select the year to visualize", years, selected = 2021),
                     selectInput("Station3", "Select #3 Station", station, selected = "Cermak-Chinatown"),
                     selectInput("Year3", "Select the year to visualize", years, selected = 2021)
                     
    )
#the body of the dashboard divided into 3 Columns with 3 rows. ( total bar charts)
body<- dashboardBody(
        tabItems(
           tabItem(tabName = "Dashboard",
        column(4,
            fluidRow(
                column(12,
                       fluidRow(

                           box(title = "Station #1 Total Entries Per Year", solidHeader = TRUE, status = "primary", width = 12,
                              plotOutput("Hist3", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Station #1 Total Entries Per Month", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist2", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Station #1 Entries Per Ride Per Day", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist1", height = 200)
                           )
                       )
                )
            )
        ),
        
    column(4,
            fluidRow(
                column(12,
                       fluidRow(
            
                           box(title = "Station #2 Total Entries Per Year", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist4", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Station #2 Total Entries Per Month", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist5", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Station #2 Entries Per Ride Per Day", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist6", height = 200)
                           )
                       )
                )
            )
        ),
        
        
    column(4,
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Station #3 Total Entries Per Year", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist7", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Station #3 Total Entries Per Month", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist8", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Station #3 Entries Per Ride Per Day", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist9", height = 200)
                           )
                       )
                )
            )
        
        
        
       ),
           ),
        
       tabItem(tabName = "About", p("Project 1 - Subway"),p("Data file from the Chicago Data Portal"),
                                                          p("An interactive visualization in R and Shiny on Shinyapps.io"),
                                                          p("Dashboard initially shows 3 columns and 3 rows each column represent a station with the menu items for each on the 
                                                            sidebar. each column has options to change the station and change the year for that specific station."),
                                                          p("
                                                            colomn 1 is connected station 1 and year 1 choices. 
                                                            colomn 2 is connected station 2 and year 2 choices. 
                                                            colomn 3 is connected station 3 and year 3 choicesn. "),
                                                          p("
                                                            The First row shows a bar chart showing total entries at all the stations for each year (2001, 2002, ... 2021).
                                                            The Second row shows a bar chart showing total entries for each month of each year1 (jan, feb, ... dec) at all the stations. 
                                                            The Third row shows a bar chart showing entries for each day of each year (jan 1, jan 2, ... dec 31) at all the stations."),
                                        
                                                           p(" The User is allowed to use the side bar and change the menu items for each station and year that corresponds\n"),
                                                        
                                                            p("Written by Andrea Herrera"))
    
   )
       
 ) 
#putting everything together
 ui <- dashboardPage(
     dashboardHeader(title = "Project 1 CTA DATA - Andrea Herrera "),
     sidebar,
     body
)  

# Define server logic required to draw a histogram
server <- function(input, output) {
    # increase the default font size
    theme_set(theme_grey(base_size = 14)) 
    
    # calculate the values for each station and year date 
    
    # for station1 year 1
    justOneYearReactive1 <- reactive(
        if ( input$Station1 == "UIC-Halsted"){
            {subset(uicData, year(uicData$newDate) == input$Year1)}
        }else if(input$Station1=="Cermak-Chinatown"){
            {subset(chinatownData, year(chinatownData$newDate) == input$Year1)}
        }else if(input$Station1=="O'Hare Airport"){
            {subset(ohareData, year(ohareData$newDate) == input$Year1)}
        }
    )
    
    justOneMonthperYearReactive1 <- reactive(
        if( input$Station1 == "UIC-Halsted"){
            {subset(data_aggrUIC, year(data_aggrUIC$year_month) == input$Year1)}
        }else if(input$Station1=="Cermak-Chinatown"){
            {subset(data_aggrChina, year(data_aggrChina$year_month) == input$Year1)}
        }else if(input$Station1=="O'Hare Airport"){
            {subset(data_aggrOhare, year(data_aggrOhare$year_month) == input$Year1)}
        }
    )
    
    # for station2 year 2
    justOneYearReactive2 <- reactive(
        if ( input$Station2 == "UIC-Halsted"){
            {subset(uicData, year(uicData$newDate) == input$Year2)}
        }else if(input$Station2=="Cermak-Chinatown"){
            {subset(chinatownData, year(chinatownData$newDate) == input$Year2)}
        }else if(input$Station2=="O'Hare Airport"){
            {subset(ohareData, year(ohareData$newDate) == input$Year2)}
        }
    )
    
    justOneMonthperYearReactive2 <- reactive(
        if( input$Station2 == "UIC-Halsted"){
            {subset(data_aggrUIC, year(data_aggrUIC$year_month) == input$Year2)}
        }else if(input$Station2=="Cermak-Chinatown"){
            {subset(data_aggrChina, year(data_aggrChina$year_month) == input$Year2)}
        }else if(input$Station2=="O'Hare Airport"){
            {subset(data_aggrOhare, year(data_aggrOhare$year_month) == input$Year2)}
        }
    )
    
    # for station3 year 3
    justOneYearReactive3 <- reactive(
        
        if ( input$Station3 == "UIC-Halsted"){
            {subset(uicData, year(uicData$newDate) == input$Year3)}
        }else if(input$Station3=="Cermak-Chinatown"){
            {subset(chinatownData, year(chinatownData$newDate) == input$Year3)}
        }else if(input$Station3=="O'Hare Airport"){
            {subset(ohareData, year(ohareData$newDate) == input$Year3)}
        }
    )
    
    justOneMonthperYearReactive3 <- reactive(
        if( input$Station3 == "UIC-Halsted"){
            {subset(data_aggrUIC, year(data_aggrUIC$year_month) == input$Year3)}
        }else if(input$Station3=="Cermak-Chinatown"){
            {subset(data_aggrChina, year(data_aggrChina$year_month) == input$Year3)}
        }else if(input$Station3=="O'Hare Airport"){
            {subset(data_aggrOhare, year(data_aggrOhare$year_month) == input$Year3)}
        }
    )
    
    #for all the 3 different stations
    YearsReactive1 <- reactive(
        if ( input$Station1 == "UIC-Halsted"){
            {subset(year_aggrUIC, year(year_aggrUIC$yearss)  == years)}
        }else if(input$Station1=="Cermak-Chinatown"){
            {subset(year_aggrChinatownData, year(year_aggrChinatownData$yearss)== years)}
        }else if(input$Station1=="O'Hare Airport"){
            {subset(year_aggrOhare, year(year_aggrOhare$yearss) == years)}
        }
    )  
    YearsReactive2 <- reactive(
        if(input$Station2== "UIC-Halsted"){
            {subset(year_aggrUIC, year(year_aggrUIC$yearss)  == years)}
        }else if(input$Station2=="Cermak-Chinatown"){
            {subset(year_aggrChinatownData, year(year_aggrChinatownData$yearss)== years)}
        }else if(input$Station2=="O'Hare Airport"){
            {subset(year_aggrOhare, year(year_aggrOhare$yearss) == years)}
        }
    )
    YearsReactive3 <- reactive(
        if(input$Station3== "UIC-Halsted"){
            {subset(year_aggrUIC, year(year_aggrUIC$yearss)  == years)}
        }else if(input$Station3=="Cermak-Chinatown"){
            {subset(year_aggrChinatownData, year(year_aggrChinatownData$yearss)== years)}
        }else if(input$Station3=="O'Hare Airport"){
            {subset(year_aggrOhare, year(year_aggrOhare$yearss) == years)}
        }
    )

    # 9 total bar charts
    output$Hist1 <- renderPlot({
        if ( input$Station1 == "UIC-Halsted"){
            color<- "darkblue"
            justOneYear <- justOneYearReactive1()
            
        }else if(input$Station1=="Cermak-Chinatown"){
            color<- "#782208"
            justOneYear <- justOneYearReactive1()
            
        }else{
            color<- "#1c4a36"
            justOneYear <- justOneYearReactive1()
            
        }
        ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Vizualize]))+
            labs(x=paste("Day in", input$Year1), y = "# of rides") + geom_bar(stat="identity", fill = color) +
            scale_x_date(date_breaks = "1 month", date_labels =  "%m/%d", expand = c(0, 0))
    })
    
    output$Hist2 <- renderPlot({
        if ( input$Station1 == "UIC-Halsted"){
            color<- "darkblue"
            monthly<-justOneMonthperYearReactive1()
            
        }else if(input$Station1=="Cermak-Chinatown"){
            color<- "#782208"
            monthly<-justOneMonthperYearReactive1()
            
        }else{
            color<- "#1c4a36"
            monthly<-justOneMonthperYearReactive1()
            
        }

        #monthly<-justOneMonthperYearReactive1()
        ggplot(monthly, aes(x=year_month, y=monthly[,input$Vizualize]))+
            labs(x=paste("Month in", input$Year1), y = "# of rides") + geom_bar(stat="identity",fill = color) +
            scale_x_date(date_breaks = "1 month",date_labels = "%B", expand = c(0, 0))
    })
    
    output$Hist3 <- renderPlot({
        if ( input$Station1 == "UIC-Halsted"){
            color<- "darkblue"
            allYears <- YearsReactive1()
            
        }else if(input$Station1=="Cermak-Chinatown"){
            color<- "#782208"
            allYears <- YearsReactive1()
            
        }else{
            color<- "#1c4a36"
            allYears <- YearsReactive1()
            
        }
        #allYears <- YearsReactive1()
        ggplot(allYears, aes(x=yearss, y=allYears[,input$Vizualize]))+
            labs(x=paste("Per Year in 2000's For:",input$Station1 ), y = "# of rides") + geom_bar(stat="identity",fill = color)+
            scale_x_date(date_breaks = "1 year", date_labels =  "%y", expand = c(0, 0))
       # }
    })
    
    
    output$Hist6 <- renderPlot({
        if ( input$Station2 == "UIC-Halsted"){
            color<- "darkblue"
            justOneYear <- justOneYearReactive2()
            
        }else if(input$Station2=="Cermak-Chinatown"){
            color<- "#782208"
            justOneYear <- justOneYearReactive2()
            
        }else{
            color<- "#1c4a36"
            justOneYear <- justOneYearReactive2()
            
        }
       # justOneYear <- justOneYearReactive2()
        ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Vizualize]))+
            labs(x=paste("Day in", input$Year2), y = "# of rides") + geom_bar(stat="identity", fill = color) +
            scale_x_date(date_breaks = "1 month", date_labels =  "%m/%d", expand = c(0, 0))
    })
    
    output$Hist5 <- renderPlot({
        if ( input$Station2 == "UIC-Halsted"){
            color<- "darkblue"
            monthly<-justOneMonthperYearReactive2()
            
        }else if(input$Station2=="Cermak-Chinatown"){
            color<- "#782208"
            monthly<-justOneMonthperYearReactive2()
            
        }else{
            color<- "#1c4a36"
            monthly<-justOneMonthperYearReactive2()
            
        }
        
      #  monthly<-justOneMonthperYearReactive2()
        ggplot(monthly, aes(x=year_month, y=monthly[,input$Vizualize]))+
            labs(x=paste("Month in", input$Year2), y = "# of rides") + geom_bar(stat="identity",fill = color) +
            scale_x_date(date_breaks = "1 month",date_labels = "%B", expand = c(0, 0))
    })
    
    output$Hist4 <- renderPlot({
        if ( input$Station2 == "UIC-Halsted"){
            color<- "darkblue"
            allYears <- YearsReactive2()
            
        }else if(input$Station2=="Cermak-Chinatown"){
            color<- "#782208"
            allYears <- YearsReactive2()
            
        }else{
            color<- "#1c4a36"
            allYears <- YearsReactive2()
            
        }
       # allYears <- YearsReactive2()
        ggplot(allYears, aes(x=yearss, y=allYears[,input$Vizualize]))+
            labs(x=paste("Per Year in 2000's For:",input$Station2 ), y = "# of rides") + geom_bar(stat="identity",fill = color)+
            scale_x_date(date_breaks = "1 year", date_labels =  "%y", expand = c(0, 0))
        # }
    })
    
    
    
    
    output$Hist9 <- renderPlot({
        if ( input$Station3 == "UIC-Halsted"){
            color<- "darkblue"
            justOneYear <- justOneYearReactive3()
            
        }else if(input$Station3=="Cermak-Chinatown"){
            color<- "#782208"
            justOneYear <- justOneYearReactive3()
            
        }else{
            color<- "#1c4a36"
            justOneYear <- justOneYearReactive3()
            
        }
     #   justOneYear <- justOneYearReactive3()
        ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Vizualize]))+
            labs(x=paste("Day in", input$Year3), y = "# of rides") + geom_bar(stat="identity", fill = color) +
            scale_x_date(date_breaks = "1 month", date_labels =  "%m/%d", expand = c(0, 0))
    })
    
    output$Hist8 <- renderPlot({
        if ( input$Station3 == "UIC-Halsted"){
            color<- "darkblue"
            monthly<-justOneMonthperYearReactive3()
            
        }else if(input$Station3=="Cermak-Chinatown"){
            color<- "#782208"
            monthly<-justOneMonthperYearReactive3()
            
        }else{
            color<- "#1c4a36"
            monthly<-justOneMonthperYearReactive3()
            
        }
        
       # monthly<-justOneMonthperYearReactive3()
        ggplot(monthly, aes(x=year_month, y=monthly[,input$Vizualize]))+
            labs(x=paste("Month in", input$Year3), y = "# of rides") + geom_bar(stat="identity",fill = color) +
            scale_x_date(date_breaks = "1 month",date_labels = "%B", expand = c(0, 0))
    })
    
    output$Hist7 <- renderPlot({
        if ( input$Station3 == "UIC-Halsted"){
            color<- "darkblue"
            allYears <- YearsReactive3()
            
        }else if(input$Station3=="Cermak-Chinatown"){
            color<- "#782208"
            allYears <- YearsReactive3()
            
        }else{
            color<- "#1c4a36"
            allYears <- YearsReactive3()
            
        }
       # allYears <- YearsReactive3()
        ggplot(allYears, aes(x=yearss, y=allYears[,input$Vizualize]))+
            labs(x=paste("Per Year in 2000's For:",input$Station3 ), y = "# of rides") + geom_bar(stat="identity",fill = color)+
            scale_x_date(date_breaks = "1 year", date_labels =  "%y", expand = c(0, 0))
        # }
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
