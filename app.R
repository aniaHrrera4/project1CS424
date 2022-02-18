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


station <- c(uicData$stationname[1],ohareData$stationname[1],chinatownData$stationname[1])

#aggregates all the rides data by month and year
uicData$year_month <- floor_date(uicData$newDate, "month")
data_aggrUIC <- uicData %>%                         # Aggregate data
    group_by(year_month) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()

ohareData$year_month <- floor_date(ohareData$newDate, "month")
data_aggrOhare <- ohareData %>%                         # Aggregate data
    group_by(year_month) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()

chinatownData$year_month <- floor_date(chinatownData$newDate, "month")
data_aggrChina <- chinatownData %>%                         # Aggregate data
    group_by(year_month) %>% 
    dplyr::summarize(Rides2 = sum(Rides2)) %>% 
    as.data.frame()


listNames <- c(colnames(uicData))
years<-c(2000:2021)
months<-c(1:12)
color <- "red"

# Define UI for application that draws a histogram


sidebar <- dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
                     sidebarMenu(
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                         menuItem("About", icon = icon("th"), tabName = "About")),
                     
                     
                     selectInput("Year", "Select the year to visualize", years, selected = 2021),
                     selectInput("Station", "Select a Station", station, selected = "UIC-Halsted"),
                     selectInput("Vizualize", "Select the column to visualize", listNames, selected = "Rides2")
    )
    #your dashboard should initially show a bar chart showing total entries at UIC-Halsted 
    #for each year (2001, 2002, ... 2021)
body<- dashboardBody(
        tabItems(
           tabItem(tabName = "Dashboard",
            fluidRow(
                column(12,
                       fluidRow(
                           #tabBox(
                            #   title = "UIC",
                               # The id lets us use input$tabset1 on the server to find the current tab
                            #   id = "tabset1", height = "400px",
                            #   tabPanel("total entries per year",plotOutput("Hist3", height = 200)),
                            #   tabPanel("total entries per month", plotOutput("Hist2", height = 200))
                          # )
                           box(title = "Total Entries Per Year", solidHeader = TRUE, status = "primary", width = 12,
                              plotOutput("Hist3", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Total Entries Per Month", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist2", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Entries Per Ride Per Day", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist1", height = 200)
                           )
                       )
                )
            )
        
       ),
        
       tabItem(tabName = "About", p("Project 1 - Subway"),p("Data file from the Chicago Data Portal
                                                            An interactive visualization in R and Shiny on Shinyapps.io
                                                            Dashboard initially show a bar chart showing total entries at UIC-Halsted for each year (2001, 2002, ... 2021)
                                                            allow the user to choose to see each of the following charts (either individually or all at the same time)
                                                            bar chart showing entries at UIC-Halsted each day for 2021 (jan 1, jan 2, ... dec 31)
                                                            bar chart showing total entries at UIC-Halsted for each month for 2021 (jan, feb, ... dec)
                                                            bar chart showing total entries at UIC-Halsted for each day of the week for 2021 (mon, tue, ... sun)
                                                            allow the user to use a menu to choose any of the years from 2001 - 2021 and have all of the UIC-Halsted charts update for the chosen year
                                                            allow the user to see the data for each of the charts as a table in the same order
                                                            
                                                            
                                                            
                                                            Written by Andrea Herrera"))
    
   )
       
 ) 
 
 ui <- dashboardPage(
     dashboardHeader(title = "Project 1 CTA DATA - Andrea Herrera "),
     sidebar,
     body
)  

# Define server logic required to draw a histogram
server <- function(input, output) {
    # increase the default font size
    theme_set(theme_grey(base_size = 14)) 
    
    # calculate the values one time and re-use them in multiple charts to speed things up

    justOneYearReactive <- reactive(
        if ( input$Station == "UIC-Halsted"){
            {subset(uicData, year(uicData$newDate) == input$Year)}
        }else if(input$Station=="Cermak-Chinatown"){
            {subset(chinatownData, year(chinatownData$newDate) == input$Year)}
        }else{
            {subset(ohareData, year(ohareData$newDate) == input$Year)}
        }
    )
    
    justOneMonthperYearReactive <- reactive(
        if ( input$Station == "UIC-Halsted"){
            {subset(data_aggrUIC, year(data_aggrUIC$year_month) == input$Year)}
        }else if(input$Station=="Cermak-Chinatown"){
            {subset(data_aggrChina, year(data_aggrChina$year_month) == input$Year)}
        }else{
            {subset(data_aggrOhare, year(data_aggrOhare$year_month) == input$Year)}
        }
    )
    
    YearsReactive <- reactive(
        if ( input$Station == "UIC-Halsted"){
            {subset(data_aggrUIC, year(data_aggrUIC$year_month)  == years)}
        }else if(input$Station=="Cermak-Chinatown"){
            {subset(data_aggrChina, year(data_aggrChina$year_month)== years)}
        }else{
            {subset(data_aggrOhare, year(data_aggrOhare$year_month) == years)}
        }
    )

    output$Hist1 <- renderPlot({
        if ( input$Station == "UIC-Halsted"){
            color<- "darkblue"
        }else if(input$Station=="Cermak-Chinatown"){
            color<- "#782208"
        }else{
            color<- "#1c4a36"
        }
        justOneYear <- justOneYearReactive()
        ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Vizualize]))+
            labs(x=paste("Day in", input$Year), y = "# of rides") + geom_bar(stat="identity", fill = color) +
            scale_x_date(date_breaks = "1 week",date_minor_breaks = "1 week", date_labels =  "%m/%d", expand = c(0, 0))
    })
    
    output$Hist2 <- renderPlot({
        if ( input$Station == "UIC-Halsted"){
            color<- "darkblue"
        }else if(input$Station=="Cermak-Chinatown"){
            color<- "#782208"
        }else{
            color<- "#1c4a36"
        }

        monthly<-justOneMonthperYearReactive()
        ggplot(monthly, aes(x=year_month, y=monthly[,input$Vizualize]))+
            labs(x=paste("Month in", input$Year), y = "# of rides") + geom_bar(stat="identity",fill = color) +
            scale_x_date(date_breaks = "1 month",date_labels = "%B", expand = c(0, 0))
    })
    
    output$Hist3 <- renderPlot({
        if ( input$Station == "UIC-Halsted"){
            color<- "darkblue"
        }else if(input$Station=="Cermak-Chinatown"){
            color<- "#782208"
        }else{
            color<- "#1c4a36"
        }
        allYears <- YearsReactive()
        ggplot(allYears, aes(x=year_month, y=allYears[,input$Vizualize]))+
            labs(x="Per Year", y = "# of rides") + geom_bar(stat="identity",fill = color)+
            scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0, 0))
       # }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
