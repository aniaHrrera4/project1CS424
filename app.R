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


##import data for uic
uicData <- read.csv("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_UIC-Halsted.csv", header=TRUE, stringsAsFactors=FALSE)
# convert the dates to the internal format
uicData$newDate <- as.Date(uicData$date, "%m/%d/%Y")
uicData$MonthDate <- as.Date(uicData$date, "%m/%Y")

# convert the temperatures from strings to numbers
uicData$rides <- as.numeric(as.character(uicData$rides))

station <- c(uicData$stationname)
listNames <- c(colnames(uicData))

years<-c(2001:2021)
months<-c(1:12)
options <-c('each day for 2021', 'for each month','for each day of the week')


# Define UI for application that draws a histogram


sidebar <- dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
                     sidebarMenu(
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                         menuItem("About", icon = icon("th"), tabName = "About")),
                     
                     
                     selectInput("Year", "Select the year to visualize", years, selected = 2021),
                     selectInput("Station", "Select a Station", station, selected = "UIC-Halsted"),
                     selectInput("Vizualize", "Select the column to visualize", listNames, selected = "rides"),
                     selectInput("Option", "Select charts to see", options, selected = "each day for 2021")
                
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
                           box(title = "Total entries per year", solidHeader = TRUE, status = "primary", width = 12,
                              plotOutput("Hist3", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Total entries per month", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist2", height = 200)
                           )
                       )
                )
            ),
            fluidRow(
                column(12,
                       fluidRow(
                           box(title = "Entries per ride per day", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("Hist1", height = 200)
                           )
                       )
                )
            )
        
       ),
        
       tabItem(tabName = "About", p("Project 1 - Subway"),p("download the data file from the Chicago Data Portal and break the file into appropriate chunks that are less than 5 MB each
                                                            use lubridate to convert the date information into a more usable form
                                                            create an interactive visualization in R and Shiny on Shinyapps.io
                                                            your dashboard should initially show a bar chart showing total entries at UIC-Halsted for each year (2001, 2002, ... 2021)
                                                            allow the user to choose to see each of the following charts (either individually or all at the same time)
                                                            bar chart showing entries at UIC-Halsted each day for 2021 (jan 1, jan 2, ... dec 31)
                                                            bar chart showing total entries at UIC-Halsted for each month for 2021 (jan, feb, ... dec)
                                                            bar chart showing total entries at UIC-Halsted for each day of the week for 2021 (mon, tue, ... sun)
                                                            allow the user to use a menu to choose any of the years from 2001 - 2021 and have all of the UIC-Halsted charts update for the chosen year
                                                            allow the user to see the data for each of the charts as a table in the same order
                                                            have an 'about page' in your app, perhaps as a separate tab in the shiny interface, with appropriate credits (where the data is from, who wrote the app, when, why, etc.)"))
    
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
    justOneYearReactive <- reactive({subset(uicData, year(uicData$newDate) == input$Year)})
    YearsReactive <- reactive({subset(uicData, year(uicData$newDate) == years)})
    permonthReactive <- reactive({subset(uicData, year(uicData$MonthDate) == input$Year)})
    

    output$Hist1 <- renderPlot({
        justOneYear <- justOneYearReactive()
        ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Vizualize]))+
            labs(x=paste("Day in", input$Year), y = "# of rides") + geom_bar(stat="identity") +
            scale_x_date(date_breaks = "1 day", date_labels =  "%m/%d", expand = c(0, 0))
    })
    
    output$Hist2 <- renderPlot({
       # justOnemonth <- permonthReactive()
        justOneYear <- justOneYearReactive()
        #ggplot(justOnemonth, aes(x=MonthDate, y=justOnemonth[,input$Vizualize]))+
        ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Vizualize]))+
            labs(x=paste("Month in", input$Year), y = "# of rides") + geom_bar(stat="identity") +
            scale_x_date(date_breaks = "1 month", date_labels = "%B", expand = c(0, 0))
    })
    
    output$Hist3 <- renderPlot({
        allYears <- YearsReactive()
        ggplot(allYears, aes(x=newDate, y=allYears[,input$Vizualize]))+
            labs(x="Per Year", y = "# of rides") + geom_bar(stat="identity")+
            scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0, 5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
