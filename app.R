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
#uicData$Date <- NULL

# convert the temperatures from strings to numbers
uicData$rides <- as.numeric(as.character(uicData$rides))

station <- c(uicData$stationname)
listNames <- c(colnames(uicData))

years<-c(2001:2021)
options <-c('each day for 2021', 'for each month','for each day of the week')


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Project 1 CTA DATA - Andrea Herrera "),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
                     sidebarMenu(
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                     
                     selectInput("Year", "Select the year to visualize", years, selected = 2021),
                     selectInput("Station", "Select a Station", station, selected = "UIC-Halsted"),
                     selectInput("Vizualize", "Select the column to visualize", listNames, selected = "rides"),
                     selectInput("Option", "Select charts to see", options, selected = "each day for 2021")
                
    ),
    #your dashboard should initially show a bar chart showing total entries at UIC-Halsted 
    #for each year (2001, 2002, ... 2021)
    dashboardBody(
        fluidRow(
            column(12,
                   fluidRow(
                       box(title = "total entries for each year", solidHeader = TRUE, status = "primary", width = 12,
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
    
    )
    
)  
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    # increase the default font size
    theme_set(theme_grey(base_size = 14)) 
    
    
    # calculate the values one time and re-use them in multiple charts to speed things up
    justOneYearReactive <- reactive({subset(uicData, year(uicData$newDate) == input$Year)})
    permonthReactive <- reactive({subset(uicData, month(uicData$newDate) == input$Year)})
    

    output$Hist1 <- renderPlot({
        justOneYear <- justOneYearReactive()
        ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Vizualize]))+
            labs(x=paste("Day in", input$Year), y = "# of rides") + geom_bar(stat="identity") +
            scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
    })
    
    output$Hist2 <- renderPlot({
        ggplot(uicData, aes(x=newDate, y=years))+
            labs(x=paste("per year", input$Year), y = "# of rides") + geom_bar(stat="identity") +
            scale_x_date(date_breaks = "1 year", date_labels =  "%b", expand = c(0, 0))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
