library(data.table)
library(ggplot2)
library(lubridate)
library(plotly)
library(dplyr)
library(ggmap)
library(ggvis)
library(shiny)
library(shinydashboard)
library(bit64)

# Import csv
crime = fread("Crime.csv")
crime = subset(crime, Category!="")
crime = subset(crime, Date!="X")
crime = subset(crime, Date!="")
crime$date <- mdy(crime$Date)
crime$year <- lubridate::year(crime$date)
crime$shortCat <- substring(crime$Category,1,25)
crime[, DayOfWeek:=factor(DayOfWeek, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))]

ui <- shinydashboard::dashboardPage(
  
  # Application title
  shinydashboard::dashboardHeader(title="Crime"),
  
  # Sidebar with a slider input for number of bins
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      dateRangeInput("date", strong("Date range:"), 
                     start = "2017-01-01", end = "2017-12-31",
                     min = "2010-01-01", max = "2020-12-31"),
      checkboxGroupInput(inputId = "type", label = strong("Crime Categories:"),
                         choices = unique(crime$shortCat),
                         selected = unique(crime$shortCat))#)
    )
  ),
    
    # Show a plot of the generated distribution
    shinydashboard::dashboardBody(
      plotOutput("distPlot", height = "600")
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Subset data
  d <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    crime1 = subset(crime, date >= input$date[1] & date <= input$date[2])
    crime1 = subset(crime1, shortCat %in% input$type)
    crime1[, .N, by =.(shortCat, DayOfWeek)][,Percentage:=N/sum(N)*100, by = shortCat][order(N, decreasing = T)]
  })
  
  output$distPlot <- renderPlot(
  {
    dta <- d()
    g = ggplot(dta, aes(x = DayOfWeek, y = shortCat, fill = Percentage)) + 
      geom_tile() +
      geom_hline(yintercept=seq(.5, 40, by=1), size = 1, color = 'grey') + 
      scale_fill_distiller(palette = 'RdYlGn') + 
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),axis.text=element_text(size=11),axis.text.x=element_text(angle = 45, hjust = 1))
    g
  })
}

# Run the application 
shinyApp(ui = ui, server = server)