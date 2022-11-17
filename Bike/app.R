#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)

#Read data
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv"
bike = read.csv(url,check.names=F)
colnames(bike) <- c("date", "count", "hour", "temp", "humidity", "windspeed", 
                    "visibility", "dew", "solar", "rain", "snow", 
                    "seasons", "holiday", "functioning")
bike <- bike %>% 
  filter(functioning == "Yes") %>% 
  select(-14)
head(bike)
bike <- bike %>%
  mutate(date = as.POSIXct(date, format = "%d/%m/%Y"), # date
         month =  factor(months(date)), # add months variable
         hour = factor(hour), 
         seasons = factor(seasons), # factor
         holiday = factor(holiday))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Demand for Rental Bikes in Seoul"),

    # Sidebar 
    sidebarLayout(
      
      sidebarPanel(
        h4(
          "This app will help you know demand of sharing bikes in Seoul! Just use the filters below..."
        ),
        br(),
        #feature 1: filter by season
        checkboxInput("filterSeason", "Filter by season", FALSE),
        conditionalPanel(
          condition = "input.filterSeason",
          uiOutput("seasonSelectorOutput")
        ),
        #feature 2: filter by holiday or not holiday
        radioButtons("holidayInput","Day:",choices = c("No Holiday","Holiday")),
        br(),
        #feature 4: download the full table
        downloadButton("download", "Download filtered table"),
        br(),br(),
        #feature 6: add a picture to UI
        img(src='bike_image.webp', align = "center",width = 350)
       ),
        # Show a plot of the generated distribution
        mainPanel(
           #feature 5: show the number of records
           h3(textOutput("summaryText")),
           plotOutput("distPlot"),
           br(), br(),
           #feature 3: show a summary of weather condition grouped by months
           DT::dataTableOutput("summary")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$seasonSelectorOutput <- renderUI({
    selectInput("seasonInput", "Season:",
                sort(unique(bike$season)),
                selected = "Spring")
  })
  
  counts <- reactive({
    counts <- bike
    if (input$filterSeason) {
      counts <- dplyr::filter(counts, seasons == input$seasonInput)
    }
    counts <- dplyr::filter(counts, holiday == input$holidayInput)
    counts
  })
  
  summary <- reactive({
    summary <- bike
    if (input$filterSeason) {
      summary <- dplyr::filter(summary, seasons == input$seasonInput)
    }
    summary <- dplyr::filter(summary, holiday == input$holidayInput)
    summary <- summary %>% 
      group_by(month) %>% 
      summarise(count = round(mean(count),2),
                temp = round(mean(temp),2),
                humidity = round(mean(humidity),2),
                windspeed = round(mean(windspeed),2),
                visibility = round(mean(visibility),2),
                dew = round(mean(dew),2),
                solar = round(mean(solar),2),
                rain = round(mean(rain),2),
                snow = round(mean(snow),2))
    summary
  })
  
    output$distPlot <- renderPlot({
          ggplot(counts(),aes(hour, count)) +
            geom_col(fill = "#C0C999") +
            ylab("Count") +
            xlab("Hour") + 
            theme_bw() 
    })
    
    output$summary <- DT::renderDataTable({
      summary()
    })
    
    output$summaryText <- renderText({
      numOptions <- nrow(counts())
      if (is.null(numOptions)) {
        numOptions <- 0
      }
      paste0("Total number of records: ", numOptions)
    })
    
    output$download <- downloadHandler(
      filename = function() {
        "bcl-results.csv"
      },
      content = function(con) {
        write.csv(counts(), con)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
