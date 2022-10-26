#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

library(shiny)
library(ggplot2)
library(DT)
library(bslib)

mobility <- read.csv("movement_data.csv", sep =';')
mobility$Date <- as.Date(mobility$Date) #as.Date() method returns the object of a class "Date"
mobility$Province <- as.factor(mobility$Province) #returns the original object of a class with the requested column specified as a factor rather than a numeric
#UI
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "darkly",
    
  ),
  
  titlePanel("Utopia (Covid-19 Data)"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "dv", label = "Category",
                  choices = c("Retail_Recreation", "Grocery_Pharmarcy", "Parks", "Transit_Stations", "Workplaces", "Residential"),
                  selected = "Grocery_Pharmarcy"),
      
      selectInput(inputId = "provinces", "Province(s)",
                  choices = levels(mobility$Province),
                  multiple = TRUE,
                  selected = "Freetown"),
      dateRangeInput(inputId = "date", label = "Date Range",
                     start = min(mobility$Date),
                     end = max (mobility$Date))
    ),
    
    mainPanel(
      plotOutput (outputId = "plot"),
      em("Positive and negative percentages indicate an increase and decrease from the baseline period (median value between January 3 and February 6, 2020) respectively."),
      
      DT::dataTableOutput(outputId = "table"),
      
      
    )
  )
)

#Server
server <- function(input, output) {
  filtered_data <- reactive({
    subset(mobility,
           Province %in% input$provinces & 
             Date >= input$date[1] & Date <= input$date[2])
  })
  
  output$plot <- renderPlot ({
    ggplot(filtered_data(),
           aes_string(x="Date", y=input$dv, color="Province")) + geom_point(alpha = 0.5) + 
      ylab("%change from baseline")
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
    
  )
  
}

shinyApp(ui = ui, server = server)