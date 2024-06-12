# Load necessary libraries

library(shiny)
library(dplyr)
library(readr)
library(DT)

# Load the cleaned data
file_path <- 'data/cleaned_champions.csv'
champions_data <- read_csv(file_path)

# Define UI for the app
ui <- fluidPage(
  titlePanel("League of Legends Champions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("role", "Select Role:", choices = c("All", unique(champions_data$Role)), selected = "All"),
      selectInput("difficulty", "Select Difficulty:", choices = c("All", unique(champions_data$Difficulty)), selected = "All")
    ),
    mainPanel(
      DT::dataTableOutput("championTable")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  filtered_data <- reactive({
    data <- champions_data
    if (input$role != "All") {
      data <- data %>% filter(Role == input$role)
    }
    if (input$difficulty != "All") {
      data <- data %>% filter(Difficulty == input$difficulty)
    }
    data
  })
  
  output$championTable <- DT::renderDataTable({
    data <- filtered_data()
    data$Image_URL <- paste0('<img src="', data$Image_URL, '" width="200">')
    datatable(data, escape = FALSE, options = list(autoWidth = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
