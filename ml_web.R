# Load necessary libraries
library(shiny)
library(dplyr)
library(DT)
library(mongolite)
library(caret)

# Connect to MongoDB and load the cleaned data
mongo_conn <- mongolite::mongo(collection = "champions", db = "league_of_legends", url = "mongodb://localhost")
champions_data <- mongo_conn$find()

# Manually add a Strength column for demonstration purposes
# In practice, this column should be based on actual data or labels
set.seed(123)
champions_data$Strength <- sample(c("Strong", "Weak"), nrow(champions_data), replace = TRUE)

# Data Preprocessing
champions_data <- champions_data %>%
  filter(!is.na(Role) & Role != "NA") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Description_Length = nchar(as.character(Description)))

# Train a Random Forest model to predict Strength
set.seed(123)
trainIndex <- createDataPartition(champions_data$Strength, p = .8, list = FALSE, times = 1)
train_data <- champions_data[trainIndex, ]
test_data <- champions_data[-trainIndex, ]

model <- train(Strength ~ Description_Length + Difficulty, data = train_data, method = "rf")

# Use the trained model to predict the strength of all champions
champions_data$Predicted_Strength <- predict(model, champions_data)

# Define UI for the app
ui <- fluidPage(
  titlePanel("League of Legends Champions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("role", "Select Role:", choices = c("All", as.character(unique(champions_data$Role))), selected = "All"),
      selectInput("strength", "Select Strength:", choices = c("All", "Strong", "Weak"), selected = "All")
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
    if (input$strength != "All") {
      data <- data %>% filter(Predicted_Strength == input$strength)
    }
    data
  })
  
  output$championTable <- DT::renderDataTable({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(datatable(data.frame(), options = list(autoWidth = TRUE)))
    }
    data <- data %>%
      select(Name, Image_URL, Description, Role, Difficulty, Predicted_Strength) %>%  # Ensure columns are selected properly
      mutate(Image_URL = paste0('<img src="', Image_URL, '" width="100">'))  # Properly format image URL
    datatable(data, escape = FALSE, options = list(autoWidth = TRUE), 
              rownames = FALSE, 
              colnames = c("Name", "Image", "Description", "Role", "Difficulty", "Predicted Strength"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
