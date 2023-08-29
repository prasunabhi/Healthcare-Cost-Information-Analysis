library(shiny)
library(caret)
library(kernlab) 
library(e1071)
library(tidyverse)
library(imputeTS)
ui <- fluidPage (
  #Read the data
  fileInput("upload", label="Choose an input file for testing:", accept = c(".csv")),
  #Read the actual (solution) data
  fileInput("upload_Solution", label="Choose the solution file:", accept = c(".csv")),
  #get a number (how much of the dataframe to show)
  numericInput("n", "Number of Rows", value = 5, min = 1, step = 1),
  #a place to output a table (i.e., a dataframe)
  tableOutput("headForDF"),
  #output the results (for now, just simple text)
  verbatimTextOutput("txt_results", placeholder = TRUE)
)

server <- function(input, output, session) {
  #load a model, do prediction and compute the confusion matrix
  use_model_to_predict <- function(df, df_solution){
    #load the pre-built model, we named it 'out_model.rda')
    #load(file="svm_model_expensive.rds")
    #use the model with new data
   
    my_model <- readRDS("my_model.rds")
    print(my_model)
    svmPred <- predict(my_model, df)
    print(svmPred)
    #show how the model performed
    df_solution$expensive <- as.factor(df_solution$expensive)
    confusionMatrix(svmPred, df_solution$expensive)
  }
  #require an input file, then read a CSV file
  getTestData <- reactive({ 
    req(input$upload)
    read_csv(input$upload$name)
  })
  #require an the actual values for the prediction (i.e. solution file)
  getSolutionData <- reactive({ 
    req(input$upload_Solution)
    read_csv(input$upload_Solution$name) 
  })
  output$txt_results <- renderPrint({
    #load the data
    dataset <- getTestData() 
    dataset_solution <- getSolutionData()
    #load and use the model on the new data
    use_model_to_predict(dataset, dataset_solution)
  })
  #show a few lines of the dataframe
  output$headForDF <- renderTable({
    df <- getTestData()
    head(df, input$n)
  })
}
shinyApp(ui, server)