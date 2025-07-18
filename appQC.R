## https://shiny.posit.co/r/getstarted/shiny-basics/

## https://shiny.posit.co/r/gallery/widgets/widget-gallery/ more on widgets
#install.packages('shiny')
library(shiny)
library(bslib)
library(bsicons)
library(vroom)
library(utils)
library(ggplot2)
library(dplyr)

source("./helpersQC.R")
#runExample("01_hello")
# Define UI ----
ui <- page_sidebar(
  title = "Time series analysis for quantifying restoration impacts",
  sidebar = sidebar(
    helpText(
        "Select an analysis method",
        "and analyze your data. We have",
        "collated methods from the manuscript titled: ",
        "Time series analyses provide a low-cost and scalable way",
        "to assess restoration outcomes from satellite data."
      ),
    fileInput("file1", label = "Time Series", accept = ".csv" ),
    selectInput(
        "select",
        "Choose analysis method",
        choices = list("BCP" , "BFAST", "BEAST" , "BSTS" ),
        selected = "BCP" 
      ),
    selectInput(
        "date", 
        label = "Select intervention year (BSTS)", 
        choices = 2000:2025),
    fileInput("file2", label = "Predictor (for BSTS)", accept = ".csv" ),
    #card(
    #  card_header(""),
    #  actionButton("action", "Submit")#https://shiny.posit.co/r/articles/build/action-buttons/
    #),
    card_image("./fig1.png")),## too small here but doesn't make sense in the main?
    tableOutput("file"),
    textOutput("date"),
    mainPanel(
      #tableOutput("head"),
      plotOutput("plot")
  )
)


server <- function(input, output, session) {
  
  #output$file <- renderTable(input$file1)
  
  data <- reactive({
    req(input$file1)
    
    ext <- tools::file_ext(input$file1$name)
    switch(ext,
           csv = vroom::vroom(input$file1$datapath, delim = ","),
           tsv = vroom::vroom(input$file1$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  #output$head <- renderTable({
  #  req(data())
  #  if (is.null(data())) {
  #    return(NULL)
  #  }   
  #  head(data())
  #  #head(yankee)
  #})
  
  observeEvent(input$file1, {
    message(paste0("File Chosen: ", input$file1))
  })
  
  ## data
  datafile = reactive({
    input$file1$datapath
  })
  
  ## predictor
  predfile = reactive({
    input$file2$datapath
  })
  
  restDate = reactive({
    input$date
  })
  
  output$date = renderText({
    paste0("Restoration date: ", input$date)
  })
  
  ## method
  method = reactive({
    input$select
  })
  
  ## plot
  output$plot = renderPlot({
   req(data())
  
  make_plot(method(), datafile(), predfile(), restDate())

  })
}
#  observeEvent(input$action, {
#    output$data = renderTable({
#      head(df)
#      
#    })
#  })
### define the shiny object w ui and server obj params
shinyApp(ui = ui, server = server)