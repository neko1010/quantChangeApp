
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(utils)
library(ggplot2)
library(dplyr)

source("./helpersQC.R")

# Define UI ----
ui <- page_sidebar(
  title = "Time series analysis for quantifying restoration impacts",
  sidebar = sidebar(
    width = 500,
    helpText(
        "Use the figure below to select an analysis method",
        "and analyze your data from the ",
        tags$a(href = "https://nekolarik.users.earthengine.app/view/monthly-mrrmaid",
        "Monthly MRRMaid Google Earth Engine (GEE) web application."),
        " Download .csv files from plots produced in the GEE app and apply any of the methods we have",
        "collated from the manuscript titled: ",
        tags$a(href = "https://doi.org/10.1111/rec.70184", 
               "Time series analyses to demonstrate restoration outcomes and system change from satellite data."),
        "Please refer to the manuscript for details regarding each method.", 
        "For the BSTS, we recommend using the data from the  GRIDMET plot",
        "produced in the Monthly MRRMaid application as the required predictor."
      ),
    
    card_image("./fig1.png")),
    fileInput("file1", label = "Choose time series file (.csv)", accept = ".csv" ),
    selectInput(
        "select",
        "Choose analysis method",
        choices = list("BCP" , "BFAST", "BEAST" , "BSTS" ),
        selected = "BCP" 
      ),
    ## omit for now
   # selectInput(
   #   "freq", 
   #   label = "Select number of observations/cyle", 
   #   choices = 2:24,
   #   selected = 4),
    
    selectInput(
        "date", 
        label = "Select intervention year (BSTS)", 
        choices = 2004:2025),
    
    fileInput("file2", label = "Climate predictor (for BSTS)", accept = ".csv" ),
    mainPanel(
      ## Define plot size
      plotOutput("plot",  width = "700px", height = "500px"),
      textOutput("summary"),
      downloadButton("dwnld")
  )
)

server <- function(input, output, session) {
  
  #output$file <- renderTable(input$file1)
  
  data <- reactive({
    req(input$file1)
    
    ext <- tools::file_ext(input$file1$name)
    switch(ext,
           csv = read.csv(input$file1$datapath)
    )
  })
  
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
  
  obsFreq = reactive({
    input$freq
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
    
    if(input$select == "BSTS"){
      validate(
        need(input$file2 != "", "Please select a predictor file")
      )
    }
    #make_plot(method(), datafile(), predfile(), restDate(), obsFreq())
    make_plot(method(), datafile(), predfile(), restDate(), 4)
  })
  
 # ## summary
 output$summary = renderText({
  req(data())
   
  if(input$select == "BSTS"){
    validate(
      need(input$file2 != "", "Please select a predictor file")
    )
  }
   
  #make_sum(method(), datafile(), predfile(), restDate(), obsFreq())
  make_sum(method(), datafile(), predfile(), restDate(), 4)
  })
 
 
 output$dwnld <- downloadHandler(
   filename = "output.png" ,
   content <- function(file) {
     png(file, width = 700, height = 500, units = "px", pointsize = 12,
         bg = "white", res = NA)
     plt = make_plot(method(), datafile(), predfile(), restDate(), obsFreq())
     print(plt)
     dev.off()},
   contentType = 'image/png'
 )
}

#      
#    })
#  })
### define the shiny object w ui and server obj params
shinyApp(ui = ui, server = server)