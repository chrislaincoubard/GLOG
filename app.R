# webshot::install_phantomjs()
library(colourpicker)
library(data.table)
library(ggplot2)
library(maps)
library(mapview)
library(RColorBrewer)
library(shiny)
library(tmap)

source("maps.R")

data("World")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mapping the data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("filebrowse", "Select file..."),
          
          selectInput("country_col", "Indicate the column containing the country names:",
                      choices = c("Select file first")),
          
          selectInput("col_choice", "Choose the type of data to visualise:",
                      choices = c("Select file first")),
          
          colourInput("color1", "Select first color for gradient", value = "#DEF4FF"),
          colourInput("color2", "Select last color for gradient", value = "#0C4999")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tmapOutput("mapView"),
          downloadButton("downloadMap"),
          dataTableOutput("contents")
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "country_col", 
                      label = "Indicate the column containing the country names:",
                      choices = colnames(read_file()))
  })
  
  observe({
    updateSelectInput(session, "col_choice", 
                             label = "Choose the type of data to visualise:",
                             choices = colnames(read_file()))
  })
  
  update_list <- reactive({
    choice_list = colnames(read_file())
    choice_list
  })
  
  selected <- reactive({
    curr_selected <- input$col_choice
    curr_selected
  })
  
  countries <- reactive({
    col <- input$country_col
    col
  })
  
  col1 <- reactive({
    input$color1
  })
  
  col2 <- reactive({
    input$color2
  })
  
  read_file <- reactive({
    inFile <- input$filebrowse
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header=T, sep=";", fileEncoding="UTF-8-BOM")
    return(df)
  })
  
  current_map <- reactive({
    df <- read_file()
    if (is.null(df)){
      p = plot_empty_map()
    }
    else{
      country_col <- countries()
      data_type <- selected()
      color1 <- col1()
      color2 <- col2()
      p = plot_map(df, country_col, data_type, color1, color2)
    }
  })
  
  output$mapView <- renderTmap({
    current_map()
  })
  
  output$downloadMap <- downloadHandler(
    filename = "monkeypox_map.png",
    content = function(file) {
      mapshot(tmap_leaflet(current_map()), file = file)
    }
  )
  
  output$contents <- renderDataTable({
    df <- read_file()
    df
    })
}

# Run the application 
shinyApp(ui = ui, server = server)