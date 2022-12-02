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
          
          radioButtons("sep", "Separator", 
                       choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                       selected = ","),
          
          selectInput("country_col", "Column containing the country names",
                      choices = c("Select file first")),
          
          radioButtons("type", "Country data is",
                       choices = c("Full name (English)"="names", "Alpha-2 code" = "a2", "Alpha-3 code" = "a3"),
                       selected = "Full name (English)"),
          
          selectInput("col_choice", "Data to visualise",
                      choices = c("Select file first")),
          
          colourInput("color1", "First color for gradient", value = "#6A994E"),
          colourInput("color2", "Last color for gradient", value = "#CB4749"),
          checkboxInput("third_color", "Include a middle color value", value=TRUE),
          colourInput("color3", "Middle color for gradient", value = "#FFFFFF")
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
                      label = "Column containing the country names:",
                      choices = colnames(read_file()))
  })
  
  observe({
    updateSelectInput(session, "col_choice", 
                             label = "Data to visualise:",
                             choices = colnames(read_file()))
  })
  
  update_list <- reactive({
    colnames(read_file())
  })
  
  selected <- reactive({
    input$col_choice
  })
  
  countries <- reactive({
    input$country_col
  })
  
  ctype <- reactive({
    input$type
  })
  
  col1 <- reactive({
    input$color1
  })
  
  col2 <- reactive({
    input$color2
  })
  
  col3 <- reactive({
    input$color3
  })
  
  three_colors <- reactive({
    input$third_color
  })
  
  read_file <- reactive({
    inFile <- input$filebrowse
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header=T, sep = input$sep, fileEncoding="UTF-8-BOM")
    return(df)
  })
  
  current_map <- reactive({
    df <- read_file()
    if (is.null(df)){
      p = plot_empty_map()
    }
    else{
      country_col <- countries()
      ctype <- ctype()
      data_type <- selected()
      color1 <- col1()
      color2 <- col2()
      if (three_colors()){
        color3 <- col3()
      }
      else{
        color3 <- NULL
      }
      p = plot_map(df, country_col, ctype, data_type, color1, color2, color3)
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

options(shiny.host = "127.0.0.1")
options(shiny.port = 1234)
shinyApp(ui = ui, server = server)
