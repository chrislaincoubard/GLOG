### Library imports
library(colourpicker)
library(maps)
library(mapview)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(tmap)
library(webshot)
if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()

### Map functions import
source("functions_maps.R")

### Background world map
data("World")


########## USER INTERFACE ##########
ui <- fluidPage(
    useShinyjs(),

    ### Application title
    titlePanel("Mapping the data"),

    ### Sidebar
    sidebarLayout(
        sidebarPanel(
          # File upload and parsing
          fileInput("filebrowse", "Select file...", accept = c(".csv", ".tsv")),
          
          radioButtons("sep", "Separator", 
                       choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                       selected = ","),
          
          # Data specification
          selectInput("country_col", "Column containing the country names",
                      choices = c("Select file first")),
        
          radioButtons("type", "Country data is",
                       choices = c("Full name (English)"="names", "Alpha-2 code" = "a2", "Alpha-3 code" = "a3"),
                       selected = "names"),
        
          selectInput("col_choice", "Data to visualise",
                      choices = c("Select file first")),
          
          
          # Visual parameters (hidden by default, toggled with show_params button)
          actionButton("show_params", "Show color settings"),
          br(),
          br(),
          hidden(
            colourpicker::colourInput("color1", "First color for gradient", value = "#E3F2FD"),
            colourpicker::colourInput("color2", "Last color for gradient", value = "#0D47A1"),
            checkboxInput("third_color", "Include a middle color value", value=FALSE),
            colourpicker::colourInput("color3", "Middle color for gradient", value = "#FFFFFF"),
            actionButton("color_reset", "Reset colors")
          )
        ),

        mainPanel(
          # Map output
          tmapOutput("mapView"),
          
          # Download map object
          downloadButton("downloadMap"),
          
          # Dataframe contents
          dataTableOutput("contents")
        )
    )
    
    
)



########## SERVER FUNCTIONS ##########

server <- function(input, output, session) {

  ### Read CSV file
  read_file <- reactive({
    inFile <- input$filebrowse
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header=T, sep = input$sep, fileEncoding="UTF-8-BOM")
    return(df)
  })


  ### Toggle visibility of the color settings  
  observeEvent(input$show_params,{
    toggle(selector = "[id*='color']")
  })
  
  ### Toggle the usability of all inputs after file us uploaded
  observe({
    for(n in names(input))
      if (n!="filebrowse"){
        toggleState(id=n, condition = !is.null(read_file()))
      }
  })
  
 
  ### Update lists with columns names
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
  
  ### Reset colors when button is pressed
  observeEvent(input$color_reset, {
    colourpicker::updateColourInput(session, "color1", value = "#E3F2FD")
    colourpicker::updateColourInput(session, "color2", value = "#0D47A1")
    colourpicker::updateColourInput(session, "color3", value = "#FFFFFF")
  })
  
  
  
  ### Create reactive variables
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
  

  ### Map render
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
  
  
  ### Download map
  output$downloadMap <- downloadHandler(
    filename = "monkeypox_map.png",
    content = function(file) {
      mapshot(tmap_leaflet(current_map()), file = file)
    }
  )
  
  ### Show dataframe contents
  output$contents <- renderDataTable({
    df <- read_file()
    df
    })
}


# Fixed local port
options(shiny.host = "127.0.0.1")
options(shiny.port = 1234)

# Run the application
shinyApp(ui = ui, server = server)