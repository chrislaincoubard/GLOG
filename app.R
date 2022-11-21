library(colourpicker)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(shiny)
#library(tmap)

source("maps.R")

world_data = map_data("world")
world_data$ISO2 = iso.alpha(world_data$region,2)

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
          
          colourInput("col", "Select colour", value = "ivory")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("mapView"),
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
    print(colnames(read_file()))
  })
  
  observe({
    updateSelectInput(session, "col_choice", 
                             label = "Choose the type of data to visualise:",
                             choices = colnames(read_file()))
    print(colnames(read_file()))
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
  
  read_file <- reactive({
    inFile <- input$filebrowse
    if (is.null(inFile))
      return(NULL)
    df <- fread(inFile$datapath)
    return(df)
  })
  
  # output$summary <- renderPrint({
  #   df <- read_file()
  #   paste(cat("File summary:\n"),summary(df))
  # })
  
  output$mapView <- renderPlot({
    df <- read_file()
    data_type <- selected
    worldMaps(df, world_data, data_type)
  })
  
  output$contents <- renderDataTable({
    df <- read_file()
    df
    })
}

# Run the application 
shinyApp(ui = ui, server = server)