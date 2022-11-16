library(shiny)

ui <- fluidPage(
  titlePanel("Premiers pas SHINY"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Ceci est un help test"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("2", 
                              "3"),
                  selected = "2"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)),
      
      fileInput("filechoser", label = "filechoser", accept = ".csv"),
      checkboxInput("header", "HEADER", TRUE),
    ),
      
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_and_max"),
      tableOutput("contents"),
      textOutput("mean")
    )
  )
)


server <- function(input, output) {
  
  read_file <- reactive({
    inFile <- input$filechoser
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ";") 
    return(df)
  })
  
  output$contents <- renderTable({
    df <- read_file()
    df
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected this", input$var)
  })
  output$min_and_max <- renderText({
    paste("The min and max are : ", input$range[1], " ", input$range[2])
  })
  
  output$mean <- renderText ({
    data = read_file()
    choice = input$var
    print(choice)
    if (is.null(data)){
      print("You still have not imported files")
      return (NULL)
    }
    moy = mean(data[,as.integer(choice)])
    paste("The mean is : ", moy)
  })
  
}

shinyApp(ui = ui, server = server)
