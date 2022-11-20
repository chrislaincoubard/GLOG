library(shiny)

ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the 2010 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Choice 1", 
                              "Choice 2",
                              "Choice 3", 
                              "Choice 4"),
                  selected = "Percent White"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)),
      
      fileInput("filechoser", label = "filechoser", accept = ".csv"),
      checkboxInput("header", "HEADER", TRUE),
    ),
      
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_and_max"),
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$contents <- renderTable({
   file <- input$filechoser
    
    read.csv(file$datapath, header = input$header, sep = ";")
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected this", input$var)
  })
  output$min_and_max <- renderText({
    paste("The min and max are : ", input$range[1], " ", input$range[2])
  })
  
}

shinyApp(ui = ui, server = server)
