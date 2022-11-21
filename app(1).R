library(shiny)

ui <- fluidPage(
  titlePanel("GLOG -- STATS"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Upload a file, choose the test you want to perform and then choose the column for the chosen test"),
      
      radioButtons("test_choice", "Input radio buttons",
                   c("Mean", "None"), selected = character(0)),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)),
      checkboxInput("show", "show_dataframe"),
      
      checkboxGroupInput("column_choice", "choose the column",
                         choices = c("Choose a file first")),
      
      fileInput("filechoser", label = "filechoser", accept = ".csv"),
      
    ),
    
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_and_max"),
      dataTableOutput("contents"),
      verbatimTextOutput("Mean")
    )
  )
)


server <- function(input, output,session) {
  
  observe({
    updateCheckboxGroupInput(session, "column_choice", 
                             label = "choose the column(s)",
                             choices = colnames(read_file()))
  })
  
  update_list <- reactive({
    choice_list = colnames(read_file())
    choice_list
  })
  
  selected <- reactive({
    curr_selected <- input$column_choice
    curr_selected
  })
  
  read_file <- reactive({
    inFile <- input$filechoser
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ";")  
    return(df)
  })
  
  
  output$contents <- renderDataTable({
    if (input$show) {
      df <- read_file()
      return (df)
    }
    NULL
  })
  
  output$selected_var <- renderText({
    if (length(input$column_choice)){
      paste("You have selected this", input$column_choice)
    }
    else {
      paste("")
    }
  })

  output$Mean <- renderText ({
    
    final_msg = ""
    if (length(input$test_choice) | input$test_choice == "None"){
      if (input$test_choice == "Mean"){
        df = read_file()
        if (length(input$column_choice)){
          for (value in unlist(input$column_choice)){
            moy = mean(as.numeric(unlist(na.omit(df[value]))))
            final_msg = paste(final_msg, "La moyenne est de", moy, "\n")
          }
        }
        
      }
    }
    
    paste(final_msg, sep = "\n")
    
  })
  
}

shinyApp(ui = ui, server = server)
