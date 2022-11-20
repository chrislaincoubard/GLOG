library(shiny)

ui <- fluidPage(
  titlePanel("Premiers pas SHINY"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Ceci est un help test"),
      
      radioButtons("col_choice", "Input radio buttons",
                   c("Mean", "Liste", "des", "tests", "stats")),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)),
      
      checkboxGroupInput("col_choice_2", "choose the column",
                         choices = c("Choose a file first")),
      
      fileInput("filechoser", label = "filechoser", accept = ".csv"),
      checkboxInput("header", "HEADER", TRUE),
    ),
    
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_and_max"),
      tableOutput("contents"),
      textOutput("Mean")
    )
  )
)


server <- function(input, output,session) {
  
  observe({
    updateCheckboxGroupInput(session, "col_choice_2", 
                             label = "choose the column(s)",
                             choices = colnames(read_file()))
    print(colnames(read_file()))
  })
  update_list <- reactive({
    choice_list = colnames(read_file())
    choice_list
  })
  
  selected <- reactive({
    curr_selected <- input$col_choice_2
    curr_selected
  })
  
  read_file <- reactive({
    inFile <- input$filechoser
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ";")  
    return(df)
  })
  
  
  output$contents <- renderTable({
    df <- read_file()
    head(df[input$col_choice_2], 10)
  })
  
  output$selected_var <- renderText({
    paste("You have selected this", input$col_choice)
  })
  output$min_and_max <- renderText({
    paste(input$col_choice_2)
  })
  
  # output$mean <- renderText ({
  #   data = read_file()
  #   choice = input$var
  #   print(choice)
  #   if (is.null(data)){
  #     paste("You still have not imported files")
  #     return (NULL)
  #   }
  #   moy = mean(data[,as.integer(choice)])
  #   paste("The mean is : ", moy)
  # })
  output$Mean <- renderText ({
    df = read_file()
    moy = mean(as.numeric(unlist(na.omit(df[input$col_choice_2]))))
    paste("La moyenne est de", moy)
  })
  
}

shinyApp(ui = ui, server = server)
