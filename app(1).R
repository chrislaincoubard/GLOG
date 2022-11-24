library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("GLOG -- STATS"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("sep", "Separator", 
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      
      fileInput("filechoser", label = "Chose a file", accept = ".csv"),
      
      #pour afficher l'UI de façon conditionnelle
      conditionalPanel(
        condition = "output.fileUploaded == true",
        conditionalPanel(
          condition = "input.master == 'Display'",
          selectInput("show_df", "show_data",
                      choices = c("Don't show data" = 0,"Show Selected" = 1)),
          
          checkboxGroupInput("column_choice", "choose the column",
        )),
        conditionalPanel(
          condition = 'input.master === "Stats"',
          actionButton("mean_btn", "Calculate mean"),
          actionButton("reset", "Reset")),
        conditionalPanel(
          condition = 'input.master === "Plots"',
          actionButton("plot", "Plot"),
          actionButton('reset', "Reset")
        )
      
    )),

    mainPanel(
      tabsetPanel(id = "master",
                  tabPanel("Display", dataTableOutput("contents")),
                  tabPanel("Stats", verbatimTextOutput("text_stats")),
                  tabPanel("Plots", plotOutput("plot_stats")))
    )
)
)


server <- function(input, output,session) {
  
  #Pour stocker les résultats des popups
  RV <- reactiveValues()
  RV$X <- NULL
  RV$Y <- NULL
  
  observe({
    updateCheckboxGroupInput(session, "column_choice", 
                             label = "choose the column(s)",
                             choices = colnames(read_file()),
                             selected = colnames(read_file()))
  })
  
  observeEvent(input$reset, {
    RV$mean = c()
  })
  
  observeEvent(input$mean_btn, {
    # display a modal dialog (popup)
    showModal(modalDialog(
      tags$h2('Please Choose the columns you want to compute their mean'),
      checkboxGroupInput("mean_choice", "Mean_choice", 
                         choices = colnames(read_file())),
      footer=tagList(
        actionButton('submit', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  observeEvent(input$plot, {
    showModal(modalDialog(
      tags$h2("Please choose your axis"),
       selectInput(inputId = "X_axis",label = "X axis",
                   choices = colnames(read_file())),
       selectInput(inputId = "Y_axis", label = "Y axis",
                   choices = colnames(read_file())),
      footer = tagList(
        actionButton('submit_plot', "Submit"),
        modalButton('cancel')
      )
      
      
    ))
  })
  
  observeEvent(input$submit_plot, {
    removeModal()
    print(input$X_axis)
    print(input$Y_axis)
    RV$X <- input$X_axis
    RV$Y <- input$Y_axis
  })
  
  observeEvent(input$submit, {
      removeModal()
      RV$mean <- input$mean_choice
  })
  
  read_file <- reactive({
    inFile <- input$filechoser
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep)  
    return(df)
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(read_file()))
  })
  
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  output$contents <- DT::renderDataTable({
    if (input$show_df == 1){
      df <- read_file()
      print(unlist(input$column_choice))
      
      return(DT::datatable(df[, input$column_choice, drop = FALSE]))
      }
    
    NULL
  })

  output$plot_stats <- renderPlot(
    if ((!(is.null(RV$X))) & (!(is.null(RV$Y)))) {
      df <- read_file()
      
      plot(x = unlist(df[RV$X]), y = unlist(df[RV$Y]), xlab = RV$X, ylab = RV$Y)
    }
    
  )
  
  output$text_stats <- renderText ({
    final_msg = ""
        df = read_file()
        if (length(RV$mean >1)){
          for (value in unlist(RV$mean)){
            moy = mean(as.numeric(unlist(na.omit(df[value]))))
            final_msg = paste(final_msg, "La moyenne est de", moy, "\n")
          }
        }
    paste(final_msg, sep = "\n")
  })
  
}

shinyApp(ui = ui, server = server)

