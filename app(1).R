library(shiny)

ui <- fluidPage(
  titlePanel("GLOG -- STATS"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("sep", "Separator", 
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      
      fileInput("filechoser", label = "filechoser", accept = ".csv"),
      
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
          actionButton("mean_btn", "Mean_btn", color = "warning"),
          actionButton("plot", "Plot", color ="warning"),
          
        )
      
    )),
    
    #Pour faire les onglets, un onglet ne peut contenir qu'un output d'après ce que
    #j'ai vu sur la doc donc pour l'instant y'a des onglets imbriqués, a voir si on garde
    mainPanel(
      tabsetPanel(id = "master",
                  tabPanel("Display", dataTableOutput("contents")),
                  tabPanel("Stats",
                           tabsetPanel(id = "stats",
                                       tabPanel("Mean", verbatimTextOutput("Moy")),
                                       tabPanel("Plot", plotOutput("plot")))))
    )
)
)


server <- function(input, output,session) {
  
  #Pour stocker les résultats des popups
  RV <- reactiveValues()
  
  observe({
    updateCheckboxGroupInput(session, "column_choice", 
                             label = "choose the column(s)",
                             choices = colnames(read_file()),
                             selected = colnames(read_file()))
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
      selectInput(id = "X_axis", label = "X axis",
                  choices = colnames(read_file())),
      selectInput(id = "Y_axis", label = "Y axis",
                  choices = colnames(read_file())),
      footer = tagList(
        actionButton('submit_plot', "Submit"),
        actionButton('cancel')
      )
      
      
    ))
  })
  
  observeEvent(input$submit_plot, {
    removeModal()
    RV$X <- input$X_axis
    RV$Y <- inpput$Y_axis
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

  
  
  output$Mean <- renderText ({
    final_msg = ""
    if (length(input$mean_choice) | input$mean_choice == "None"){
        df = read_file()
        if (length(RV$mean)){
          for (value in unlist(RV$mean)){
            moy = mean(as.numeric(unlist(na.omit(df[value]))))
            final_msg = paste(final_msg, "La moyenne est de", moy, "\n")
          }
        }
    }
    paste(final_msg, sep = "\n")
    
  })
  
}

shinyApp(ui = ui, server = server)
