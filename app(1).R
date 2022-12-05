library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("GLOG -- STATS"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("sep", "Separator", 
                   choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''),
                   selected = ","),
      
      fileInput("filechoser", label = "Choose a file", accept = ".csv"),
      conditionalPanel(
        condition = 'input.master === "Display"',
      varSelectizeInput("test", "TEST", data = ""),
      ),
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
          actionButton("stdev_btn", "Calculate standard deviation"),
          actionButton("sum_btn", "Calculate sum"),
          actionButton("ratio_btn", "Calculate ratio"),
          actionButton("prop_btn", "Calculate proportion"),
          actionButton("prev_btn", "Calculate prevalence"),
          actionButton("death_btn", "Calculate Death rate"),
          actionButton("reset", "Reset")),
        conditionalPanel(
          condition = 'input.master === "Plots"',
          conditionalPanel(
            condition = "output.fileUploaded == true",
            textInput("currentChoice", "Choose which country to show", value = "WORLD", width = NULL, placeholder = NULL)            
          )
        )
      
    )),

    mainPanel(
      tabsetPanel(id = "master",
                  tabPanel("Display", dataTableOutput("contents")),
                  tabPanel("Stats", 
                           conditionalPanel(
                           condition = 'input.master === "Stats"',
                           verbatimTextOutput("text_stats")),
                  ),
                  tabPanel("Plots", )),
                  conditionalPanel(
                  condition = 'input.master === "Plots"',
                  plotOutput("plotTest"),
                  plotOutput("plotDeath")
        )
    )
)
)


server <- function(input, output,session) {
  
  output$plot_stats <- renderPlot(
    if ((!(is.null(RV$X))) & (!(is.null(RV$Y)))) {
      df <- read_file()
      if (is.null(input$row_choice)){
        ggplot(data   =df,
               mapping = aes(x = unlist(df[RV$X]),
                             y = unlist(df[RV$Y]))) + geom_point(col="#058E40") + ggtitle("Plot of x by Y") +
          xlab(RV$X) + ylab(RV$Y)

      }
      else {
        df2 <- df[df[,input$filter] %in% c(input$row_choice),]

        ggplot(data   =df2,
               mapping = aes(x = unlist(df2[RV$X]),
                             y = unlist(df2[RV$Y]))) + geom_point(col="#058E40")  +
          xlab(RV$X) + ylab(RV$Y)
      }
    }

  )
  
  
  output$plotTest <- renderPlot({
    df = read_file()
    df$date = as.Date(df$date,"%Y-%m-%d")
    subCase = df[df$location=='World',c("total_cases")]
    subTime = df[df$location=='World',c("date")]
    plot(subCase ~ subTime, df, xaxt = "n", type = "l",
         main= "Nombre de cas confirmé depuis le 1er mai 2022",
         xlab= "Date",
         ylab= "Nombre total de cas enregistré."
         
         )
    axis(1, df$date, format(df$date, "%b %d"), cex.axis = .7)
  })
  
  output$plotDeath <- renderPlot({
    df = read_file()
    df$date = as.Date(df$date,"%Y-%m-%d")
    subDeath = df[df$location=='World',c("total_deaths")]
    subTime = df[df$location=='World',c("date")]
    plot(subDeath ~ subTime, df, xaxt = "n", type = "l")
    axis(1, df$date, format(df$date, "%b %d"), cex.axis = .7)
  })
  
  read_file <- reactive({
    inFile <- input$filechoser
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep)  
    return(df)
  })
  #Pour stocker les résultats des popups
  RV <- reactiveValues(X = NULL, Y = NULL)
  
  observe({
    updateVarSelectizeInput(session, "Test", 
                            data = read_file())
  })
  
  observe({
    updateCheckboxGroupInput(session, "column_choice", 
                             label = "choose the column(s)",
                             choices = colnames(read_file()),
                             selected = colnames(read_file()))
  })
  
  observeEvent(input$filter, {
    df = read_file()
    updateSelectizeInput(session, "row_choice",
                         choices = df[df$location,])
  })
  
  observeEvent(input$reset, {
    RV$mean = c()
  })
  
  observeEvent(input$reset, {
    RV$stdev = c()
  })
  
  observeEvent(input$reset, {
    RV$sum = c()
  })
  
  observeEvent(input$reset, {
    RV$num = c()
    RV$den = c()
  })
  observeEvent(input$reset, {
    RV$nump = c()
    RV$denp = c()
  })
  observeEvent(input$reset, {
    RV$cas = c()
    RV$tp = c()
  })
  observeEvent(input$reset, {
    RV$death = c()
    RV$tc = c()
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
    observeEvent(input$stdev_btn, {
      # display a modal dialog (popup)
      showModal(modalDialog(
        tags$h2('Please Choose the columns you want to compute their standard deviation'),
        checkboxGroupInput("stdev_choice", "Stdev_choice", 
                           choices = colnames(read_file())),
        footer=tagList(
          actionButton('submit', 'Submit'),
          modalButton('cancel')
        )
      ))
  })
  
    observeEvent(input$sum_btn, {
      # display a modal dialog (popup)
      showModal(modalDialog(
        tags$h2('Please Choose the columns you want to compute their sum'),
        checkboxGroupInput("sum_choice", "Sum_choice", 
                           choices = colnames(read_file())),
        footer=tagList(
          actionButton('submit', 'Submit'),
          modalButton('cancel')
        )
      ))
    })
    
    observeEvent(input$ratio_btn, {
      # display a modal dialog (popup)
      showModal(modalDialog(
        tags$h2('Please Choose the columns you want to compute their ratio'),
        checkboxGroupInput("ratio_num", "Numerator", 
                           choices = colnames(read_file())),
        checkboxGroupInput("ratio_den", "Denominator", 
                           choices = colnames(read_file())),
        footer=tagList(
          actionButton('submit', 'Submit'),
          modalButton('cancel')
        )
      ))
    })
    
    observeEvent(input$prop_btn, {
      # display a modal dialog (popup)
      showModal(modalDialog(
        tags$h2('Please Choose the columns you want to use compute the proportion'),
        checkboxGroupInput("prop_num", "Numerator", 
                           choices = colnames(read_file())),
        checkboxGroupInput("prop_den", "Denominator", 
                           choices = colnames(read_file())),
        footer=tagList(
          actionButton('submit', 'Submit'),
          modalButton('cancel')
        )
      ))
    })

    observeEvent(input$prev_btn, {
      # display a modal dialog (popup)
      showModal(modalDialog(
        tags$h2('Please Choose the column corresponding to cases'),
        checkboxGroupInput("cases", "Cases ", 
                           choices = colnames(read_file())),
        numericInput("totpop", "Total population", 
                           1),
        footer=tagList(
          actionButton('submit', 'Submit'),
          modalButton('cancel')
        )
      ))
    })
    
    observeEvent(input$death_btn, {
      # display a modal dialog (popup)
      showModal(modalDialog(
        tags$h2('Please Choose the columns you want to use compute the death rate'),
        checkboxGroupInput("deaths", "Deaths", 
                           choices = colnames(read_file())),
        checkboxGroupInput("totcases", "Total cases", 
                           choices = colnames(read_file())),
        footer=tagList(
          actionButton('submit', 'Submit'),
          modalButton('cancel')
        )
      ))
    })
  observeEvent(input$plot, {
    df <- read_file()
    showModal(modalDialog(
      tags$h2("Please choose your axis"),
       selectInput(inputId = "X_axis",label = "X axis",
                   choices = colnames(df)),
       selectInput(inputId = "Y_axis", label = "Y axis",
                   choices = colnames(df)),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to keep",
                       choices = unlist(df[input$filter]),
                       selected = " ",
                     multiple = TRUE),
      
      footer = tagList(
        actionButton('submit_plot', "Submit"),
        modalButton('cancel')
      ))
      
      
    )
    
  })
  
  observeEvent(input$submit_plot, {
    removeModal()
    RV$X <- input$X_axis
    RV$Y <- input$Y_axis
    RV$row_choice <- input$row_choice
  })
  
  observeEvent(input$submit, {
      removeModal()
      RV$mean <- input$mean_choice
  })
  
  observeEvent(input$submit, {
    removeModal()
    RV$stdev <- input$stdev_choice
  })
  
  observeEvent(input$submit, {
    removeModal()
    RV$sum <- input$sum_choice
  })
  
  observeEvent(input$submit, {
    removeModal()
    RV$num <- input$ratio_num
    RV$den <- input$ratio_den
    
  })
  
  observeEvent(input$submit, {
    removeModal()
    RV$nump <- input$prop_num
    RV$denp <- input$prop_den
    
  })
  
  observeEvent(input$submit, {
    removeModal()
    RV$cas <- input$cases
    RV$tp <- input$totpop
    
  })
  
  observeEvent(input$submit, {
    removeModal()
    RV$death <- input$deaths
    RV$tc <- input$totcases
    
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(read_file()))
  })
  
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  output$contents <- renderDataTable({
    if (input$show_df == 1){
      df <- read_file()
      df[, input$column_choice, drop = FALSE]
      }
    else {
      NULL
    }
    
  })

  # output$plot_stats <- renderPlot(
  #   if ((!(is.null(RV$X))) & (!(is.null(RV$Y)))) {
  #     df <- read_file()
  #     if (is.null(input$row_choice)){
  #       ggplot(data   =df,              
  #              mapping = aes(x = unlist(df[RV$X]),   
  #                            y = unlist(df[RV$Y]))) + geom_point(col="#058E40") + ggtitle("Plot of x by Y") +
  #         xlab(RV$X) + ylab(RV$Y)
  # 
  #     }
  #     else {
  #       df2 <- df[df[,input$filter] %in% c(input$row_choice),]
  #       
  #       ggplot(data   =df2,              
  #              mapping = aes(x = unlist(df2[RV$X]),   
  #                            y = unlist(df2[RV$Y]))) + geom_point(col="#058E40")  +
  #         xlab(RV$X) + ylab(RV$Y)
  #     }
  #   }
  #   
  # )
  
  output$text_stats <- renderText ({
    final_msg = ""
        df = read_file()
        if (length(RV$mean >1)){
          for (value in unlist(RV$mean)){
            moy = mean(as.numeric(unlist(na.omit(df[value]))))
            final_msg = paste(final_msg, "La moyenne est de", moy, "\n")
          }
        }
    
    if (length(RV$stdev >1)){
      for (value in unlist(RV$stdev)){
        var = var(as.numeric(unlist(na.omit(df[value]))))
        final_msg = paste(final_msg, "La variance est de", var, "\n")
      }
    }
        
        if (length(RV$sum >1)){
          for (value in unlist(RV$sum)){
            sum = sum(as.numeric(unlist(na.omit(df[value]))))
            final_msg = paste(final_msg, "La somme est de", sum, "\n")
          }
        }
        
        if (length(RV$num >1 & RV$den >1)){
          
          ratio = sum(as.numeric(unlist(na.omit(df[RV$num]))))/sum(as.numeric(unlist(na.omit(df[RV$den]))))
          final_msg = paste(final_msg, "Le ratio est de", ratio, "\n")
        }
        
        if (length(RV$nump >1 & RV$denp >1)){
          
          prop = sum(as.numeric(unlist(na.omit(df[RV$nump]))))/(sum(as.numeric(unlist(na.omit(df[RV$denp])))) + sum(as.numeric(unlist(na.omit(df[RV$nump])))))
          final_msg = paste(final_msg, "La proportion est de", prop, "\n")
        }
        
        if (length(RV$cas >1)){
          
          prev = sum(as.numeric(unlist(na.omit(df[RV$cas]))))/RV$tp
          binom = binom.test(sum(as.numeric(unlist(na.omit(df[RV$cas])))),RV$tp,p=0,alternative="less",conf.level=.95)
          pvalue = binom$p.value
          final_msg = paste(final_msg, "La prevalence est de", prev, "\n")
          final_msg = paste(final_msg, "Le binom test(alternative less, 95%) a pour pvalue", pvalue, "\n")
          
        }
        
        if (length(RV$death >1 & RV$tc >1)){
          
          d_rate = sum(as.numeric(unlist(na.omit(df[RV$death]))))/(sum(as.numeric(unlist(na.omit(df[RV$tc])))))
          final_msg = paste(final_msg, "Le taux de mortalite est de", d_rate, "\n")
        }
        
    paste(final_msg, sep = "\n")
          
  })
  
}

shinyApp(ui = ui, server = server)

