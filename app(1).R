library(shiny)
library(dplyr)
library(ggplot2)
library(epitools)

ui <- fluidPage(
  titlePanel("GLOG -- STATS"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("sep", "Separator", 
                   choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''),
                   selected = ","),
      
      fileInput("filechoser", label = "Choose a file", accept = ".csv"),
      conditionalPanel(
        condition = 'input.master == "Display"',
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
          actionButton("mean_btn", "Mean"),
          actionButton("stdev_btn", "Variance"),
          actionButton("sum_btn", "Sum"),
          actionButton("ratio_btn", "Ratio"),
          actionButton("prop_btn", "Proportion"),
          actionButton("prev_btn", "Prevalence"),
          actionButton("death_btn", "Death Rate"),
          actionButton("incidence", "Incidence Rate and Confidence Interval"),
          actionButton("riskratio", "Risk Ratio"),
          actionButton("oddratio", "Odd Ratio"),
          actionButton("incidence_ratio", "Incidence Rate Ratio"),
          actionButton("reset", "Reset")),
        
        ##### PLOT Inputs #####
        conditionalPanel(
          condition = 'input.master === "Plots"',
          conditionalPanel(
            condition = "output.fileUploaded == true",
            ### Input for the user to choose which column to use ###
            varSelectizeInput("countryName", "Column containing the country name", data = "Select file first"),
            varSelectizeInput("colTime", "Column containing time reference", data = "Select file first"),
            radioButtons("dateFormat", "Date format", 
                         choices = c(YearMonthDay="%Y-%m-%d",DayMonthYear="%d-%m-%Y",MonthDayYear="%m-%d-%Y"),
                         selected = "%Y-%m-%d"),
            varSelectizeInput("colInterest", "Data to visualise", data = "Select file first"),
            ### Input the countries of interest ###
            selectizeInput("currentChoice", "Choose which country to show",choices="World",multiple=TRUE),
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
                              plotOutput("plot_interaction")
                  ),
                  tabPanel("Plots", )),
      conditionalPanel(
        condition = 'input.master === "Plots"',
        plotOutput("plotTest"),
      )
    )
  )
)


server <- function(input, output,session) {
  
############## PLOT Reactive function ##############
  
  ### PLOT 1
  output$plotTest <- renderPlot({
    if (!is.null(read_file())){
      df = read_file()
      df$date = as.Date(df$date,input$dateFormat)
      # Creation du sub dataframe utilisé par ggplot2
      filter = df[,as.character(input$countryName)]==input$currentChoice
      subdf = data.frame(
        location = df[filter,as.character(input$countryName)],
        Date = df[filter,as.character(input$colTime)],
        subCase = df[filter,as.character(input$colInterest)]
      )
      # Creation du plot
      ggplot(subdf, aes(x = Date, y = subCase,colour = location, group =location)) +
        geom_line()+
        ggtitle("Number of total cases by time")

    }})

  
  ########## Reactive function for the plots input ################# 
  
  observe({
    updateSelectizeInput(session, "currentChoice",
                         choices = unique(read_file()[,as.character(input$countryName)],selected="Austria"))
    
  })
  
  observe({
    updateVarSelectizeInput(session, "countryName", 
                            data = read_file())
  })
  
  observe({
    updateVarSelectizeInput(session, "colTime", 
                            data = read_file())
  })
  
  observe({
    updateVarSelectizeInput(session, "colInterest", 
                            data = read_file())
  })
  
  
  ############ READ CSV FUNCTION ##################
  
  read_file <- reactive({
    inFile <- input$filechoser
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep, fileEncoding="UTF-8-BOM")  
    return(df)
  })
  #Pour stocker les résultats des popups
  RV <- reactiveValues(X = NULL, Y = NULL)
  msg_stat <- reactiveVal("")
  
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
                         choices = df[input$filter])
  })
  
  #reset button
  observeEvent(input$reset, {
    msg_stat("")
    output$text_stats <- renderText ({
      paste(msg_stat())
    })
    output$plot_interaction <- renderPlot({})
  })
  
  ########### DATATABLE DISPLAY ########
  output$contents <- renderDataTable({
    if (input$show_df == 1){
      df <- read_file()
      df[, input$column_choice, drop = FALSE]
    }
    else {
      NULL
    }
    
  })
  
  
  
  
  ##############  Mean  ############
  observeEvent(input$mean_btn, {
    df <- read_file()
    showModal(modalDialog(
      tags$h2('Please choose the columns to compute their respective mean :'),
      selectInput("mean_choice", "Mean_choice", 
                  choices = colnames(df), selected = " ", multiple = TRUE),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to exclude",
                     choices = unlist(df[input$filter]),
                     selected = " ",
                     multiple = TRUE),
      footer=tagList(
        actionButton('submit_mean', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  observeEvent(input$submit_mean, {
    removeModal()
    RV$mean <- input$mean_choice
    final_msg = msg_stat()
    updateSelectizeInput(session, "mean_choice", selected = " " )
    df <- read_file()
    if (!(is.null(input$row_choice))) {
      df <- df[!(df[,input$filter] %in% c(input$row_choice)),]
    }
    output$text_stats <- renderText ({
      for (value in unlist(RV$mean)){
        moy <- mean(as.numeric(unlist(na.omit(df[value]))))
        final_msg = paste(final_msg, "Mean of", value," = ", moy, "\n")
      }
      msg_stat(final_msg)
      final_msg
    })
    
  })
  
  ##################  STDEV  ###############
  observeEvent(input$stdev_btn, {
    df <- read_file()
    showModal(modalDialog(
      tags$h2('Please choose the columns to compute their respective variance :'),
      selectInput("stdev_choice", "Stdev_choice", 
                  choices = colnames(df), selected = " ", multiple = TRUE),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to exclude",
                     choices = unlist(df[input$filter]),
                     selected = " ",
                     multiple = TRUE),
      footer=tagList(
        actionButton('submit_std', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  observeEvent(input$submit_std, {
    removeModal()
    RV$stdev <- input$stdev_choice
    df <- read_file()
    if (!(is.null(input$row_choice))) {
      df <- df[!(df[,input$filter] %in% c(input$row_choice)),]
    }
    final_msg = msg_stat()
    output$text_stats <- renderText ({
      for (value in unlist(RV$stdev)){
        var = var(as.numeric(unlist(na.omit(df[value]))))
        final_msg = paste(final_msg, "", var, "\n")
      }
      msg_stat(final_msg)
      final_msg
    })
    
  })
  
  ################ SUM ##############
  
  observeEvent(input$sum_btn, {
    df <- read_file()
    showModal(modalDialog(
      tags$h2('Please choose the columns to compute their respective sum :'),
      selectInput("sum_choice", "Sum_choice", 
                  choices = colnames(df),selected = " ", multiple = TRUE),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to exclude",
                     choices = unlist(df[input$filter]),
                     selected = " ",
                     multiple = TRUE),
      footer=tagList(
        actionButton('submit_sum', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  observeEvent(input$submit_sum, {
    removeModal()
    RV$sum <- input$sum_choice
    df <- read_file()
    if (!(is.null(input$row_choice))) {
      df <- df[!(df[,input$filter] %in% c(input$row_choice)),]
    }
    final_msg = msg_stat()
    output$text_stats <- renderText ({
      for (value in unlist(RV$sum)){
        sum = sum(as.numeric(unlist(na.omit(df[value]))))
        final_msg = paste(final_msg, "La somme est de", sum, "\n")
      }
      msg_stat(final_msg)
      final_msg
    })
    
  })
  
  ######### RATIO ######
  observeEvent(input$ratio_btn, {
    df <- read_file()
    showModal(modalDialog(
      tags$h2('Please choose the columns to compute their ratio :'),
      selectInput("ratio_num", "Numerator", 
                  choices = colnames(df), selected = " "),
      selectInput("ratio_den", "Denominator", 
                  choices = colnames(df), selected = " "),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to exclude",
                     choices = unlist(df[input$filter]),
                     selected = " ",
                     multiple = TRUE),
      footer=tagList(
        actionButton('submit_ratio', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  
  observeEvent(input$submit_ratio, {
    removeModal()
    RV$num <- input$ratio_num
    RV$den <- input$ratio_den
    final_msg = msg_stat()
    df <- read_file()
    if (!(is.null(input$row_choice))) {
      df <- df[!(df[,input$filter] %in% c(input$row_choice)),]
    }
    final_msg = msg_stat()
    output$text_stats <- renderText ({
      ratio = sum(as.numeric(unlist(na.omit(df[RV$num]))))/sum(as.numeric(unlist(na.omit(df[RV$den]))))
      final_msg = paste(final_msg, "Le ratio est de", ratio, "\n")
    })
    msg_stat(final_msg)
    final_msg
  })
  
  
  ######## PROP ########
  
  observeEvent(input$prop_btn, {
    df <- read_file()
    showModal(modalDialog(
      tags$h2('Please choose the columns to compute their proportion :'),
      selectInput("prop_num", "Numerator", 
                  choices = colnames(df), selected = " "),
      selectInput("prop_den", "Denominator", 
                  choices = colnames(df), selected = " "),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to exclude",
                     choices = unlist(df[input$filter]),
                     selected = " ",
                     multiple = TRUE),
      footer=tagList(
        actionButton('submit_prop', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  observeEvent(input$submit_prop, {
    removeModal()
    RV$nump <- input$prop_num
    RV$denp <- input$prop_den
    df <- read_file()
    if (!(is.null(input$row_choice))) {
      df <- df[!(df[,input$filter] %in% c(input$row_choice)),]
    }
    final_msg = msg_stat()
    output$text_stats <- renderText ({
      prop = sum(as.numeric(unlist(na.omit(df[RV$nump]))))/(sum(as.numeric(unlist(na.omit(df[RV$denp])))) + sum(as.numeric(unlist(na.omit(df[RV$nump])))))
      final_msg = paste(final_msg, "La proportion est de", prop, "\n")
    })
    msg_stat(final_msg)
    final_msg
  })
  
  ########### Prevalence ############
  
  observeEvent(input$prev_btn, {
    df <- read_file()
    showModal(modalDialog(
      helpText("Choose in your data the columns of deaths count and total cases",
               "You can exclude one or multiple line before computing the result."),
      tags$h2('Please choose the column corresponding to cases :'),
      selectInput("cases", "Cases ", 
                  choices = colnames(df), selected = " "),
      numericInput("totpop", "Total population", 
                   "Choose the total population"),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to exclude",
                     choices = unlist(df[input$filter]),
                     selected = " ",
                     multiple = TRUE),
      footer=tagList(
        actionButton('submit_prevalence', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  observeEvent(input$submit_prevalence, {
    removeModal()
    RV$cas <- input$cases
    RV$tp <- input$totpop
    df <- read_file()
    if (!(is.null(input$row_choice))) {
      df <- df[!(df[,input$filter] %in% c(input$row_choice)),]
    }
    final_msg = msg_stat()
    output$text_stats <- renderText ({
      prev = sum(as.numeric(unlist(na.omit(df[RV$cas]))))/RV$tp
      binom = binom.test(sum(as.numeric(unlist(na.omit(df[RV$cas])))),RV$tp,p=0,alternative="less",conf.level=.95)
      pvalue = binom$p.value
      final_msg = paste(final_msg, "The prevalence is of", prev, "\n",
                        "p-value fo the binomial test (alternative less, alpha = 0.05)",
                        pvalue, "\n")
    })
    msg_stat(final_msg)
    final_msg
  })
  
  ############ DEATH RATE ############
  
  observeEvent(input$death_btn, {
    df <- read_file()
    showModal(modalDialog(
      helpText("Choose in your data the columns of deaths count and total cases",
               "You can exclude one or multiple line before computing the result."),
      tags$h2('Please choose the the data'),
      selectInput(inputId = "deaths",label = "Deaths",
                  choices = colnames(df)),
      selectInput(inputId = "totcases", label = "Total cases",
                  choices = colnames(df)),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to exclude",
                     choices = unlist(df[input$filter]),
                     selected = " ",
                     multiple = TRUE),
      footer=tagList(
        actionButton('submit_death_rate', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  
  
  observeEvent(input$submit_death_rate, {
    removeModal()
    RV$death <- input$deaths
    RV$tc <- input$totcases
    df <- read_file()
    if (!(is.null(input$row_choice))) {
      df <- df[!(df[,input$filter] %in% c(input$row_choice)),]
    }
    final_msg <- msg_stat()
    output$text_stats <- renderText ({
      d_rate = sum(as.numeric(unlist(na.omit(df[RV$death]))))/(sum(as.numeric(unlist(na.omit(df[RV$tc])))))
      final_msg = paste(final_msg, "Death rate : ", d_rate, "\n") 
    })
    msg_stat(final_msg)
    final_msg
  })
  
  #################### Incidence ################
  observeEvent(input$incidence, {
    df <- read_file()
    showModal(modalDialog(
      helpText("Data should have 3 columns with the dates for each individual",
               "Each line should be an individual"),
      tags$h2("Please choose your axis"),
      selectInput(inputId = "DO",label = "Date of origin",
                  choices = colnames(df)),
      selectInput(inputId = "DDN", label = "Last news date",
                  choices = colnames(df)),
      selectInput("DM", "Date start of disease",
                  choices = colnames(df)),
      selectInput("filter", "Filter",
                  choices = colnames(df)),
      selectizeInput("row_choice","Choose individuals to exclude",
                     choices = unlist(df[input$filter]),
                     selected = " ",
                     multiple = TRUE),
      
      footer = tagList(
        actionButton('submit_incidence', "Submit"),
        modalButton('cancel'))
    ))
  })
  
  observeEvent(input$submit_incidence, {
    removeModal()
    RV$DO <- input$DO
    RV$DM <- input$DM
    RV$DDN <- input$DDN
    df <- read_file()
    if (!(is.null(input$row_choice))) {
      df <- df[!(df[,input$filter] %in% c(input$row_choice)),]
    }
    final_msg = msg_stat()
    output$text_stats <- renderPrint ({
      df$DO = as.Date(df[RV$DO], "%d/%m/%Y")
      df$DDN = as.Date(df[RV$DDN], "%d/%m/%Y")
      df$DM = as.Date(df[RV$DM], "%d/%m/%Y")
      dd <- subset(df,(DMn>DOn|is.na(DM)))
      DFn <- pmin(dd$DDNn,dd$DMn,na.rm=TRUE)
      TP <- DFn-dd$DOn
      Nst <- sum(TP)
      Nnm <- nrow(subset(dd,DMn>DOn))
      result <- pois.exact(Nnm, Nst, conf.level = 0.95)
      result
    })
    
  })
  
  ######### Risk-Ratio ##########
  
  observeEvent(input$riskratio, {
    df <- read_file()
    showModal(modalDialog(
      helpText("Data should be a contingency table. ",
               "If you have a confusion factor in tour data you can specify it. ",
               "Ohterwise, just click submit"),
      
      tags$h4("Please choose your axis"),
      selectInput("confusion_factor", "Add Confusion Factor (Optionnal)",
                  choices = c(" ",colnames(df)), selected = " "),
      footer = tagList(
        actionButton('submit_riskratio', "Submit"),
        modalButton('cancel'))
    ))
  })
  
  observeEvent(input$submit_riskratio, {
    
    df <- read_file()
    df2 <- data.matrix(df)
    if (!(input$confusion_factor == " ")) {
      removeModal()
      output$text_stats <- renderPrint({
        splitd <- split(df, df[input$confusion_factor])
        test <- lapply(splitd, function(sd)riskratio.wald(df2))
        test
      })
      
    }
    else {
      removeModal()
      output$text_stats <- renderPrint ({
        result <- riskratio.wald(x = df)
        result
    })}
  })
  
  ############  OddRatio ###############
  
  observeEvent(input$oddratio, {
    df <- read_file()
    showModal(modalDialog(
      helpText("Data should be a contingency table. ",
               "If you have a confusion factor in tour data you can specify it. ",
               "Ohterwise, just click submit"),
      tags$h2("Please choose your data"),
      selectInput("confusion_factor", "Add Confusion Factor (Optionnal)",
                  choices = c(" ",colnames(df)), selected = " "),
      footer = tagList(
        actionButton('submit_oddratio', "Submit"),
        modalButton('cancel'))
    ))
  })
  
  observeEvent(input$submit_oddratio, {
    removeModal()
    df <- read_file()
    df2 <- data.matrix(df)
    if (!(input$confusion_factor == " ")) {
      output$text_stats <- renderPrint({
        splitd <- split(df, df[input$confusion_factor])
        test <- lapply(splitd, function(sd)oddsratio(df2))
        test
      })
      
    }
    else {
      output$text_stats <- renderPrint ({
        result <- oddsratio(x = df2)
        result
    })}
  })
  
  ############# Incidence rate ratio ############
  
  observeEvent(input$incidence_ratio, {
    df <- read_file()
    showModal(modalDialog(
      helpText("Input the number of inidivduals in each group"),
      tags$h2("Please choose your data"),
      numericInput(inputId = "x1",label = "Number of cases in the non-exposed group", value = 0),
      numericInput(inputId = "x2", label = "Number of cases in the exposed group", value = 0),
      numericInput("st1", "Number of person-time in the non-exposed group", value = 0),
      numericInput("st2", "Number of person-time in the exposed group", value = 0),
      
      footer = tagList(
        actionButton('submit_incidenceratio', "Submit"),
        modalButton('cancel'))
    ))
  })
  
  observeEvent(input$submit_incidenceratio, {
    removeModal()
    x1 = input$x1
    x2 = input$x2
    st1 = input$st1
    st2 = input$st2
    output$text_stats <- renderPrint ({
        result <- rateratio(c(x1,x2,st1,st2))
        result
      })
  })
      
    ######################################
  output$fileUploaded <- reactive({
    return(!is.null(read_file()))
  })
      
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  output$plot_stats <- renderPlot(
    if ((!(is.null(RV$X))) & (!(is.null(RV$Y)))) {
      df <- read_file()
      if (is.null(input$row_choice)){
        plot(x = unlist(df[RV$X]), y = unlist(df[RV$Y]), xlab = RV$X, ylab = RV$Y)
      }
      else {
        df2 <- df[df[,input$filter] %in% c(input$row_choice),]
        plot(unlist(df2[RV$X]), unlist(df2[RV$Y]), xlab = RV$X, ylab = RV$Y)
      }
    }
    
  )
  

  output$text_stats <- renderText ({})
  
}

shinyApp(ui = ui, server = server)
