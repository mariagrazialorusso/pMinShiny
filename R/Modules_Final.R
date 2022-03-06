#'@title module for data loading
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT



# library(shiny)
# library(dplyr)
# library(shinyWidgets)
# library(DT)

import_mod_ui<- function(id, tit,flag){
  #flag=TRUE-->Merge Module, it is necessary to select a key for the uploaded data set
  #flag=FALSE-->Visualization module
  ns<-NS(id)

  sidebarLayout(
    sidebarPanel(
      fileInput(ns("file"),
                tags$span(style="color: black;",tit),
                multiple = FALSE,
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      #----
      tags$hr(),
      # Input: Checkbox if file has header
      checkboxInput(ns("header"), tags$span(style="color: black;","Header"), TRUE),
      # Input: Select separator
      radioButtons(ns("sep"), tags$span(style="color: black;","Separator"),
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      # Input: Select quotes
      radioButtons(ns("quote"), tags$span(style="color: black;","Quote"),
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      if(flag){
        pickerInput(ns("key"),
                    label = "Select the key of the Table",
                    choices = NULL
        )
      }
      # #Input: select the key of the table
      # pickerInput(ns("key"),
      #             label = "Select the key of the Table",
      #             choices = NULL
      #             )
    ),

    mainPanel(
      fluidRow(
        column(12,
               DT::dataTableOutput(ns("table"))
        )
      )

    )
  )
}



import_data_server<- function(input,
                              output,
                              session,
                              name){
  #creating variable Mydata which contains dataframe uploaded by fileInput
  myData <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      d <- data.frame()
    } else {
      d <- read.csv(inFile$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote)
      EventLog1<<-d
      # saveData(d,name)
      all.data[[name]]<<-d
    }
    d
  })

  observeEvent(input$file,{
    updatePickerInput(
      session = session,
      inputId = "key",
      label =  "Select the key of the Table",
      choices = colnames(myData()),
      selected = NULL
    )
  })

  observeEvent(input$key,{
    all.key[[name]]<<-input$key
  })


  #show uploaded data
  output$table <- DT::renderDataTable({
    req(input$file)
    tryCatch(
      {
        myData()
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
  },
  selection = list(mode = 'single', target = 'column'),server = TRUE)

  observeEvent(input$table_columns_selected,{
    updatePickerInput(
      session = session,
      inputId = "key",
      label =  "Select the key of the Table",
      choices =colnames(myData()),
      selected =colnames(myData())[input$table_columns_selected]
    )
  })

}


























