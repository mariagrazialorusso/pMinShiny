#'@title module for data loading
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT


import_mod_ui<- function(id, tit,flag,col_setting=FALSE){
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

      conditionalPanel(condition = "output.showswich =='yes'", ns= ns,
        tags$hr(),
        materialSwitch(
          inputId = ns("date.format"),
          label = "Select Date Format (default: yyyy-mm-dd)",
          status = "primary",
          right = TRUE
        ),
      ),

      conditionalPanel(condition = 'output.showdatef == "yes"', ns = ns ,
                       fluidRow(
                         column(6,
                                selectInput(ns("data.or"),"Data orientation",
                                            choices= c("day-month-year","month-day-year","year-month-day"),
                                            selected= "day-month-year",
                                )),
                         column(6,
                                selectInput(ns("data.sep"),"Separator",
                                            choices= c("-","/"),
                                            selected= "-",
                                )
                                )

                       ),
                       fluidRow(
                         column(4,
                                selectInput(ns("month"),"month format",
                                            choices= c("01-12","Jan","January"),
                                            selected= "01-12"
                                )
                                ),
                         column(4,
                                selectInput(ns("year"),"year format",
                                            choices= c("two digit (07)","four digit (2007)"),
                                            selected= "four digit (2007)"
                                )
                                ),
                         column(4,
                                selectInput(ns("day"),"day format",
                                            choices= c("01-31"),
                                            selected= "01-31"
                                )
                                )
                       ),
                       fluidRow(

                           column(4,
                                  p(strong("  Selected Date format:"))
                                  ),
                           column(4,
                                  textOutput(ns("ex.date"))
                                  ),
                           column(4,
                                  actionButton(ns("save.date"), "Save format")
                                  )


                         # mainPanel(
                         #   uiOutput(ns("ex.date"))
                         # )

                       )

      ),


      if(flag){
        pickerInput(ns("key"),
                    label = "Select the key of the Table",
                    choices = NULL
        )
      }
    ),

    if(!col_setting){
      mainPanel(
        fluidRow(
          column(12,
                 DT::dataTableOutput(ns("table"))
          )
        )
      )
    }else{
      mainPanel(
        uiOutput(ns("col_names"))
      )
    }
  )
}



import_data_server<- function(input,
                              output,
                              session,
                              name,
                              flag= FALSE){

  data_re<-reactiveValues(
    id = c(),
    date = c(),
    event = c()

  )

  ns <- session$ns
  #creating variable Mydata which contains dataframe uploaded by fileInput
  myData <- reactive({
    if(flag){
      inFile<-NULL
    }else{
      inFile <- input$file
    }
    if (is.null(inFile)) {
      d <- data.frame()
    } else {
      d <- read.csv(inFile$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote)
      EventLog1<<-d
      # colnames(d)<-input$col_names
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

  output$col_names<-renderUI({
    fluidPage(
      fluidRow(
        column(12,
               DT::dataTableOutput(ns("table"))
        )
      ),
      fluidRow(
       column(12,
              tags$hr(),
              p(h4("Variable Mapping"))
              )

      ),
      fluidRow(
          column(4,
                 pickerInput(
                   inputId =ns("ID"),
                   label = "ID",
                   choices = colnames(myData()),
                   multiple = FALSE,
                   options = list(
                     title = "select ID")
                 )
          ),
          column(4,
                 pickerInput(
                   inputId =ns("date"),
                   label = "DATE",
                   choices = colnames(myData()),
                   multiple = FALSE,
                   options = list(
                     title = "select DATE")
                 )
          ),
          column(4,
                 pickerInput(
                   inputId =ns("event"),
                   label = "EVENT",
                   choices = colnames(myData()),
                   multiple = FALSE,
                   options = list(
                     title = "select Event")
                 )
          )
        ),

        fluidRow(
          column(4,
                 actionButton(ns("reload"), label= "Reset"),
                 actionButton(ns("save"), label= "Save")
                 # actionBttn(
                 #   inputId = ns("reload"),
                 #   label = NULL,
                 #   style = "minimal",
                 #   color = "primary",
                 #   icon = icon("rotate-left")
                 # ),
                 # actionBttn(
                 #   inputId = ns("save"),
                 #   label = "SAVE",
                 #   style = "minimal",
                 #   color = "primary"
                 # )
                 # actionButton(ns("reload"),"RELOAD"),
                 # actionButton(ns("save_col"),"SAVE")
          ),
          # column(3,
          #        actionButton(ns("save_col"),"SAVE")
          # )
        ),
      fluidRow(
        tags$hr()
      )



      # fluidRow(
      #   bucket_list(
      #     header = "",
      #     group_name = "col_names",
      #     orientation = "horizontal",
      #     add_rank_list(
      #       text = "Event Log var names",
      #       labels = colnames(myData()),
      #       input_id = "colnames"
      #     ),
      #     add_rank_list(
      #       text = "ID",
      #       labels = NULL,
      #       input_id = ns("id")
      #     ),
      #     add_rank_list(
      #       text = "DATE",
      #       labels = NULL,
      #       input_id = ns("data_ini")
      #     ),
      #     add_rank_list(
      #       text = "EVENT",
      #       labels = NULL,
      #       input_id = ns("event")
      #     ),
      #
      #   )
      # )
    )
  })


  observeEvent(input$reload,{
    data_re$id<-c()
    data_re$data<-c()
    data_re$event<-c()
    updatePickerInput(
      session = session,
      inputId ="event",
      label = "EVENT",
      choices = colnames(myData()),
      options = list(
        title = "select Event")
    )
    updatePickerInput(
      session = session,
      inputId ="ID",
      label = "ID",
      choices = colnames(myData()),
      options = list(
        title = "select ID")
    )
    updatePickerInput(
      session = session,
      inputId ="date",
      label = "DATE",
      choices = colnames(myData())[!colnames(myData()) %in% c(data_re$date, data_re$id)],
      options = list(
        title = "select DATE")
    )


  })


  observeEvent(input$save,{
    if(data_re$id=="" || data_re$date=="" ||data_re$event==""  ){
      sendSweetAlert(
        session = session,
        title = "Error in: Variable Mapping",
        text = "Please enter all fiels",
        type = "primary"
      )
    }else{
      id.ind<-which(colnames(myData())==data_re$id)
      date.ind<-which(colnames(myData())==data_re$date)
      event.ind<-which(colnames(myData())==data_re$event)

      colnames(all.data[[1]])[id.ind]<<-"ID"
      colnames(all.data[[1]])[date.ind]<<-"DATE_INI"
      colnames(all.data[[1]])[event.ind]<<-"EVENT"
    }


  })

 ######################################################### OBSERVE ID ########################################################
  observeEvent(input$ID,{

    if(!is.null(input$file)){
      data_re$id<-input$ID

      if(is.null(data_re$event) || data_re$event==""){
        updatePickerInput(
          session = session,
          inputId ="event",
          label = "EVENT",
          choices = colnames(myData())[!colnames(myData()) %in% c(data_re$date, data_re$id)],
          options = list(
            title = "select Event")
        )
      }

      if(is.null(data_re$date) || data_re$date=="" ){
        updatePickerInput(
          session = session,
          inputId ="date",
          label = "DATE",
          choices = colnames(myData())[!colnames(myData()) %in% c(data_re$id, data_re$event)],
          options = list(
            title = "select Date")
        )
      }
    }
  })

  ######################################################### OBSERVE DATA ########################################################
  observeEvent(input$date,{
    data_re$date<-input$date
    if(data_re$date== ""){

      rv.showswich$show.showswich <- FALSE
    }else{
      rv.showswich$show.showswich <- TRUE
    }


    if(!is.null(input$file)){
      # print(colnames(myData()))
      #
      # ind<-which(colnames(myData())==input$date)
      # if(!is.null(input$date) & identical(ind,integer(0))){
      #   print("aaaaa")
      # }
      # colnames(all.data[[1]])[ind]<<-"DATE_INI"
      # print(ind)

      if(is.null(data_re$event) || data_re$event==""){
        updatePickerInput(
          session = session,
          inputId ="event",
          label = "EVENT",
          choices = colnames(myData())[!colnames(myData()) %in% c(data_re$date, data_re$id)],
          options = list(
            title = "select Event")
        )
      }
      else if(input$event==data_re$date){
        updatePickerInput(
          session = session,
          inputId ="date",
          label = "DATE",
          choices = colnames(myData())[!colnames(myData()) %in% c(data_re$id, data_re$date)],
          options = list(
            title = "select Date")
        )
      }

      if(is.null(data_re$id)|| data_re$id==""){
        updatePickerInput(
          session = session,
          inputId ="ID",
          label = "ID",
          choices = colnames(myData())[!colnames(myData()) %in% c(data_re$date, data_re$event)],
          options = list(
            title = "select ID")
        )
      }
    }
  })


  ######################################################### OBSERVE EVENT ########################################################
  observeEvent(input$event,{
    data_re$event<-input$event

    if(!is.null(input$file)){


      if(is.null(data_re$date)|| data_re$date==""){
        updatePickerInput(
          session = session,
          inputId ="date",
          label = "DATE",
          choices = colnames(myData())[!colnames(myData()) %in% c(data_re$id, data_re$event)],
          options = list(
            title = "select Date")
        )
      }


      if(is.null(data_re$id)|| data_re$id==""){
        updatePickerInput(
          session = session,
          inputId ="ID",
          label = "ID",
          choices = colnames(myData())[!colnames(myData()) %in% c(data_re$date, data_re$event)],
          options = list(
            title = "select ID")
        )
      }
    }
  })





  # observeEvent(input$id,{
  #   if(!is.null(input$file)){
  #     ind<-which(colnames(all.data[[1]])==input$id[1])
  #     if(!is.null(input$id) & identical(ind,integer(0))){
  #       print(colnames(myData()))
  #
  #       colnames(all.data[[1]])<<-colnames(myData())
  #
  #     }
  #
  #     colnames(all.data[[1]])[which(colnames(all.data[[1]])==input$id[1])]<<-"ID"
  #     print(colnames(myData()))
  #
  #   }
  #
  # })
  #
  # observeEvent(input$data_ini,{
  #   if(!is.null(input$file)){
  #     ind<-which(colnames(all.data[[1]])==input$data_ini[1])
  #     if(!is.null(input$data_ini) & identical(ind,integer(0))){
  #       colnames(all.data[[1]])<<-colnames(myData())
  #
  #       print(colnames(myData()))
  #
  #     }
  #
  #     colnames(all.data[[1]])[which(colnames(all.data[[1]])==input$data_ini[1])]<<-"DATE_INI"
  #
  #
  #     print( colnames(all.data[[1]]))
  #     rv.showswich$show.showswich <- !(rv.showswich$show.showswich)
  #
  #   }
  # })
  #
  # observeEvent(input$event,{
  #   if(!is.null(input$file)){
  #     ind<-which(colnames(all.data[[1]])==input$event[1])
  #     if(!is.null(input$event) & identical(ind,integer(0))){
  #       colnames(all.data[[1]])<<-colnames(myData())
  #     }
  #     colnames(all.data[[1]])[which(colnames(all.data[[1]])==input$event[1])]<<-"EVENT"
  #     print(colnames(all.data[[1]]))
  #
  #   # colnames(all.data[[1]])[which(colnames(all.data[[1]])==input$event[1])]<<-"EVENT"
  #   }
  # })


  #############################    CONDITION SWITCH    #################################
  rv.showswich <- reactiveValues(show.showswich = FALSE)

  # other missing code in observeEvent(input$data_ini)

  output$showswich <- renderText({
    if(rv.showswich$show.showswich){
      "yes"
    } else{
      "no"
    }
  })

  outputOptions(output, "showswich", suspendWhenHidden = FALSE)
  ########################################################################################


  ############################### CONDITION DATE FORMAT #################################
  rv.showdatef <- reactiveValues(show.showdatef = FALSE)

  observeEvent(input$date.format, ({
    rv.showdatef$show.showdatef <- !(rv.showdatef$show.showdatef)
  }))

  output$showdatef <- renderText({
    if(!rv.showdatef$show.showdatef & rv.showswich$show.showswich){
      "yes"
    } else{
      "no"
    }
  })

  outputOptions(output, "showdatef", suspendWhenHidden = FALSE)
  ######################################################################################


  date<-reactive({
    var<-input$data.or
    a<-"11-10-2020"
    switch (var,
            "day-month-year"= {
              if(input$year=="two digit (07)"){
                switch (input$month,
                        "01-12"  = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d-%m-%y")
                            new<-"%d-%m-%y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d/%m/%y")
                            new<-"%d/%m/%y"
                          }
                        },
                        "Jan"    = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d-%b-%y")
                            new<-"%d-%b-%y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d/%b/%y")
                            new<-"%d/%b/%y"
                          }

                        },
                        "January"= {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d-%B-%y")
                            new<-"%d-%B-%y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d/%B/%y")
                            new<-"%d/%B/%y"
                          }
                        }

                )
              }else{
                switch (input$month,
                        "01-12"  = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d-%m-%Y")
                            new<-"%d-%m-%Y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d/%m/%Y")
                            new<-"%d/%m/%Y"
                          }
                        },
                        "Jan"    = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d-%b-%Y")
                            new<-"%d-%b-%Y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d/%b/%Y")
                            new<- "%d/%b/%Y"
                          }

                        },
                        "January"= {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d-%B-%Y")
                            new<-"%d-%B-%Y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%d/%B/%Y")
                            new<-"%d/%B/%Y"
                          }
                        }

                )

              }


            },

            "month-day-year" = {

              if(input$year=="two digit (07)"){
                switch (input$month,
                        "01-12"  = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%m-%d-%y")
                            new<-"%m-%d-%y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%m/%d/%y")
                            new<-"%m/%d/%y"
                          }
                        },
                        "Jan"    = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%b-%d-%y")
                            new<-"%b-%d-%y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%b/%d/%y")
                            new<-"%b/%d/%y"
                          }

                        },
                        "January"= {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%B-%d-%y")
                            new<-"%B-%d-%y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%B/%d/%y")
                            new<-"%B/%d/%y"
                          }
                        }

                )
              }else{
                switch (input$month,
                        "01-12"  = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%m-%d-%Y")
                            new<-"%m-%d-%Y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%m/%d/%Y")
                            new<-"%m/%d/%Y"
                          }
                        },
                        "Jan"    = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%b-%d-%Y")
                            new<-"%b-%d-%Y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%b/%d/%Y")
                            new<-"%b/%d/%Y"
                          }

                        },
                        "January"= {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%B-%d-%Y")
                            new<-"%B-%d-%Y"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%B/%d/%Y")
                            new<-"%B/%d/%Y"
                          }
                        }

                )

              }




            },

            "year-month-day" ={
              if(input$year=="two digit (07)"){
                switch (input$month,
                        "01-12"  = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%y-%m-%d")
                            new<-"%y-%m-%d"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%y/%n/%d")
                            new<-"%y/%n/%d"
                          }
                        },
                        "Jan"    = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%y-%b-%d")
                            new<- "%y-%b-%d"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%y/%b/%d")
                            new<-"%y/%b/%d"
                          }

                        },
                        "January"= {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%y-%B-%d")
                            new<-"%y-%B-%d"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%y/%B/%d")
                            new<-"%y/%B/%d"
                          }
                        }

                )
              }else{
                switch (input$month,
                        "01-12"  = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%Y-%m-%d")
                            new<-"%Y-%m-%d"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%Y/%m/%d")
                            new<-"%Y/%m/%d"
                          }
                        },
                        "Jan"    = {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%Y-%b-%d")
                            new<-"%Y-%b-%d"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%Y/%b/%d")
                            new<-"%Y/%b/%d"
                          }

                        },
                        "January"= {
                          if(input$data.sep=="-"){
                            ex<- format(as.Date(a,"%d-%m-%y"), "%Y-%B-%d")
                            new<-"%Y-%B-%d"
                          }else{
                            ex<- format(as.Date(a,"%d-%m-%y"), "%Y/%B/%d")
                            new<-"%Y/%B/%d"
                          }
                        }

                )

              }

            }

    )
    return(c(ex,new))
  })

  output$ex.date<-renderText(date()[1])

  observeEvent(input$save.date,{


    if(colnames(all.data[[1]])[which(colnames(myData())==data_re$date)]!="DATE_INI"){
      sendSweetAlert(
        session = session,
        title = "Error in Variable Mapping:",
        text = "Please first save the changes in the var mapping section, then proceed to the Date Format Setting",
        type = "primary"
      )
    }else{
      if(!is.na(as.Date(format(as.Date(all.data[[1]][,"DATE_INI"],date()[2]), "%Y-%m-%d")[1], "%Y-%m-%d"))){
        all.data[[1]][,"DATE_INI"]<<- format(as.Date(all.data[[1]][,"DATE_INI"],date()[2]), "%Y-%m-%d")
      }else{
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Check Date Format",
          type = "primary"
        )
      }
    }
  })




}


























