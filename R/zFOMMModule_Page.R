#'@title FOMM module: main page
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@import pMineR
#'@import DiagrammeR
#'@import shinyjqui





server.FOMM<-function(input,output,session){
  #visualizzazione EventLog
  tab<-callModule(import_data_server,"uploadEL","EventLog")



  # reactiveValues: initializing data as null data frame
  data_reactive <- reactiveValues(
    EventLog=data.frame(),
    th = c(),  #threshold
    al = c(),  #autoloops
    FOMM = c()
  )


  #TAB EVENTLOG: data visualization of eventlog
  observeEvent(input$loadEL,{
    data_reactive$EventLog <- all.data[["EventLog"]]

    if(is_empty(data_reactive$EventLog)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Load your EventLog, then press 'Load Event Log'  button",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()
    }else if(length(which(colnames(all.data[[1]]) %in% c("ID","DATE_INI","EVENT")))<3){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "It is necessary to explicit which columns of the uploaded Event Log contain information about: ID, DATE and EVENT label ",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()

    }else if(is.na(as.Date(all.data[[1]]$DATE_INI[1], "%Y-%m-%d"))){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Please check the Date Format",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()
    }else{
      # Creating Dl obj e CFM obj
      ObjDL<<-dataLoader(verbose.mode = FALSE)
      ObjDL$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                            format.column.date = "%Y-%m-%d")



      #FOMM GRAPH PANEL
      removeTab(inputId = "tabs", target = "FOMM")
      insertTab(inputId = "tabs",
                tabPanel("FOMM",
                         titlePanel("Process Discovery: FOMM"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 3,
                                      p(h3("Parameter Setting")),
                                      tags$hr(),

                                      #parametro soglia
                                      fluidRow(
                                        column(7,
                                               numericInput("th", label = "Select Thresshold:", min = 0, max = 1, step = 0.01, value = 0),
                                               ),
                                        column(5,
                                               br(),
                                               br(),
                                               materialSwitch(
                                                 inputId = "al",
                                                 label = "Autoloops",
                                                 status = "default",
                                                 right = TRUE
                                               )
                                        )
                                      ),
                                      br(),
                                      br(),
                                      fluidRow(
                                        column(12,
                                               actionButton("KM","Survival Analysis",width = '100%')
                                               )
                                      )
                                    ),

                                    mainPanel(
                                      jqui_resizable(grVizOutput("CareFlowGraph"))
                                    )
                                  )
                           )
                         )

                ),
                target = "Loading EventLog",
                position = "after"
      )
    }


    observeEvent(input$th,{
      data_reactive$th<-input$th
    })

    observeEvent(input$al,{
      data_reactive$al<-input$al
    })


    fomm.graph<-reactive({
      param= list("threshold"=data_reactive$th, "considerAutoLoop"= data_reactive$al)
      FOMM<-firstOrderMarkovModel(parameters.list = param)
      FOMM$loadDataset(dataList = ObjDL$getData())
      FOMM$trainModel()
      data_reactive$FOMM<-FOMM
      fomm.plot<-FOMM$getModel(kindOfOutput = "grViz")
      return(fomm.plot)
    })

    output$CareFlowGraph<-renderGrViz({
      grViz(fomm.graph())
    })


    #################################################### SURVIVAL ANALYSIS TAB ##############################################################################

    observeEvent(input$KM,{
      removeTab(inputId = "tabs", target = "Survival Analysis")
      insertTab(inputId = "tabs",
                tabPanel("Survival Analysis",
                         titlePanel("Process Discovery: FOMM, Survival Analysis"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 3,
                                      p(h3("Parameter Setting")),
                                      tags$hr(),
                                      fluidRow(
                                        column(12,
                                               br()
                                               )
                                      ),

                                      #KAPLAN MAIER PARAM
                                      fluidRow(
                                        column(6,
                                               selectInput("event.from","From State: ", choices = unique(all.data[[1]]$EVENT))
                                               ),
                                        column(6,
                                               selectInput("event.to","To State: ", choices = unique(all.data[[1]]$EVENT), selected = unique(all.data[[1]]$EVENT)[2] )
                                               )
                                        )
                                      ),

                                    mainPanel(
                                      plotOutput("surv.curve")
                                    )
                                  )
                           )
                         )

                ),
                target = "FOMM",
                position = "after"
      )

    })


    surv<-reactive({
      FOMM<-data_reactive$FOMM
      KM <- FOMM$KaplanMeier(fromState = input$event.from,toState = input$event.to, UM = "days")
      return(plot( KM$KM, main="Covid_BEGIN -> Covid_END", xlab="days",ylab="p"))
    })

    output$surv.curve<-renderPlot(surv())

    ##################################################################################################################################################################





  })










}


