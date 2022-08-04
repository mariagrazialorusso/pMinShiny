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

      if(is.factor(data_reactive$EventLog$EVENT)) { data_reactive$EventLog$EVENT <- as.character(data_reactive$EventLog$EVENT)  }
      ObjDL<<-dataLoader(verbose.mode = FALSE)
      ObjDL$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                            format.column.date = "%Y-%m-%d")



      ############################################################# FOMM GRAPH TAB #########################################################################
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

      ############################################################################# KAPLAN MEIER TAB ####################################################################

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
                                      # p(h3("Parameter Setting")),
                                      # tags$hr(),
                                      fluidRow(
                                        column(9,
                                               p(h3("Parameter Setting")),
                                        ),
                                        column(3,
                                               dropdownButton(
                                                 tags$h4(strong("Survival Analysis with Kaplan Meier")),

                                                 tags$h5("The cohort consists of patients who have experienced a certain state, which the user must make explicit in the", strong("\"from state\" field"), "and who have experienced a certain state of interest,
                                                 which must be made explicit in the", strong("\"to state\" field.")),

                                                 tags$h5("Through the", strong("\"censored at\" field") ," it will be possible to indicate which state the patients will have to transit
                                                 to in order to be considered censored."),


                                                 tags$h5("It is possible to apply filters on the population involved in the analysis.
                                                         Through the", strong("\"passing through\""), "and" , strong("\"passing not through\""),"fields, it is possible to indicate, respectively, which states must and must not have experienced by patients in order to be used for the analysis"),


                                                 circle = FALSE,
                                                 status = "info",
                                                 size = "xs",
                                                 icon = icon("fas fa-info"),
                                                 width = "300px",
                                                 right = TRUE,
                                                 tooltip = tooltipOptions(title = "Click to more info")
                                               )

                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               br()
                                        )
                                      ),



                                      #KAPLAN MAIER PARAM
                                      fluidRow(
                                        column(6,
                                               selectInput("event.from","From State: ", choices = unique(data_reactive$EventLog$EVENT))
                                        ),
                                        column(6,
                                               selectInput("event.to","To State: ",
                                                           choices = unique(data_reactive$EventLog$EVENT), selected = unique(data_reactive$EventLog$EVENT)[2] )
                                        )
                                      ),

                                      fluidRow(
                                        column(6,
                                               pickerInput(
                                                 inputId ="PDVat",
                                                 label = "Censored at:",
                                                 choices = unique(data_reactive$EventLog$EVENT),
                                                 multiple = TRUE,
                                                 options = list(
                                                   title = "select event")
                                               )
                                               # selectInput("PDVat","Censored at:",
                                               #             choices = NULL)
                                        ),
                                        column(6,
                                               selectInput("UM","Temporal Scale:",
                                                           choices = c("mins", "hours","days","weeks","months")),
                                               # materialSwitch(
                                               #   inputId = "filters",
                                               #   label = "add filters",
                                               #   status = "primary",
                                               #   right = TRUE)

                                        )
                                      ),

                                      fluidRow(
                                        column(12,
                                               p(h3("Add Filters")),
                                               )
                                      ),

                                      fluidRow(
                                        column(6,
                                               pickerInput(
                                                 inputId ="pass.thr",
                                                 label = "Passing Through",
                                                 choices = unique(data_reactive$EventLog$EVENT),
                                                 multiple = TRUE,
                                                 options = list(
                                                   title = "select event")
                                               )

                                        ),

                                        column(6,
                                               pickerInput(
                                                 inputId ="pass.not.thr",
                                                 label = "Passing not Through",
                                                 choices = unique(data_reactive$EventLog$EVENT),
                                                 multiple = TRUE,
                                                 options = list(
                                                   title = "select event")
                                               )
                                        )
                                      )

                                      # conditionalPanel(condition = "input.filters",
                                      #                  fluidRow(
                                      #                    column(6,
                                      #                           selectInput("pass.thr","Passing Through",
                                      #                                       choices = NULL)
                                      #                           ),
                                      #
                                      #                    column(6,
                                      #                           selectInput("pass.not.thr","Passing not Through",
                                      #                                       choices = NULL)
                                      #                           )
                                      #                    )
                                      #                  )



                                    ),

                                    mainPanel(
                                      fluidRow(
                                        column(11,
                                               ),
                                        column(1,
                                               dropdownButton(
                                                 grVizOutput("prev.fomm"),

                                                 circle = FALSE,
                                                 status = "primary",
                                                 size = "xs",
                                                 icon = icon("fas fa-info"),
                                                 width = "800px",
                                                 right = TRUE,
                                                 tags$div(style = "height: 80px;"),
                                                 tooltip = tooltipOptions(title = "Click to see fomm graph")
                                               )

                                        )
                                      ),
                                      fluidRow(
                                        plotOutput("surv.curve")
                                      )
                                    )
                                  )
                           )
                         )

                ),
                target = "FOMM",
                position = "after"
      )
     ###########################################################################################################################################################################



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



    output$prev.fomm<-renderGrViz({
      grViz(fomm.graph())
    })


    # observeEvent(input$event.from,{
    #   updateSelectInput(session = session,
    #                     inputId = "PDVat",
    #                     label = "Censored at:",
    #                     choices = unique(all.data[[1]]$EVENT)[!unique(all.data[[1]]$EVENT) %in% c(input$event.from)],
    #                     selected = NULL
    #
    #                     )
    #
    #   updateSelectInput(session = session,
    #                     inputId = "event.to",
    #                     label = "To State: ",
    #                     choices = unique(all.data[[1]]$EVENT)[!unique(all.data[[1]]$EVENT) %in% c(input$event.from)],
    #                     selected = NULL
    #
    #   )
    # })



    surv<-reactive({
      FOMM<-data_reactive$FOMM
      pass.th<-input$pass.thr
      pass.not.th<-input$pass.not.thr
      pdv<-input$PDVat
      if(pass.th=="" || is.null(pass.th)){

        pass.th<-c()
      }
      if(pass.not.th=="" || is.null(pass.not.th)){
        pass.not.th<-c()
      }

      if(pdv=="" || is.null(pdv)){
        pdv<-c()
      }




      KM <- KaplanMeier(fromState = input$event.from,
                        toState = input$event.to,
                        ObjDL,
                        passingThrough=pass.th,
                        passingNotThrough=pass.not.th,
                        PDVAt=pdv,
                        UM=input$UM)
      if(is.null(KM)){
        to_ret<-NULL
      }else if(input$event.from == input$event.to){
        to_ret<-NULL
      }else{
        to_ret<-plot(KM$KM, main=paste0(input$event.from, "->", input$event.to),
                     xlab=input$UM,
                     ylab="p",
                     mark.time=TRUE)
      }

      return(to_ret)


    })

    output$surv.curve<-renderPlot({
      if(is.null(surv())){
        validate("Error: please check ")
      }else{
        surv()
      }
    })

    ##################################################################################################################################################################





  })










}


