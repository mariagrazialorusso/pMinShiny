#'@title Careflow module: main page
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@import pMineR
#'@import DiagrammeR
#'@import shinyjqui







# library(shiny)
# library(shinythemes)
# library(dplyr)
# library(shinyWidgets)
# library(DT)
# library(rlang)
# library(shinybusy)
# library(pMineR)

# ui.careFlow<-fluidPage(
#
#     #Pagina Principale
#     navbarPage("pMining: CareFlow Mining", id="tabs",
#                tabPanel("Loading EventLog",
#                         titlePanel("EventLog Uploading"),
#                         br(),
#                         import_mod_ui("uploadEL","Upload EventLog file",FALSE),
#                         actionButton("loadEL","Load Event Log",width = '32%') ,
#                )
#     )
#   )





server.careFlow<-function(input,output,session){
  #visualizzazione EventLog
  tab<-callModule(import_data_server,"uploadEL","EventLog")


  # reactiveValues: initializing data as null data frame
  data_reactive <- reactiveValues(
    EventLog=data.frame(),
    depth= array(),
    max_depth=FALSE,
    support=array(),
    leaf=FALSE,
    median_time=FALSE
  )


  #TAB EVENTLOG: data visualization of eventlog
  observeEvent(input$loadEL,{
    data_reactive$EventLog <- all.data[["EventLog"]]
    # data_reactive$EventLog <- loadData("uploadEL.csv")
    if(is_empty(data_reactive$EventLog)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Load your EventLog, then press 'Load Event Log'  button",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()
    }else{
      # Creating Dl obj e CFM obj
      removeTab(inputId = "tabs", target = "CareFlowMiner")
      ObjDL<-dataLoader(verbose.mode = FALSE)
      ObjDL$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                            format.column.date = "%Y-%m-%d")
      ObjCFM<-careFlowMiner(verbose.mode = FALSE)
      ObjCFM$loadDataset(inputData = ObjDL$getData())


      #loading data uploaded in the upload section

      insertTab(inputId = "tabs",
                tabPanel("CareFlowMiner",
                         titlePanel("Process Discovery: CareFlowMiner"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 3,
                                      p(h3("Parameter Setting")),
                                      tags$hr(),

                                      #parametro profondità
                                      fluidRow(
                                        column(8,
                                               sliderInput("depth", label = "Select depth:", min = 1,
                                                           max = 10, value = 5)
                                        ),
                                        column(4,
                                               br(),
                                               br(),
                                               materialSwitch(
                                                 inputId = "max_depth",
                                                 label = "max depth",
                                                 status = "primary",
                                                 right = TRUE
                                               )
                                        )
                                      ),
                                      br(),
                                      tags$hr(),
                                      #parametro supporto
                                      fluidRow(
                                        column(12,
                                               sliderInput("support", label = "Select support value:", min = 1,
                                                           max = 100, value = 20)
                                        )
                                      ),
                                      br(),
                                      tags$hr(),
                                      #parametro duration
                                      fluidRow(
                                        column(12,
                                               materialSwitch(
                                                 inputId = "time",
                                                 label = "Show median Time",
                                                 status = "default",
                                                 right = TRUE)
                                        )
                                      ),
                                      #parametro foglie out
                                      fluidRow(
                                        column(12,
                                               materialSwitch(
                                                 inputId = "leaf",
                                                 label = "Show far leaf",
                                                 status = "default",
                                                 right = TRUE)
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

   observeEvent(input$depth, {
     data_reactive$depth<-input$depth
   })

   observeEvent(input$max_depth, {
     data_reactive$max_depth<-input$max_depth
   })

   observeEvent(input$support, {
     data_reactive$support<-input$support
   })

   observeEvent(input$time, {
     data_reactive$median_time<-input$time
   })

   observeEvent(input$leaf, {
     data_reactive$leaf<-input$leaf
   })

   #PLOT
   CFgraph<-reactive({
     if(data_reactive$max_depth){
       dp<-Inf
     }else{
       dp<-data_reactive$depth
     }
     cf.graph<-ObjCFM$plotCFGraph(depth = dp,  #PROFONDITA
                                  withPercentages = TRUE,
                                  relative.percentages = TRUE,
                                  show.far.leaf = data_reactive$leaf, #leaf
                                  show.median.time.from.root = data_reactive$median_time,#time
                                  abs.threshold = data_reactive$support, #support
                                  kindOfGraph = "dot",
                                  nodeShape = "square")$script
     return(cf.graph)
   })

   output$CareFlowGraph<-renderGrViz({
     grViz(CFgraph())
   })

  })
}


