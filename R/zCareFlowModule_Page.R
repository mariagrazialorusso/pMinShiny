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
      ObjDL<<-dataLoader(verbose.mode = FALSE)
      ObjDL$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                            format.column.date = "%Y-%m-%d")
      ObjCFM<<-careFlowMiner(verbose.mode = FALSE)
      ObjCFM$loadDataset(inputData = ObjDL$getData())


      #CARE FLOW PANEL
      removeTab(inputId = "tabs", target = "CareFlowMiner")
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
                                        column(7,
                                               numericInput("depth", label = "Select depth:", value = 5),

                                        ),
                                        column(5,
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
                                               numericInput("support", label ="Select support value:", value = 10),

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
                                      ),

                                      fluidRow(
                                        column(12,
                                               actionButton("EN_EL","Enriched EL Analysis",width = '100%')
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

      #PREDICTIVE PANEL
      removeTab(inputId = "tabs", target = "")
      insertTab(inputId = "tabs",
                tabPanel("Predictive CareFlowMiner",
                         titlePanel("Process Discovery: CareFlowMiner - Predictive Model"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 3,
                                      p(h3("Parameter Setting")),
                                      tags$hr(),
                                      fluidRow(
                                        column(7,
                                               numericInput("depth.pred", label = "Select depth:", value = 5),

                                        ),
                                        column(5,
                                               br(),
                                               br(),
                                               materialSwitch(
                                                 inputId = "max_depth.pred",
                                                 label = "max depth",
                                                 status = "primary",
                                                 right = TRUE
                                               )
                                        )
                                      ),

                                      tags$hr(),
                                      #parametro supporto
                                      fluidRow(
                                        column(12,
                                               numericInput("support.pred", label ="Select support value:", value = 10),

                                        )
                                      ),
                                      #param outcome to predict
                                      tags$hr(),

                                      fluidRow(
                                        column(12,
                                               shiny::selectInput("pred.outcome",
                                                                  label = "Select the outcome to predict",
                                                                  choices = unique(data_reactive$EventLog["EVENT"])
                                                                  )
                                        )
                                      ),

                                      #param colori
                                      p(h5("It is possible to highlight with different colors events
                                           that may be more interesting. (Max 7 events")),
                                      fluidRow(
                                        column(12,
                                          shiny::selectInput("pred.outcome.col",
                                                             label = "Select event to highlight",
                                                             choices = unique(data_reactive$EventLog["EVENT"]),
                                                             selected = NULL,
                                                             multiple = TRUE
                                          )
                                        )
                                      )

                                    ),

                                    mainPanel(
                                      jqui_resizable(grVizOutput("CareFlowGraph.pred"))

                                    )
                                  )
                           )
                         )

                ),
                target = "CareFlowMiner",
                position = "after"
      )
    }


   observeEvent(input$depth, {
     if(is.na(input$depth)){
       data_reactive$depth<-1
     }else{
       data_reactive$depth<-input$depth
     }

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

   #PLOT CAREFLOW CLASSICO
   CFgraph<-reactive({
     if(data_reactive$max_depth){
       dp<-Inf
     }else{
       dp<-data_reactive$depth
     }

     if(data_reactive$median_time){
       cf.graph<-ObjCFM$plotCFGraph(depth = dp,  #PROFONDITA
                                    abs.threshold = data_reactive$support, #support
                                    kindOfGraph = "dot",
                                    nodeShape = "square")
       len<-length(cf.graph$arr.nodi)
       id.nodi<-array()
       for (i in c(1:len)) {
         id.nodi[i]<-strsplit(cf.graph$arr.nodi[i],"'")[[1]][2]
       }

       id.nodi<-as.numeric(id.nodi[2:length(id.nodi)])

       lst.nodi<-ObjCFM$getDataStructure()$lst.nodi
       loadedDataset<-ObjDL$getData()
       median.time<-array()
       for (i in c(1:length(id.nodi))) {
         son<-as.character(id.nodi[i])
         tmp.tempi<-unlist(lapply(lst.nodi[[son]]$IPP, function(tmpIPP)
         { loadedDataset$pat.process[[tmpIPP]][lst.nodi[[son]]$depth,"pMineR.deltaDate"] }))
         if( length(tmp.tempi) > 0) {
           tmp.tempi <- as.numeric(unlist(lapply(tmp.tempi,function(x){  format((x/(24*60)),digits=3) })))
           med<-median(tmp.tempi)
         }else{
           tmp.tempi<-NA
           med<-NA
         }
         median.time[i]<-med
       }

       time.unique<-unique(median.time[order(median.time)])
       cut.off<-floor(length(time.unique)/4)
       col.threshold<-c(time.unique[cut.off],time.unique[cut.off*2],time.unique[cut.off*3])
     }else{
       col.threshold<-c()
     }
     cf.graph<-ObjCFM$plotCFGraph(depth = dp,  #PROFONDITA
                                  withPercentages = TRUE,
                                  relative.percentages = TRUE,
                                  show.far.leaf = data_reactive$leaf, #leaf
                                  show.median.time.from.root = data_reactive$median_time,#time
                                  heatmap.based.on.median.time = col.threshold,
                                  abs.threshold = data_reactive$support, #support
                                  kindOfGraph = "dot",
                                  nodeShape = "square")$script
     return(cf.graph)
   })

   output$CareFlowGraph<-renderGrViz({
     grViz(CFgraph())
   })


   #PLOT CAREFLOW PREDITTIVO
   CFgraph.pred<-reactive({
     if(input$max_depth.pred){
       dp<-Inf
     }else{
       dp<-input$depth.pred
     }

     shades<-c("Red","LightGoldenrodYellow","Lavender","LightCyan","LightSalmon","SandyBrown","	LightYellow","LightGreen")


     if(is.null(input$pred.outcome.col)){
       sub.shades<-shades[1]
       names(sub.shades)<-input$pred.outcome

     }else{
       len<-length(input$pred.outcome.col)+1
       sub.shades<-shades[1:len]
       names<-c(input$pred.outcome,input$pred.outcome.col)
       names(sub.shades)<-names
     }


     graph<- ObjCFM$plotCFGraph(depth = dp,
                                 predictive.model = TRUE,
                                 predictive.model.outcome = input$pred.outcome,
                                 arr.States.color = sub.shades,
                                 abs.threshold = input$support.pred,
                                 kindOfGraph = "dot",
                                 nodeShape = "square")$script



     # graph<-cf_pred(ObjCFM,
     #         max_depth= dp,
     #         input$support.pred,
     #         outcome= input$pred.outcome,
     #         col.pred= input$pred.outcome.col)

     return(graph)
   })

   output$CareFlowGraph.pred<-renderGrViz({
     grViz(CFgraph.pred())
   })

  })

  #STRATIFIED CF PANEL
  observeEvent(input$EN_EL,{
    removeTab(inputId = "tabs", target = "Stratified CareFlowMiner")
    insertTab(inputId = "tabs",
              tabPanel("Stratified CareFlowMiner",
                titlePanel("Stratification of the CareFlow Miner"),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    p(h5("A stratification variable can be entered in this section.
                    In this way it is possible to analyze whether the two groups into which the total population is divided,
                    present significant differences in terms of the pathway calculated by the Care Flow Miner")),
                    br(),
                    p(h5("Please note that in this section are used the depth and support parameters entered in the previous panel")),
                    tags$hr(),

                    fluidRow(
                      selectInput("strat.var", label = "Select variable for the stratification:",
                                  choices = colnames(data_reactive$EventLog)[5:length(data_reactive$EventLog)])
                    ),

                    p(h5("Before proceeding with the setting of the stratification parameters,
                         it is important to specify whether the chosen attribute is numeric or categorical")),
                    fluidRow(
                     selectInput("strat.var.type", label ="",
                                 choices = c("Categorical","Numeric"),
                                 selected = NULL
                                 )

                    ),
                    fluidRow(
                      column(6,
                             selectInput("strat.value1", label = "Select possible value fot the selected var:",
                                         choices = NULL,
                                         multiple = FALSE
                             )
                             ),
                      column(6,
                             selectInput("strat.value2", label = "Select possible value fot the selected var:",
                                         choices = NULL,
                                         multiple = TRUE
                             ),
                             )
                    ),
                    tags$hr(),
                    p(h5("Switch to see if the node reach times are different between the two courts")),
                    fluidRow(
                      materialSwitch(
                        inputId = "strat.time",
                        label = "",
                        status = "default",
                        right = TRUE)
                    ),
                    tags$hr(),
                    p(h5("Switch to see if there is a difference in terms of hit (number of patients per node)among the two courts,
                         between those who would then reach the Event entred in the", strong("Future State input"))),
                    fluidRow(
                      column(3,
                             materialSwitch(
                               inputId = "perc.end",
                               label = "",
                               status = "default",
                               right = TRUE)
                             ),
                      column(9,
                             selectInput("final.state", label = "Future State:", choices = unique(data_reactive$EventLog["EVENT"]))
                             )
                    )



                  ),
                  mainPanel(
                    jqui_resizable(grVizOutput("CF.strat"))
                  )
                )
              ),
              target = "Predictive CareFlowMiner",
              position = "after"
    )

  })

  observeEvent(input$strat.var,{
    shiny::updateSelectInput(
      inputId = "strat.value1",
      label = "Select possible value fot the selected var:",
      choices = unique(data_reactive$EventLog[input$strat.var])
    )
    shiny::updateSelectInput(
      inputId = "strat.value2",
      label = "Select possible value fot the selected var:",
      choices = unique(data_reactive$EventLog[input$strat.var])
    )
  })

  observeEvent(input$strat.value1,{
    shiny::updateSelectInput(
      inputId = "strat.value2",
      label = "Select possible value fot the selected var:",
      choices = unique(data_reactive$EventLog[input$strat.var])[!unique(data_reactive$EventLog[input$strat.var]) %in% input$strat.value1]
    )
  })


  observeEvent(input$strat.var.type,{
    if(input$strat.var.type=="Categorical"){
      shiny::updateSelectInput(
        inputId = "strat.value",
        label = "Select possible value fot the selected var:",
        choices = unique(data_reactive$EventLog[input$strat.var])

      )
    }
    else{
      shiny::updateSelectInput(
        inputId = "strat.value",
        label = "Select possible value fot the selected var:",
        choices = c("only for categorical var")
      )
    }
  })

  CF.strat.plot<-reactive({
    if(input$strat.var.type=="Categorical"){
      if(length(input$strat.value2)>1){
        ObjDL.out<-ObjDL$getData()
        tmp.csv <- ObjDL.out$original.CSV
        #MINORE -> 0
        tmp.csv[which(objDL.out$original.CSV[,input$strat.var] %in% input$strat.value2),input$strat.var]<-0
        tmp.csv[which(objDL.out$original.CSV[,input$strat.var]%in% input$strat.value2),input$strat.var]<-1
        #MAGGIORE = ->1
        tmp.DL <- dataLoader(verbose.mode = FALSE)
        tmp.DL$load.data.frame(mydata = tmp.csv,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",format.column.date = "%d/%m/%Y")

        tmp.objCFM <- careFlowMiner()
        tmp.objCFM$loadDataset(inputData = tmp.DL$getData() )
        script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
                                             stratificationValues = c(0,1),
                                             depth= data_reactive$depth,
                                             abs.threshold = data_reactive$support,
                                             checkDurationFromRoot = input$strat.time,
                                             hitsMeansReachAGivenFinalState = input$perc.end,
                                             finalStateForHits = input$final.state ,
                                             kindOfGraph = "dot",
                                             nodeShape = "square")$script

      }else{
        script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
                                             stratificationValues = c(input$strat.value1,input$strat.value2),
                                             depth= data_reactive$depth,
                                             abs.threshold = data_reactive$support,
                                             checkDurationFromRoot = input$strat.time,
                                             hitsMeansReachAGivenFinalState = input$perc.end,
                                             finalStateForHits = input$final.state ,
                                             kindOfGraph = "dot",
                                             nodeShape = "square")$script
      }

    }else{
      mediana <- median(as.numeric(data_reactive$EventLog[,input$strat.var]),na.rm = T)

      ObjDL.out<-ObjDL$getData()
      tmp.csv <- ObjDL.out$original.CSV
      #MINORE -> 0
      tmp.csv[which(objDL.out$original.CSV[,input$strat.var]<mediana),input$strat.var]<-0
      tmp.csv[which(objDL.out$original.CSV[,input$strat.var]<=mediana),input$strat.var]<-1
      #MAGGIORE = ->1
      tmp.DL <- dataLoader(verbose.mode = FALSE)
      tmp.DL$load.data.frame(mydata = tmp.csv,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",format.column.date = "%d/%m/%Y")

      tmp.objCFM <- careFlowMiner()
      tmp.objCFM$loadDataset(inputData = tmp.DL$getData() )
      script<-tmp.objCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
                                           stratificationValues = c(0,1),
                                           depth= data_reactive$depth,
                                           abs.threshold = data_reactive$support,
                                           checkDurationFromRoot = input$strat.time,
                                           hitsMeansReachAGivenFinalState = input$perc.end,
                                           finalStateForHits = input$final.state ,
                                           kindOfGraph = "dot",
                                           nodeShape = "square")$script

    }
      # script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
      #                              stratificationValues = input$strat.value,
      #                              depth= data_reactive$depth,
      #                              abs.threshold = data_reactive$support,
      #                              checkDurationFromRoot = input$strat.time,
      #                              hitsMeansReachAGivenFinalState = input$perc.end,
      #                              finalStateForHits = input$final.state ,
      #                              kindOfGraph = "dot",
      #                              nodeShape = "square")$script

      return(script)

  })

  output$CF.strat<-renderGrViz({
    grViz(CF.strat.plot())
  })


}


