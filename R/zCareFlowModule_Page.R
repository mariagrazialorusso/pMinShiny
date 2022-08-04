#'@title Careflow module: main page
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@import pMineR
#'@import DiagrammeR
#'@import shinyjqui
#'@import survival





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
    median_time=FALSE,
    node.list = list(),
    strat.plot = c()
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

      #check factors

      if(is.factor(data_reactive$EventLog$EVENT)) { data_reactive$EventLog$EVENT <- as.character(data_reactive$EventLog$EVENT)  }


      # Creating Dl obj e CFM obj
      ObjDL<<-dataLoader(verbose.mode = FALSE)
      ObjDL$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                            format.column.date = "%Y-%m-%d")
      ObjCFM<<-careFlowMiner(verbose.mode = FALSE)
      ObjCFM$loadDataset(inputData = ObjDL$getData())
      data_reactive$node.list<-ObjCFM$getDataStructure()$lst.nodi



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
                         ),


                ),
                target = "Loading EventLog",
                position = "after"
      )

      ################################################################### PREDICTIVE PANEL ####################################################################
      removeTab(inputId = "tabs", target = "Probabilistic CareFlowMiner")
      insertTab(inputId = "tabs",
                tabPanel("Probabilistic CareFlowMiner",
                         titlePanel("Process Discovery: CareFlowMiner - Probabilistic Model"),
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


      ################################################################### KAPLAN MEIER PANEL ###############################################################
      removeTab(inputId = "tabs", target = "Survival Analysis")
      insertTab(inputId = "tabs",
                tabPanel("Survival Analysis",
                         titlePanel("Process Discovery: Survival Analysis"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 3,
                                      fluidRow(
                                        column(9,
                                               p(h3("Parameter Setting")),
                                        ),
                                        column(3,
                                               dropdownButton(
                                                 tags$h4(strong("Survival Analysis with Kaplan Meier")),

                                                 tags$h5("The cohort consists of patients transiting through the node chosen as the start node,
                                                         which must be selected in the ", strong("\"node id start\" field."),"and who have experienced a certain state of interest,
                                                         which must be made explicit in the", strong("\"node id end\" field.")),

                                                 tags$h5("Through the", strong("\"id node censored\" field") ," it will be possible to indicate in which nodes the patients will have to transit
                                                 to in order to be considered censored."),


                                                 tags$h5("using the input ",strong("\"use leaf as cens\"")," it will be possible to choose whether to follow the clinical follow-up of patients up to the last event they experienced"),


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

                                      #KAPLAN MAIER PARAM: FIRST ROW--> ID FROM & ID TO
                                      fluidRow(
                                        column(6,
                                               numericInput("id.start", label = "id node start:", value = 1)
                                               ),
                                        column(6,
                                               numericInput("id.end", label = "id node end:", value = NULL)
                                               )
                                        ),

                                      fluidRow(
                                        column(6,
                                               selectInput(inputId = "id.cens",
                                                           label = "id node censored:",
                                                           choices = names(data_reactive$node.list),
                                                           selected = NULL,
                                                           multiple = TRUE
                                                           )
                                        ),

                                        column(6,
                                               materialSwitch(
                                                 inputId = "cens.leaf",
                                                 label = "Use leaf for cens",
                                                 status = "default",
                                                 right = TRUE)
                                        )
                                      ),


                                    ),

                                    mainPanel(
                                      fluidRow(
                                        column(11,
                                        ),
                                        column(1,
                                               dropdownButton(
                                                 grVizOutput("prev.cfm"),

                                                 circle = FALSE,
                                                 status = "primary",
                                                 size = "xs",
                                                 icon = icon("fas fa-info"),
                                                 width = "1000px",
                                                 right = TRUE,
                                                 tags$div(style = "height: 100px;"),
                                                 tooltip = tooltipOptions(title = "Click to more info")
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
                target = "Probabilistic CareFlowMiner",
                position = "after"
      )
    }

    surv.graph<-reactive({
      plotKM<-KM_CFM(ObjCFM,
                   input$id.start,
                   input$id.end,
                   input$cens.leaf,
                   input$id.cens,
                   ObjDL,
                   UM="days")
      if(is.null(plotKM)){
        validate("please check the inputs")
      }else{
        return(plotKM)
      }
    })

    output$surv.curve<-renderPlot(surv.graph())


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

   output$prev.cfm<-renderGrViz({
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


     return(graph)
   })

   output$CareFlowGraph.pred<-renderGrViz({
     grViz(CFgraph.pred())
   })

  })

  ########################################################## STRATIFIED CF PANEL #################################################
  observeEvent(input$EN_EL,{
    removeTab(inputId = "tabs", target = "Stratified CareFlowMiner")
    insertTab(inputId = "tabs",
              tabPanel("Stratified CareFlowMiner",
                titlePanel("Stratification of the CareFlow Miner"),
                sidebarLayout(
                  sidebarPanel(
                    width = 3,

                    fluidRow(
                      column(9,
                             p(h3("Parameter Setting"))
                      ),
                      column(3,
                             br(),
                             dropdownButton(
                               p(h4("Stratification of the CareFlow Miner")),
                               br(),

                               p(h5("A stratification variable can be entered in this section.
                               In this way it is possible to analyze whether the two groups into which the total population is divided,
                                    present significant differences in terms of the pathway calculated by the Care Flow Miner")),
                               br(),

                               p(h5("Using the inputs in the Parameter Setting bar, it is possibile to select the stratification var,
                                    set whether the variable you choose is categorical or numerical, and explicit the specific values for stratification")),
                               tags$hr(),

                               p(h5("The inferential analysis presented is accomplished by considering the",strong("number of patients passing through each node."),
                                    "it is possible to compare the different sub-courses with respect to", strong("time to arrive at the node"),
                                    "and for the", strong("probability of experiencing a given event of interest"))),


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
                    # fluidRow(
                    #   column(12,
                    #          br()
                    #   )
                    # ),

                    #CFM PARAMETERS
                    fluidRow(
                      column(7,
                             numericInput("depth.strat", label = "Select depth:", value = 5),

                      ),
                      column(5,
                             br(),
                             br(),
                             materialSwitch(
                               inputId = "max_depth.strat",
                               label = "max depth",
                               status = "primary",
                               right = TRUE
                             )
                      )
                    ),
                    # br(),
                    # tags$hr(),
                    #parametro supporto
                    fluidRow(
                      column(12,
                             numericInput("support.strat", label ="Select support value:", value = 10),

                      )
                    ),

                    tags$hr(),

                    fluidRow(
                      #stratification var
                      column(6,
                             selectInput("strat.var", label = "Select variable for the stratification:",
                                         choices = colnames(data_reactive$EventLog)[!(colnames(data_reactive$EventLog) %in% c("ID","DATE_INI","EVENT"))],
                                         selected = NULL)
                             ),
                      #stratification var TYPE
                      column(6,
                             selectInput("strat.var.type", label ="Select the stratification variable type:",
                                         choices = c("Categorical","Numeric"),
                                         selected = NULL)
                             )

                    ),

                    #STRAT VAR VALUES
                    fluidRow(
                      column(6,
                             selectInput("strat.value1", label = "Select possible value fot the selected var:",
                                         choices = NULL,
                                         multiple = FALSE)
                             ),

                      column(6,
                             selectInput("strat.value2", label = "Select possible value fot the selected var:",
                                         choices = NULL,
                                         multiple = TRUE),
                             )
                    ),

                    tags$hr(),

                    fluidRow(
                      column(12,
                             p(h4(strong("Compare for:"))),
                             br()
                             )
                    ),


                    fluidRow(
                      column(6,
                             materialSwitch(
                               inputId = "strat.time",
                               label = "times",
                               status = "default",
                               right = TRUE)
                             ),
                      column(6,
                             materialSwitch(
                               inputId = "perc.end",
                               label = "future state",
                               status = "default",
                               right = TRUE)
                             )
                      ),

                    fluidRow(
                      column(6,
                             ),
                      column(6,
                             selectInput("final.state", label = "Future State:", choices = unique(data_reactive$EventLog["EVENT"]))
                      )
                    ),
                    tags$hr(),

                    fluidRow(
                      column(8,
                      ),
                      column(4,
                             actionButton("refresh","Refresh graph")
                             )
                    )



                  ),
                  mainPanel(
                    jqui_resizable(grVizOutput("CF.strat")),
                    # actionButton("refresh","Refresh graph",width = '30%')
                  )
                )
              ),
              target = "Probabilistic CareFlowMiner",
              position = "after"
    )

  })

  observeEvent(input$strat.var,{
    shiny::updateSelectInput(
      inputId = "strat.value1",
      label = "Select possible value fot the selected var:",
      choices = unique(data_reactive$EventLog[input$strat.var]),
      selected = NULL
    )
    shiny::updateSelectInput(
      inputId = "strat.value2",
      label = "Select possible value fot the selected var:",
      choices = unique(data_reactive$EventLog[input$strat.var]),
      selected = NULL
    )
  })

  observeEvent(input$strat.value1,{
    if(input$strat.var.type=="Categorical"){
      shiny::updateSelectInput(
        inputId = "strat.value2",
        label = "Select possible value fot the selected var:",
        choices = unique(data_reactive$EventLog[input$strat.var])[!unique(data_reactive$EventLog[input$strat.var]) %in% input$strat.value1],
        selected = NULL
      )
    }else{
      shiny::updateSelectInput(
        inputId = "strat.value2",
        label = "Select possible value fot the selected var:",
        choices = c("only for categorical var"),
        selected = NULL
      )
    }

  })


  observeEvent(input$strat.var.type,{
    if(input$strat.var.type=="Categorical"){
      shiny::updateSelectInput(
        inputId = "strat.value1",
        label = "Select possible value fot the selected var:",
        choices = unique(data_reactive$EventLog[input$strat.var])
      )

      shiny::updateSelectInput(
        inputId = "strat.value2",
        label = "Select possible value fot the selected var:",
        choices = unique(data_reactive$EventLog[input$strat.var])[!unique(data_reactive$EventLog[input$strat.var]) %in% input$strat.value1],
        selected = NULL
      )
    }
    else if (input$strat.var.type=="Numeric"){
      shiny::updateSelectInput(
        inputId = "strat.value1",
        label = "Select possible value fot the selected var:",
        choices = c("only for categorical var"),
        selected = NULL
      )
      shiny::updateSelectInput(
        inputId = "strat.value2",
        label = "Select possible value fot the selected var:",
        choices = c("only for categorical var"),
        selected = NULL
      )
    }
  })

  observeEvent(input$refresh,{
    if(input$max_depth.strat){
      dp<-Inf
    }else{
      dp<-input$depth.strat
    }


    if(input$strat.var.type=="Categorical"){
      if(length(input$strat.value2)>1){
        script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
                                             arr.stratificationValues.A = input$strat.value1,
                                             arr.stratificationValues.B = input$strat.value2,
                                             depth= dp,
                                             abs.threshold = input$support.strat,
                                             checkDurationFromRoot = input$strat.time,
                                             hitsMeansReachAGivenFinalState = input$perc.end,
                                             finalStateForHits = input$final.state ,
                                             kindOfGraph = "dot",
                                             nodeShape = "square")$script

      }else{
        script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
                                             stratificationValues = c(input$strat.value1,input$strat.value2),
                                             depth= dp,
                                             abs.threshold = input$support.strat,
                                             checkDurationFromRoot = input$strat.time,
                                             hitsMeansReachAGivenFinalState = input$perc.end,
                                             finalStateForHits = input$final.state ,
                                             kindOfGraph = "dot",
                                             nodeShape = "square")$script
      }

    }else{
      mediana <- median(as.numeric(data_reactive$EventLog[,input$strat.var]),na.rm = T)

      script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
                                           stratificationThreshold = mediana,
                                           depth= dp,
                                           abs.threshold = input$support.strat,
                                           checkDurationFromRoot = input$strat.time,
                                           hitsMeansReachAGivenFinalState = input$perc.end,
                                           finalStateForHits = input$final.state ,
                                           kindOfGraph = "dot",
                                           nodeShape = "square")$script

    }

    data_reactive$strat.plot<- script
  })

  # CF.strat.plot<-reactive({
  #   if(data_reactive$max_depth){
  #     dp<-Inf
  #   }else{
  #     dp<-data_reactive$depth
  #   }
  #
  #
  #   if(input$strat.var.type=="Categorical"){
  #     if(length(input$strat.value2)>1){
  #       #RICARICO EL SOLO IN CASO DI DUMMY
  #       # ObjDL.out<-ObjDL$getData()
  #       # tmp.csv <- ObjDL.out$original.CSV
  #       # #MINORE -> 0
  #       # tmp.csv[which(ObjDL.out$original.CSV[,input$strat.var] %in% input$strat.value2),input$strat.var]<-0
  #       # tmp.csv[which(ObjDL.out$original.CSV[,input$strat.var]%in% input$strat.value1),input$strat.var]<-1
  #       # #MAGGIORE = ->1
  #       # tmp.DL <- dataLoader(verbose.mode = FALSE)
  #       # tmp.DL$load.data.frame(mydata = tmp.csv,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",format.column.date = "%d/%m/%Y")
  #
  #       # tmp.ObjCFM <- careFlowMiner()
  #       # tmp.ObjCFM$loadDataset(inputData = ObjDL$getData() )
  #       script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
  #                                                arr.stratificationValues.A = input$strat.value1,
  #                                                arr.stratificationValues.B = input$strat.value2,
  #                                            depth= dp,
  #                                            abs.threshold = data_reactive$support,
  #                                            checkDurationFromRoot = input$strat.time,
  #                                            hitsMeansReachAGivenFinalState = input$perc.end,
  #                                            finalStateForHits = input$final.state ,
  #                                            kindOfGraph = "dot",
  #                                            nodeShape = "square")$script
  #
  #     }else{
  #       script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
  #                                            stratificationValues = c(input$strat.value1,input$strat.value2),
  #                                            depth= dp,
  #                                            abs.threshold = data_reactive$support,
  #                                            checkDurationFromRoot = input$strat.time,
  #                                            hitsMeansReachAGivenFinalState = input$perc.end,
  #                                            finalStateForHits = input$final.state ,
  #                                            kindOfGraph = "dot",
  #                                            nodeShape = "square")$script
  #     }
  #
  #   }else{
  #     mediana <- median(as.numeric(data_reactive$EventLog[,input$strat.var]),na.rm = T)
  #
  #     # ObjDL.out<-ObjDL$getData()
  #     # tmp.csv <- ObjDL.out$original.CSV
  #     # #MINORE -> 0
  #     # tmp.csv[which(ObjDL.out$original.CSV[,input$strat.var]<mediana),input$strat.var]<-0
  #     # tmp.csv[which(ObjDL.out$original.CSV[,input$strat.var]>=mediana),input$strat.var]<-1
  #     # #MAGGIORE = ->1
  #     # tmp.DL <- dataLoader(verbose.mode = FALSE)
  #     # tmp.DL$load.data.frame(mydata = tmp.csv,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",format.column.date = "%d/%m/%Y")
  #     #
  #     # tmp.ObjCFM <- careFlowMiner()
  #     # tmp.ObjCFM$loadDataset(inputData = tmp.DL$getData() )
  #     script<-ObjCFM$plotCFGraphComparison(stratifyFor = input$strat.var,
  #                                              stratificationThreshold = mediana,
  #                                          depth= dp,
  #                                          abs.threshold = data_reactive$support,
  #                                          checkDurationFromRoot = input$strat.time,
  #                                          hitsMeansReachAGivenFinalState = input$perc.end,
  #                                          finalStateForHits = input$final.state ,
  #                                          kindOfGraph = "dot",
  #                                          nodeShape = "square")$script
  #
  #   }
  #
  #     return(script)
  #
  # })

  output$CF.strat<-renderGrViz({
    if(is.null(data_reactive$strat.plot)){
      validate("please click the refresh button")
    }else{
      grViz(data_reactive$strat.plot)
    }

    # grViz(CF.strat.plot(),env=parent.frame())
  })


}


