#'@title descriptive module: main page
#'
#'
#'@import rlang
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT




# library(rlang)
# library(shiny)
# library(shinythemes)
# library(dplyr)
# library(shinyWidgets)
# library(DT)


# ui.descr<-tagList(
#   fluidPage(
#     # tags$head(
#     #   tags$style(HTML("
#     #   .shiny-output-error-validation {
#     #     color: red;
#     #   }
#     # "))
#     # ),
#
#     #Pagina Principale
#     navbarPage("pMining: EventLog Visual Analysis", id="tabs",
#                tabPanel("Loading Data",
#                         titlePanel("Data Uploading"),
#                         br(),
#                         import_mod_ui("uploadEL","Upload EventLog file",FALSE),
#                         actionButton("loadEL","Load Event Log",width = '32%') ,
#                )
#     )
#   )
# )

server.descr<-function(input,output,session){
  #visualizzazione EventLog
  tab<-callModule(import_data_server,"uploadEL","EventLog")


  # reactiveValues: initializing data as null data frame
  data_reactive <- reactiveValues(
    EventLog=data.frame(),
    ancillaryData=list(),
    tabs=list(),
    event_delete=array(),
    event_density=array(),
    date_range=array()
  )


  observeEvent(input$eventList,{
    if(length(data_reactive$event_delete)<length(input$eventList)){
      objDL.new <<- dataLoader(verbose.mode = FALSE)
      objDL.new$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                                format.column.date = "%Y-%m-%d")
    }
    data_reactive$event_delete<-input$eventList

  })

  observeEvent(input$eventdensityList,{
    data_reactive$event_density<-input$eventdensityList
  })





  #TAB EVENTLOG: data visualization of eventlog
  observeEvent(input$loadEL,{
    data_reactive$EventLog <- all.data[["EventLog"]]
    objDL.new <<- dataLoader(verbose.mode = FALSE)
    objDL.new$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                              format.column.date = "%Y-%m-%d")
    # objDL.new.export <<- objDL.new$getData()
    # data_reactive$EventLog <- loadData("uploadEL.csv")
    if(is_empty(data_reactive$EventLog)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Load your EventLog, then press 'Load Event Log'  button",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()
    }


    #loading data uploaded in the upload section
    removeTab(inputId = "tabs", target = "EventLog data analysis")
    if(!is_empty(data_reactive$EventLog)){
      insertTab(inputId = "tabs",
                tabPanel("EventLog data analysis",
                         titlePanel("Descriptive Data Analysis"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      p(h4(strong("Select variable to plot"))),
                                      awesomeCheckboxGroup("eventList","",
                                                           choices = unique(data_reactive$EventLog[,4]),
                                                           selected =unique(data_reactive$EventLog[,4]),
                                                           status = "#primary"
                                      )
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        #TAB PANEL1: PLOT THE EVENT DISTRIBUTION
                                        tabPanel("Event Distribution",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Event Distribution")),
                                                            tags$h5("this graph shows the" ,strong("percentage of occurrence")," of the selected events,
                                                   calculated as the number of events of the single type out of the total
                                                   of events present in the event log"),
                                                            tags$h5("switch to plot absolute counts instead of percentage on the y axis:"),

                                                            switchInput(
                                                              inputId = "eventDist_count",
                                                              label = "counts",
                                                              labelWidth = "20px",
                                                              size = "mini"
                                                            ),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(
                                                   br(),
                                                   plotOutput("eventdist"),
                                                 )
                                        ),

                                        #TAB PANEL 2: EVENT DISTRIBUTION OVER PATIENTS
                                        tabPanel("Event distribution over patiens",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Event Distribution over Patients")),
                                                            tags$h5("the graph shows the" ,strong("distribution"),"of events by patient,
                                                                  showing as a percentage, how many patients shared a specific type of event"),
                                                            tags$h5("switch to plot absolute counts instead of percentage on the y axis:"),
                                                            switchInput(
                                                              inputId = "eventPaz_count",
                                                              label = "counts",
                                                              labelWidth = "20px",
                                                              size = "mini"
                                                            ),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(
                                                   br(),
                                                   plotOutput("eventdist_Paz")
                                                 )
                                        ),
                                        #TAB PANEL 3: PANEL FOR EVENT DISTRIBUTION OVER TIME
                                        tabPanel("Event distribution over time",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Event Distribution over time")),
                                                            tags$h5("The bar plot shows" ,strong("the distribution"),"of the  events in different time."),
                                                            tags$h5("the time axis has been discretized using ",strong("quarters"),
                                                                    "in order to highlight how the distribution of each event changes over time"),

                                                            sliderInput(
                                                              "date.range",
                                                              "Select Data Range:",
                                                              min = min(as.Date(data_reactive$EventLog[,2])),
                                                              max = max(as.Date(data_reactive$EventLog[,2])),
                                                              value=c(min(as.Date(data_reactive$EventLog[,2])),max(as.Date(data_reactive$EventLog[,2]))),
                                                              timeFormat="%Y-%m-%d"),


                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info")
                                                          )
                                                   )),
                                                 fluidRow(
                                                   plotOutput("eventdist_Time")
                                                 )
                                        ),


                                        #TABPANEL 4:PANEL FOR THE PLOT HO HEAT MAP
                                        tabPanel("Co-occurence Matrix",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Co-occurence Matrix")),
                                                            tags$h5("this graph shows the" ,strong("co-occurrence")," of the selected events,
                                                                  calculated as the number of times (among the total) each event occurs in the presence of another,
                                                                  within the clinical path of the same patient."),
                                                            tags$h5(strong("Shades of red"),"are used to highlight", strong("high correlated events")),
                                                            tags$h5(strong("Shades of yellow"),"are used to", strong("low correlated events")),
                                                            tags$h5("Only co-occurrence values graater than 0.5 are shown"),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(

                                                   plotOutput("heatmap")
                                                 )
                                        ),

                                        #TABPANEL 5: TABLE PANEL for UPSET Plot
                                        tabPanel("Upset Plot",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Upset-plot")),
                                                            tags$h5("The upsetplot is another way to analyze" ,strong("the co-occurrence"),"of the  events."),
                                                            tags$h5("In the lower part of the graph are represented the",strong("combination sets"),
                                                                    ",which are the combination of events that co-occur in the clinical path of at least one patient."),
                                                            tags$h5("The graph shows the frequencies of occurrence for each combination set"),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(
                                                   plotOutput("upsetplot")
                                                 )
                                        ),
                                      )
                                    ),
                                  ),
                           )
                         ),
                ),
                target = "Loading Data",
                position = "after")
    }
  })


  #========================== Event distribution plot (first tabPanel: output$eventdist)=================================
  plot_obj_eldist<-reactive({
    plot_fin<-event_plot(data_reactive$EventLog,data_reactive$event_delete,input$eventDist_count)
    return(plot_fin)
  })

  #plot bar plot
  output$eventdist<-renderPlot({
    plot_obj_eldist()
  })

  #==================================== Event distribution over Patiens (second tabPanel: output$eventdist_Paz)===================
  plot_obj_event_paz<-reactive({
    plot_fin<-event_pat(data_reactive$EventLog,data_reactive$event_delete,input$eventPaz_count)
    return(plot_fin)
  })

  output$eventdist_Paz<-renderPlot({
    plot_obj_event_paz()
  })

  #==================================== Event distribution over time (second tabPanel: output$eventdist_Time)===================
  observeEvent(input$date.range,{
    data_reactive$date_range<-input$date.range
  })



  plot_obj_event_time<-reactive({
    plot_fin<-time_dist(data_reactive$EventLog,data_reactive$event_delete, data_reactive$date_range)
    return(plot_fin)
  })

  output$eventdist_Time<-renderPlot({
    plot_obj_event_time()
  })
  #=================================== Event heatmap (third tabPanel: output$heatmap)==========================================
  plot_obj_heatmap<-reactive({
    validate(
      need(length(data_reactive$event_delete)>1, "You need at least 2 different events")
    )

    plot_fin<-heatmap(data_reactive$EventLog,data_reactive$event_delete,objDL.new)
    return(plot_fin)
  })

  output$heatmap<-renderPlot({
    plot_obj_heatmap()
  })
  #=================================== Event upsetPlot plot (fourth tabPanel: output$upsetplot) =================================
  plot_obj_upsetplot<-reactive({
    validate(
      need(length(data_reactive$event_delete)>1, "You need at least 2 different events")
    )

    plot_fin<-upset_fun(data_reactive$EventLog,data_reactive$event_delete,objDL.new)
    return(plot_fin)
  })

  output$upsetplot<-renderPlot({
    plot_obj_upsetplot()
  })


}

# shinyApp(ui = ui.descr, server = server.descr)
