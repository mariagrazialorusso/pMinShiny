#'@title dict_visual_mod: first grouping mod
#'
#'
#'@import shiny
#'@import shinythemes
#'@import dplyr
#'@import shinyWidgets
#'@import DT





group_visual_mod<- function(id){
  #flag=TRUE-->Merge Module, it is necessary to select a key for the uploaded data set
  #flag=FALSE-->Visualization module
  ns<-NS(id)
  fluidPage(
    fluidRow(
      uiOutput(ns("visual"))
      # sidebarLayout(
      #   sidebarPanel(
      #     selectInput(ns("show.visual"),label = " ",
      #                 choices = c("Group Table","Bar Plot","New EventLog"),
      #                 selected = "Group Table")),
      #   mainPanel(
      #     uiOutput(ns("visual"))
      #   )
      # )
    )
  )

}

group_visual_server<-function(input,
                          output,
                          session,
                          data,
                          id,
                          pat,
                          type.of.visual){
  ns <- session$ns


  #TAB OUTPUT
  tab<-reactive({
    dict<-all.dict[[id]]
    df<-evt.tab(dict,unique(data[,4]))
    return(df)
  })


  #DIST OUTPUT
  dist<-reactive({
    pat.process<-pat
    dict<-all.dict[[id]]
    df1<-evt.tab(dict,unique(data[,4]))
    df<-applyDict(column.name="GROUP" ,
                  dict.name = 'main',
                  column.event.name= "EVENT",
                  pat.process,
                  param.EVENTName="EVENT",
                  df1)[,2:6]
    plot_fin<-event_plot(df,unique(df[,4]),FALSE)
    return(plot_fin)
  })

  #EL OUTPUT
  EL<-reactive({
    pat.process<-pat
    df1<-evt.tab(all.dict[[id]],unique(data[,4]))
    df<-applyDict(column.name="GROUP" ,
                  dict.name = 'main',
                  column.event.name= "EVENT",
                  pat.process,
                  param.EVENTName="EVENT",
                  df1)[,2:6]
    return(df)
  })


  output$visual<-renderUI({
    fluidPage(
      fluidRow(
        if(type.of.visual=="Group Table"){
          DT::dataTableOutput(ns("group.tab"))
        }else {
          plotOutput(ns("bar.plot"))
        }
      ),
      fluidRow(
        DT::dataTableOutput(ns("EL.tab"))
      ),
      fluidRow(
        downloadButton(ns("downloadData"), "Download")
      )
    )
    # DT::dataTableOutput(ns("group.tab"))
    # if(type.of.visual=="Group Table"){
    #   DT::dataTableOutput(ns("group.tab"))
    # }else if(type.of.visual== "Bar Plot"){
    #   plotOutput(ns("bar.plot"))
    # }else{
    #   fluidPage(
    #     fluidRow(
    #       DT::dataTableOutput(ns("EL.tab"))
    #     ),
    #     fluidRow(
    #       downloadButton(ns("downloadData"), "Download")
    #     )
    #   )
    # }
  })

  output$group.tab<- DT::renderDataTable(tab(),rownames = FALSE)
  output$bar.plot<- renderPlot(dist())
  output$EL.tab<- DT::renderDataTable(EL(),rownames = FALSE)
  output$downloadData <- downloadHandler(

    filename = function() {
      paste("groupedEL", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(EL(), file, row.names = FALSE)
    }
  )




}


