#'@title grouping module: main page
#'
#'
#'@import rlang
#'@import shiny
#'@import shinythemes
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@impot  ggplot2
#'@import sortable
#'@import pMineR




# library(rlang)
# library(shiny)
# library(shinythemes)
# library(dplyr)
# library(shinyWidgets)
# library(DT)
# library(ggplot2)
# library(sortable)
# library(pMineR)




# ui.group<-navbarPage("pMining: Grouping Module", id="tabs",
#
#                      tabPanel("Loading Data",
#                               titlePanel("Data Uploading"),
#                               br(),
#                               import_mod_ui("uploadEL","Upload EventLog file",FALSE),
#                               actionButton("loadEL","Load Event Log",width = '32%') ,
#                               )
#                      )


server.group<-function(input,output,session) {

  tab<-callModule(import_data_server,"uploadEL","EventLog")

  data_reactive<-reactiveValues(
    EventLog = data.frame(),
    pat.process=list(),
    visual=array(),
    tabs=list()
  )

  observeEvent(input$loadEL,{

    data_reactive$EventLog <- all.data[["EventLog"]]
    # removeTab(inputId = "tabs", target = "Add Dictionary")

    if(is_empty(data_reactive$EventLog)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Load your EventLog, then press 'Load Event Log'  button",
        type = "primary"
        )
      data_reactive$EventLog<-data.frame()
    } else{
      objDL.new <- dataLoader(verbose.mode = FALSE)
      objDL.new$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                                format.column.date = "%Y-%m-%d")
      obj.out<-objDL.new$getData()
      data_reactive$pat.process<-obj.out$pat.process

      removeTab(inputId = "tabs", target = "Add Dictionary")
      insertTab(inputId = "tabs",
                tabPanel("Add Dictionary",
                         titlePanel("New Dictionary"),
                         br(),
                         dict_mod_ui("dict0", data_reactive$EventLog),
                         br(),
                         fluidRow(
                          column(12,
                                 actionButton("add",label = "Add new Dictionary")
                                 )),
                         fluidRow(
                          column(12,
                                 actionButton("apply",label = "Apply Dictionary")
                                 )
                         )

                ),
                target = "Loading Data",
                position = "after")
    }

    callModule(dict_mod_server,"dict0",data_reactive$EventLog, "dict0",data_reactive$pat.process)

    observeEvent(input$add,{
      insertUI(
        selector = "#add",
        where = "beforeBegin",
        ui = dict_mod_ui(paste0("dict", input$add), data_reactive$EventLog)
      )
      callModule(dict_mod_server,paste0("dict", input$add),data_reactive$EventLog, paste0("dict", input$add),data_reactive$pat.process)
    })

    # callModule(dict_mod_server,"dict0",data_reactive$EventLog, paste0("dict", input$add),data_reactive$pat.process)

    })


  observeEvent(input$apply,{
    if(is_empty(all.dict)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "no dictionaries saved: create your new dictionary, save it with the 'save dictionary' button and then continue with the 'apply dictionary' button",
        type = "primary"
      )
    }else{
      tab<-list()
      for (i in c(1:length(all.dict))){
        tab[[i]]<-tabPanel(paste0(dict.names[[i]]),
                           group_visual_mod(paste0("visual",i))
        )
      }

      lapply(1:length(all.dict), function(i){
        callModule(group_visual_server,
                   paste0("visual",i),data_reactive$EventLog, i,data_reactive$pat.process, data_reactive$visual)
      })

      data_reactive$tabs<-tab

      removeTab(inputId = "tabs", target = "Apply Dictionary")
      insertTab(inputId = "tabs",
                tabPanel("Apply Dictionary",
                         titlePanel("Apply Dictionary"),
                         br(),
                         # uiOutput("dict.tabs"),
                         sidebarLayout(
                           sidebarPanel(
                             p(h5("Each tab allows the display of each dictionary added.
                                  You can view the grouping of events allowed by the dictionary through the display of the matching table
                                  or through an histogram showing the new events distribution")),
                             br(),
                             selectInput(inputId = "show.visual", label = "Select the visualization: ",
                                         choices = c("Group Table","Bar Plot"),
                                         selected = "Bar Plot"),
                             br(),
                             p(h5("You can download the new event log by clicking the",strong("download button ")))

                             ),


                           mainPanel(
                             uiOutput("dict.tabs")
                           )
                         ),
                         br(),

                ),
                target = "Add Dictionary",
                position = "after")

    }






  })

  output$dict.tabs<-renderUI({
    tagList(
      # do.call(tabsetPanel,tab)
      do.call(tabsetPanel,data_reactive$tabs)
    )
  })

  observeEvent(input$show.visual,{
    data_reactive$visual<-input$show.visual
    tab<-list()
    for (i in c(1:length(all.dict))){
      tab[[i]]<-tabPanel(paste0(dict.names[[i]]),
                         group_visual_mod(paste0("visual",i))
      )
    }

    lapply(1:length(all.dict), function(i){
      callModule(group_visual_server,
                 paste0("visual",i),data_reactive$EventLog, i,data_reactive$pat.process, data_reactive$visual)
    })

    data_reactive$tabs<-tab
  })

  # tabs<-reactive({
  #   tab<-list()
  #   for (i in c(1:length(all.dict))){
  #     tab[[i]]<-tabPanel(paste0(dict.names[[i]]),
  #                        group_visual_mod(paste0("visual",i))
  #     )
  #   }
  #
  #   lapply(1:length(all.dict), function(i){
  #     callModule(group_visual_server,
  #                paste0("visual",i),data_reactive$EventLog, i,data_reactive$pat.process, data_reactive$visual)
  #   })
  #   return(tab)
  # })
}

#summary of new groups
# evt.tab<-function(gruppi,eventi){
#   #gruppi<-list
#   #eventi<-array
#   gru<-rep("",length(eventi))
#   df<-data.frame(eventi,gru)
#   names(df)<-c("EVENT", "GROUP")
#
#   tmp<-lapply(1:length(eventi), function(evento){
#     for(i in c(2:length(gruppi))){
#       if(eventi[evento] %in% gruppi[[i]]){
#         gr<-names(gruppi)[i]
#
#         break
#       }else{
#         gr<-eventi[evento]
#       }
#     }
#     return(gr)
#   })
#   tmp<-lapply(tmp, function(x) if(is.null(x)) NA else x)
#   df[,2]<-unlist(tmp)
#   df<-subset(df,!is.na(df[,2]))
#   return(df)
# }





# #CREATE DICTIONARY FUNCTION
# tab.group.fun<-function(group.list,arr.eventi){
#   gruppi<-rep("",length(arr.eventi))
#   df<-data.frame(arr.eventi,gruppi)
#   names(df)<-c("EVENT", "GROUP")
#   if(!is_empty(group.list)){
#     tmp<-lapply(1:nrow(df),function(riga){
#       for(i in c(1:length(group.list))){
#         if(df[riga,1] %in% group.list[[i]]){
#           gr<-names(group.list[i])
#           break
#         }else{
#           gr<-df[riga,1]
#         }
#       }
#       return(gr)
#     })
#     tmp<-lapply(tmp, function(x) if(is.null(x)) NA else x)
#     df[,2]<-unlist(tmp)
#     df<-subset(df,!is.na(df[,2]))
#   }
#   return(df)
# }

# tmp<-lapply(1:length(x), function(lista){
#   if(length(x[[lista]])>1){
#     name<-str_c(x[[lista]],collapse = " ")
#   }else{
#     name<-x[[lista]]
#   }
#   return(name)
# })


# #LAUNCH FUN
# group.mod<-function(){
#   all.data<<-list()
#   dict.names<<-list()
#   all.dict<<-list()
#   shinyApp(ui.group, server.group)
# }





