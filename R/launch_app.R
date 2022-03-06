#'@title Launch merge module
#'
#'@export




# library(shiny)
# library(dplyr)
# library(rlang)
# library(shinyWidgets)
# library(XML)
# library(DT)
# library(shinybusy)

# library(ggplot2)
# library(scales)
# library(stringr)
# library(shinyjs)
# library(shinythemes)
# library(AmesHousing)

# files<-list.files(file.path("myScripts/merge_module"), full.names = TRUE)[!list.files(file.path("myScripts/merge_module")) %in%
#                                                                                     c(
#                                                                                       "launch_app.R",
#                                                                                       "save_load_fun.R"
#
#                                                                                       )]
# sapply(files, source)


#lancio app
launch.fun<-function(){
all.data<<-list()
all.key<<-list()
all.xml<<-list()
all.sRules<<-list()
shinyApp(ui = fluidPage(
  # #theme selection
  # shinythemes::themeSelector(),
  # #error Style:
  # tags$head(tags$style(HTML(".shiny-output-error-validation {color: #ff0000;font-weight: bold;}"))),

  #Pagina Principale
  navbarPage("pMining: EventLog Merge Module", id="tabs",
             tabPanel("Loading EventLog",
                      titlePanel("Event Log Loading"),
                      br(),
                      import_mod_ui("uploadEL","Upload EventLog file",TRUE),
                      actionButton("loadEL","Load Event Log",width = '32%') ,
             )
  )
),
         server = server)
}

# launch.fun()
# visual.mod()


