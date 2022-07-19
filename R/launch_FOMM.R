FOMM.mod<-function(){
  all.data<<-list()
  shinyApp(ui =fluidPage(
    #Pagina Principale
    navbarPage("pMining: First Order Markov Model", id="tabs",
               tabPanel("Loading EventLog",
                        titlePanel("EventLog Uploading"),
                        br(),
                        import_mod_ui("uploadEL","Upload EventLog file",FALSE,col_setting=TRUE),
                        actionButton("loadEL","Load Event Log",width = '32%') ,

               )
    )
  ),

  server = server.FOMM)
}
