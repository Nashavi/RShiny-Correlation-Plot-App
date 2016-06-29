shinyUI(pageWithSidebar(
  headerPanel("Commodity Price Vs item Price Plots"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    #These column selectors are dynamically created when the file is loaded
#     uiOutput("fvar"),
#     uiOutput("svar"),
#     uiOutput("itm"),
    uiOutput("asitmno"),
  
    #The action button prevents an action firing before we're ready
    actionButton("getplot", "Get Plot")
    
     ),
  
  mainPanel(
    #tableOutput("filetable"),
    #tableOutput("geotable"),
    plotOutput("plot")
  )
))