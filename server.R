require(dplyr)

shinyServer(function(input, output) {
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  
  #The following set of functions populate the column selectors
#   output$fvar <- renderUI({
#     df <-filedata()
#     if (is.null(df)) return(NULL)
#     
#     items=names(df)
#     names(items)=items
#     selectInput("fvar", "First Variable:",items)
#     
#   })
  
#   output$svar<- renderUI({
#     df <-filedata()
#     if (is.null(df)) return(NULL)
#     
#     items=names(df)
#     names(items)=items
#     selectInput("svar", "Second Variable:",items)
#     
#   })
#   
#   output$itm<- renderUI({
#     df <-filedata()
#     if (is.null(df)) return(NULL)
#     
#     items=names(df)
#     names(items)=items
#     selectInput("itm", "AS Item Column:",items)
#     
#   })
  
  output$asitmno <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
#     if (any(
#       is.null(x)
#     )) 
#       return("Select")
    itmchoices <- unique(df$ASItemNo)
    selectInput("asitmno", "AS Item No:", choices = itmchoices)
  })
  
#   #This previews the CSV data file
#   output$filetable <- renderTable({
#     filedata()
#   })
#   
  #This function is the one that is triggered when the action button is pressed
  #The function is a geocoder from the ggmap package that uses Google maps geocoder to geocode selected locations
  geodata <- reactive({
    if (input$getplot == 0) return(NULL)
    df=filedata()
    itm=input$asitmno
    if (is.null(df)) return(NULL)
    
    #The function acts reactively when one of the variables it uses is changed
    #If we don't want to trigger when particular variables change, we need to isolate them 
    # isolate({
    #   #Get the CSV file data
    #   dummy=filedata()
    #   #Which from/to columns did the user select?
    #   f=input$fvar
    #   s=input$svar
    #   #If locations are duplicated in from/to columns, dedupe so we don't geocode same location more than once
    #   locs=data.frame(place=unique(c(as.vector(dummy[[f]]),as.vector(dummy[[s]]))),stringsAsFactors=F)
    #   #Run the geocoder against each location, then transpose and bind the results into a dataframe
    #   cbind(locs, t(sapply(locs$place,geocode, USE.NAMES=F))) 
  })
#      
 tmpdf <- reactive({
    df <-filedata()
    df$MonthDate<-as.Date(df$MonthDate, "%m/%d/%Y")
  })
  
    output$plot <- renderPlot({
      df <-tmpdf()
     tdf<- df[df$ASItemNo %in% input$asitmno]
    p<-ggplot() + 
        geom_line(data=tdf, aes(y = stdcom, x = MonthDate, color = "Commodity Price")) +geom_line(data = tmpdf, aes(y = stitm, x = MonthDate, color = "Item Price",text = paste("R2 value:",round(cor0*100,2),"%")))  + ylab('Price') + xlab('MonthDate')+ggtitle("Plot of Item Price vs Commodity Price")
      
    print(p)
      #ggplot(data = df, aes(x=input$fvar, y=input$svar)) + geom_tile() +geom_point(aes(text = paste("Client:", Client)))+geom_smooth(aes(colour = Client))+theme_light()
   # })
})
})