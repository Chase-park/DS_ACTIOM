library(shiny)


rmse=data.frame(rmse)

ui<-pageWithSidebar(
  headerPanel('¸á·Ğ ¿ªÁÖÇà°î ºĞ¼® '),
  sidebarPanel(
    selectInput('xcol', 'X Variable', head(rmse)$id))
  ,
  mainPanel(
    plotOutput('plot1')
  )
)




server= function(input,output){
  
  selectedData=reactive(raw[which(raw$id==input$xcol),])
  
  
  
  output$plot1 = 
    
    renderPlot(
    ggplot(data = selectedData(), aes(x = weeknum, y = rank)) + 
    geom_point(color='blue') +
    geom_smooth(method = "lm", se = FALSE,color = 'red')+
    ggtitle(paste(raw[(which(raw$id==input$xcol)),]$title[1],'-',raw[(which(raw$id==input$xcol)),]$artist[1])) +
    scale_y_continuous(trans = "reverse") 
    )
}  






shinyApp(ui,server)
