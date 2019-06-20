library(shiny)


rmse=read.csv('rmse.csv')
raw=read.csv('raw.csv')



ui<-pageWithSidebar(
  headerPanel('멜론 역주행곡 분석 '),
  sidebarPanel(
    selectInput('xcol', '제목을 선택하세요 ', head(rmse)$title))
  ,
  mainPanel(
    plotOutput('plot1')
  )
)




server= function(input,output){
  
  selectedData=reactive(raw[which(raw$title==input$xcol),])
  
  
  
  output$plot1 = 
    
    renderPlot(
    ggplot(data = selectedData(), aes(x = weeknum, y = rank)) + 
    geom_point(color='blue') +
    geom_smooth(method = "lm", se = FALSE,color = 'red')+
    ggtitle(paste(raw[(which(raw$title==input$xcol)),]$title[1],'-',raw[(which(raw$title==input$xcol)),]$artist[1])) +
    scale_y_continuous(trans = "reverse") 
    )
}  






shinyApp(ui,server)

