library(shiny)

library(rsconnect)


rmse_cause=read.csv('rmse.csv')
raw= read.csv('melonchart_pre (ANSI).csv')

rmse_broad = 
  rmse_cause %>% 
  filter(cause == '방송출연')

ui<-pageWithSidebar(
  headerPanel('멜론 역주행곡 분석_방송출연 '),
  sidebarPanel(
    selectInput('xcol', '제목을 선택하세요 ', head(rmse_broad)$title))
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


setwd('C:/Users/Chase/Desktop/DS/R Shiny/app')
