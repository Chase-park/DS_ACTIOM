setwd('C:/Users/Chase/Desktop/DS/xaringan')

library(shiny)
library(rsconnect)

rmse_cause=read.csv('rmse_cause.csv')
raw= read.csv('raw.csv')

rmse_pre = 
  rmse_cause %>% 
  filter(cause == '날씨')

ui<-pageWithSidebar(
  headerPanel('멜론 역주행곡 분석_강수량  '),
  sidebarPanel(
    selectInput('xcol', '제목을 선택하세요 ', head(rmse_pre)$title))
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
        scale_y_continuous(trans = "reverse") +
        geom_line(data=a,aes(x=weeknum,y=100-pre_sum/15),color='purple')
      
    )
}  






shinyApp(ui,server)


setwd('C:/Users/Chase/Desktop/DS/R Shiny/app')
