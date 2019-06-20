library(shiny)


setwd('C:/Users/Chase/Desktop/DS/xaringan/')

rmse_cause=read.csv('rmse.csv')




rmse_cause = 
  rmse_cause %>% 
  filter(cause != '')


ui<-pageWithSidebar(
  headerPanel('멜론 역주행곡 분석 '),
  sidebarPanel(
    radioButtons('cause','역주행 원인',  unique(rmse_cause$cause)),
    conditionalPanel(
      condition = "input.cause == '방송출연'",
      selectInput('xcol', '곡을 선택하세요 ', 
                              rmse_cause[which(rmse_cause$cause == '방송출연'),]$title)
    ),
    conditionalPanel(
      condition = "input.cause == '날씨'",
      selectInput('ycol', '곡을 선택하세요 ', 
                  rmse_cause[which(rmse_cause$cause == '날씨'),]$title)
    ),
    conditionalPanel(
      condition = "input.cause == '사회적이슈'",
      selectInput('zcol', '곡을 선택하세요 ', 
                  rmse_cause[which(rmse_cause$cause == '사회적이슈'),]$title)
    ),
    conditionalPanel(
      condition = "input.cause == '계절'",
      selectInput('acol', '곡을 선택하세요 ', 
                  rmse_cause[which(rmse_cause$cause == '계절'),]$title)
    ),
    conditionalPanel(
      condition = "input.cause == '아이돌컴백'",
      selectInput('bcol', '곡을 선택하세요 ', 
                  rmse_cause[which(rmse_cause$cause == '아이돌컴백'),]$title)
    )
    )
    ,
  mainPanel(
    plotOutput('plot1')
  )
)




server= function(input,output){
  

    selectedData=reactive(raw[which(raw$title==isolate(input$xcol)),])
  
  
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

