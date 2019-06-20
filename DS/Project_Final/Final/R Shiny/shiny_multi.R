library(shiny)


setwd('C:/Users/Chase/Desktop/DS/xaringan/')

rmse_cause=read.csv('rmse.csv')




rmse_cause = 
  rmse_cause %>% 
  filter(cause != '')


ui<-pageWithSidebar(
  headerPanel('��� ������� �м� '),
  sidebarPanel(
    radioButtons('cause','������ ����',  unique(rmse_cause$cause)),
    conditionalPanel(
      condition = "input.cause == '����⿬'",
      selectInput('xcol', '���� �����ϼ��� ', 
                              rmse_cause[which(rmse_cause$cause == '����⿬'),]$title)
    ),
    conditionalPanel(
      condition = "input.cause == '����'",
      selectInput('ycol', '���� �����ϼ��� ', 
                  rmse_cause[which(rmse_cause$cause == '����'),]$title)
    ),
    conditionalPanel(
      condition = "input.cause == '��ȸ���̽�'",
      selectInput('zcol', '���� �����ϼ��� ', 
                  rmse_cause[which(rmse_cause$cause == '��ȸ���̽�'),]$title)
    ),
    conditionalPanel(
      condition = "input.cause == '����'",
      selectInput('acol', '���� �����ϼ��� ', 
                  rmse_cause[which(rmse_cause$cause == '����'),]$title)
    ),
    conditionalPanel(
      condition = "input.cause == '���̵��Ĺ�'",
      selectInput('bcol', '���� �����ϼ��� ', 
                  rmse_cause[which(rmse_cause$cause == '���̵��Ĺ�'),]$title)
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
