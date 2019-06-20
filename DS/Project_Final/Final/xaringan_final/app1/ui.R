library(shiny)


rmse=read.csv('rmse.csv')
raw=read.csv('raw.csv')



pageWithSidebar(
  headerPanel('멜론 역주행곡 분석 '),
  sidebarPanel(
    selectInput('xcol', '제목을 선택하세요 ', head(rmse)$title))
  ,
  mainPanel(
    plotOutput('plot1')
  )
)


