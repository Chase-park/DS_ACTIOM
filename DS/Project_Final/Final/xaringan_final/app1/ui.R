library(shiny)


rmse=read.csv('rmse.csv')
raw=read.csv('raw.csv')



pageWithSidebar(
  headerPanel('��� ������� �м� '),
  sidebarPanel(
    selectInput('xcol', '������ �����ϼ��� ', head(rmse)$title))
  ,
  mainPanel(
    plotOutput('plot1')
  )
)

