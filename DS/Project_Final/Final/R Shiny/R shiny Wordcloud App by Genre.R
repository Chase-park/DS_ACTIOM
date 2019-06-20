######R Shiny Wordcloud app by Genre

library(memoise)
library(shiny)
library(stringr)

fileConn<-file("Ballad.txt")
write(paste(Ballad$lyrics), fileConn)
close(fileConn)

fileConn<-file("Hiphop.txt")
write(paste(Hiphop$lyrics), fileConn)
close(fileConn)

fileConn<-file("Dance.txt")
write(paste(Dance$lyrics), fileConn)
close(fileConn)

fileConn<-file("Drama.txt")
write(paste(Drama$lyrics), fileConn)
close(fileConn)

fileConn<-file("RnB.txt")
write(paste(RnB$lyrics), fileConn)
close(fileConn)

fileConn<-file("Rock.txt")
write(paste(Rock$lyrics), fileConn)
close(fileConn)




###Global
# The list of valid books
genres <<- list("Ballad" = "Ballad",
                "Hiphop" = "Hiphop",
                "Dance" = "Dance",
                "Drama" = "Drama",
                "RnB" = "RnB",
                "Rock" ="Rock")


getTermMatrix <- memoise(function(genre){
  
  if (!(genre %in% genres))
    stop("Unknown genre")
  
  text <- readLines(sprintf("./%s.txt", genre))
  noun <- sapply(text,extractNoun, USE.NAMES=F)
  noun2 <- unlist(noun)
  noun3 <- noun2[nchar(noun2)>=2] #한글자 명사 제거
  noun4 <- gsub("\\d+", "", noun3) #숫자 제거
  noun4 <- gsub("\\.", "", noun4) #점(.) 제거
  noun4 <- gsub(",", "", noun4) #점(.) 제거
  noun4 <- gsub("'","", noun4)
  wordcount <- table(noun4) 
  
  sort(wordcount, decreasing=T)
  
})


###Server

server=function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(20,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"),random.order=FALSE)
    
  })
}


###UI

ui=fluidPage(
  # Application title
  titlePanel("Word Cloud of Lyrics By Genre"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a Genre:",
                  choices = genres),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 5),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    # Show Word Cloud
    mainPanel(
      plotOutput("plot",width = "1000px", height = "1000px")
    )
  )
)



shinyApp(ui = ui, server = server)





