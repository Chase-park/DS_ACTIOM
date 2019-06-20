#####Co-Occurence Matrix and Network Analysis of 역주행곡
##### Co-Occurence Analysis


##Ballad
rev<-as.data.frame(fread("Song_list.csv"))
rev<-rev[which(rev$rev==1),]

lyrics<-rev$lyrics
lyrics<-gsub("you","",lyrics)
lyrics<-gsub("when","",lyrics)
lyrics<-gsub("with","",lyrics)
lyrics<-gsub("I'm","",lyrics)
lyrics<-gsub("기분은","",lyrics)
lyrics<-gsub("why","",lyrics)
lyrics<-gsub("같아","",lyrics)
lyrics<-gsub("듯해","",lyrics)

Corpus1<-VCorpus(VectorSource(extractNoun(lyrics)))
Corpus1<-tm_map(Corpus1,stripWhitespace)


TDM1<-TermDocumentMatrix(Corpus1,control=list(removePunctuation=TRUE,
                                              stopwords=TRUE,
                                              removeNumbers=TRUE,
                                              wordLengths=c(2,Inf)))



tdm.matrix1<-as.matrix(TDM1)

word.count1=rowSums(tdm.matrix1)
word.order1=order(word.count1,decreasing=TRUE)
freq.word1=tdm.matrix1[word.order1[1:44],]
rownames(tdm.matrix1)[word.order1[1:44]]

co.matrix=freq.word1%*%t(freq.word1)
co.matrix


###Network Analysis
#install.packages("qgraph")
library(qgraph)

##Visualizing Network
vis<-qgraph(co.matrix,labels=rownames(co.matrix),
            diag=F,
            layout='spring',
            edge.color='blue',
            vsize=log(diag(co.matrix))*2.5)

plot(vis,main="Network Visualization")




















