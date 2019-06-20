################MELON LYRICS TEXT MINING
################2013122013 임지성



###Datasest
library(data.table)
data<-fread("melonchart 2018.csv",header=TRUE,encoding='UTF-8')
head(data)
mode(data)

data1<-as.data.frame(cbind(data$title,data$genre,data$id,data$lyrics))
colnames(data1)<-c("title","genre","id","lyrics")

data2<-unique(data1)
nrow(data1)
nrow(data2)


###Visualizing Lyrics by Genre
table(data2$genre)

library(rvest)
library(KoNLP)
library(tm)
buildDictionary(ext_dic = "woorimalsam")




Ballad=data2[which(data2$genre=="Ballad"),]
Hiphop=data2[which(data2$genre=="Hip-hop"),]
Dance=data2[which(data2$genre=="Dance"),]
Drama=data2[which(data2$genre=="Drama"),]
RnB=data2[which(data2$genre=="R&B"),]
Rock=data2[which(data2$genre=="Rock"),]






###Frequency Analysis

par(mfrow=c(2,3))

##Ballad
Balladnoun1 <- extractNoun(paste(Ballad$lyrics))
Balladnoun1 <- unlist(Balladnoun1)
Balladnoun1 <- Balladnoun1[nchar(Balladnoun1)>=2] #한글자 명사 제거
Balladnoun1 <- gsub("들이","둘이",Balladnoun1)


wordcount1 <- table(Balladnoun1) 
temp1 <- sort(wordcount1, decreasing=T)[1:20]
temp1 <- temp1[-1] 
temp1 <- as.data.frame(temp1)


##Hiphop
Hiphopnoun1 <- extractNoun(paste(Hiphop$lyrics))
Hiphopnoun1 <- unlist(Hiphopnoun1)
Hiphopnoun1 <- Balladnoun1[nchar(Hiphopnoun1)>=2] #한글자 명사 제거



wordcount2 <- table(Hiphopnoun1) 
temp2 <- sort(wordcount2, decreasing=T)[1:20]
temp2 <- temp2[-1] 
temp2 <- as.data.frame(temp2)



##Dance
Dancenoun1 <- extractNoun(paste(Dance$lyrics))
Dancenoun1 <- unlist(Dancenoun1)
Dancenoun1 <- Dancenoun1[nchar(Dancenoun1)>=2] #한글자 명사 제거



wordcount3 <- table(Dancenoun1) 
temp3 <- sort(wordcount3, decreasing=T)[1:20]
temp3 <- temp3[-1] 
temp3 <- as.data.frame(temp3)




##Drama
Dramanoun1 <- extractNoun(paste(Drama$lyrics))
Dramanoun1 <- unlist(Dramanoun1)
Dramanoun1 <- Dramanoun1[nchar(Dramanoun1)>=2] #한글자 명사 제거



wordcount4 <- table(Dramanoun1) 
temp4 <- sort(wordcount4, decreasing=T)[1:20]
temp4 <- temp4[-1] 
temp4 <- as.data.frame(temp4)



##RnB
RnBnoun1 <- extractNoun(paste(RnB$lyrics))
RnBnoun1 <- unlist(RnBnoun1)
RnBnoun1 <- RnBnoun1[nchar(RnBnoun1)>=2] #한글자 명사 제거



wordcount5 <- table(RnBnoun1) 
temp5 <- sort(wordcount5, decreasing=T)[1:20]
temp5 <- temp5[-1] 
temp5 <- as.data.frame(temp5)



##Rock
Rocknoun1 <- extractNoun(paste(Rock$lyrics))
Rocknoun1 <- unlist(Rocknoun1)
Rocknoun1 <- Rocknoun1[nchar(Rocknoun1)>=2] #한글자 명사 제거



wordcount6 <- table(Rocknoun1) 
temp6 <- sort(wordcount6, decreasing=T)[1:20]
temp6 <- temp6[-1] 
temp6 <- as.data.frame(temp6)


#Visualize
library(ggplot2)
ggplot(as.data.frame(temp1),aes(x=Balladnoun1,y=Freq))+geom_bar(stat="identity",color="blue",fill="lightblue")+ggtitle("Top 20 frequent words in Ballad")
ggplot(as.data.frame(temp2),aes(x=Hiphopnoun1,y=Freq))+geom_bar(stat="identity",color="blue",fill="lightblue")+ggtitle("Top 20 frequent words in Hiphop")
ggplot(as.data.frame(temp3),aes(x=Dancenoun1,y=Freq))+geom_bar(stat="identity",color="blue",fill="lightblue")+ggtitle("Top 20 frequent words in Dance")
ggplot(as.data.frame(temp4),aes(x=Dramanoun1,y=Freq))+geom_bar(stat="identity",color="blue",fill="lightblue")+ggtitle("Top 20 frequent words in Dance")
ggplot(as.data.frame(temp5),aes(x=RnBnoun1,y=Freq))+geom_bar(stat="identity",color="blue",fill="lightblue")+ggtitle("Top 20 frequent words in RnB")
ggplot(as.data.frame(temp6),aes(x=Rocknoun1,y=Freq))+geom_bar(stat="identity",color="blue",fill="lightblue")+ggtitle("Top 20 frequent words in Rock")




par(mfrow=c(1,1))





##Ballad Word Cloud 2
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

wordcloud2(wordcount1[wordcount1>4],shuffle=FALSE,size=2) # 단어색
