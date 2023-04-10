#Project 1
#Task 4

##Find any lyrics and save it to csv file. 
#i. Perform data cleaning/preprocessing such as remove punctuation, remove stop words, etc. 
#ii. Convert to document term matrix and find the frequency. Tabulate the frequency and its 
#corresponding terms (at least five terms). 
#iii. Represent your terms in the form of word cloud of any shape (exclude "star" as it have been shown in the class).
#iv. Write short essay which includes the following:

#-Introduction of the lyrics (why was it chosen)
#-Discussion on the finding from part 5ii and 5iii above.
#-Conclusion
#Your short essay must be at least 300 words.

#Drivers license_Olivia Rodrigo's lyric

#1) Import dataset
lyric<- read.csv(file.choose(),header = F)

str(lyric)

lyric[30,]

#2) Loading packages
library(tm)
library(NLP)
library(stringr)

#3) Data Cleaning
docs<-Corpus(VectorSource(lyric))
writeLines(as.character(docs))
writeLines(as.character(docs[[1]]))
getTransformations()
as.character(docs[[1]])

#Remove unnecessary stuff
STOPwords<-c("cause","still","okay","ive","didnt","werent","now","and","cant","ooh","one")
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeWords,c(stopwords("english"),STOPwords)) #remove stop words
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
writeLines(as.character(docs))
as.character(docs[[1]])
as.character(docs)

#stemDocument
library(SnowballC)
docs2<-tm_map(docs,stemDocument)
writeLines(as.character(docs))
writeLines(as.character(docs2))

#4)Sentiment Analysis

dtm<-DocumentTermMatrix(docs)
freq<-colSums(as.matrix(dtm))
length(freq)
ord<-order(freq,decreasing=T)
head(ord)

#Insert into data frame 
df<-data.frame(names(freq),freq)
names(df)<-c("TERM","FREQ")
head(df,10)

findFreqTerms(dtm,lowfreq = 10)
findAssocs(dtm,"get",0.3)
max(df$FREQ,select=df$TERM)
?findFreqTerms

#5) Data Visualisation
library(ggplot2)
subs<-subset(df,FREQ>=10)
ggplot(subs,aes(x=TERM,y=FREQ))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggplot(df,aes(x=TERM,y=FREQ))+geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1)) #show all include terms that hv small freq

library(wordcloud)
wordcloud(names(freq),freq) #in general
wordcloud(names(freq),freq.min.freq=15)
wordcloud(names(freq),freq,colors=brewer.pal(8,"Blues"))
wordcloud(names(freq),freq,colors=brewer.pal(12,"Paired"))
#the bigger word has highest frequency

library(wordcloud2) #can give frequency ,diff with wordcloud
wordcloud2(df)
wordcloud2(df,size=0.5)
wordcloud2(df,size=0.5,color="random-dark",backgroundColor = "black")
wordcloud2(df,shape="diamond",size=0.5)
