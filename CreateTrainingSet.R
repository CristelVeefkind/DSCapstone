library(readtext)
library(R.utils)
library(tm)
library(reshape2)
library(ggplot2)
#library(RWeka)
library(dplyr)
library(tidytext)
library(ngram)
#library(stopwords)
library(tau)
library(quanteda)
library(data.table)
library(caTools)
library(tidyr)
library(readr)
library(stringr)

#read in the datasets and sample
#blogs
conBl<- file("en_US/en_US.blogs.txt")
fullBl<- readLines(conBl)
close(conBl)
set.seed(1234)
Tfraction <- 0.15
Vfraction <- 0.17
Trfraction<- 0.14

#built indexes for all parts
testIndex<- rbinom(length(fullBl),1,Tfraction)
val_trainIndex<- 1-testIndex
sub_valIndex<- rbinom(length(fullBl),1,Vfraction)
valIndex<- val_trainIndex*sub_valIndex
trainIndex<- 1 - valIndex
trainsample_subindex<- rbinom(length(fullBl),1,Trfraction)
trainsample_index<- trainIndex*trainsample_subindex

#read the lines of the sets and write them to csv
conBlTest<- file("sampled/test/blogtest.csv","w")
for (i in 1:length(fullBl)){
  if (testIndex[i]==1) { cat(fullBl[i],file = conBlTest,sep = "\n")}
  
}
close(conBlTest)

conBlVal<- file("sampled/val/blogval.csv","w")
for (i in 1: length(fullBl)){
  if (valIndex[i]==1) { cat(fullBl[i],file = conBlVal,sep = "\n")}
  
}
close(conBlVal)

conBlTrain<- file("sampled/train/blogtrain.csv","w")
for (i in 1: length(fullBl)){
  if (trainIndex[i]==1) { cat(fullBl[i],file = conBlTrain,sep = "\n")}
  
}
close(conBlTrain)

#train random saple
conBlTrains<- file("sampled/train/sample/blogtrains.csv","w")
for (i in 1: length(fullBl)){
  if (trainsample_index[i]==1) { cat(fullBl[i],file = conBlTrains,sep = "\n")}
  
}
close(conBlTrains)
rm(fullBl)


#tweets
conTw<- file("en_US/en_US.twitter.txt")
fullTw<- readLines(conTw)

conTwTest<- file("sampled/test/tweettest.csv","w")
for (i in 1:length(fullTw)){
  if (testIndex[i]==1) { cat(fullTw[i],file = conTwTest,sep = "\n")}
  
}
close(conTwTest)

conTwVal<- file("sampled/val/tweetval.csv","w")
for (i in 1: length(fullTw)){
  if (valIndex[i]==1) { cat(fullTw[i],file = conTwVal,sep = "\n")}
  
}
close(conTwVal)

conTwTrain<- file("sampled/train/tweettrain.csv","w")
for (i in 1: length(fullTw)){
  if (trainIndex[i]==1) { cat(fullTw[i],file = conTwTrain,sep = "\n")}
  
}
close(conTwTrain)


#sample of the train
conTwTrains<- file("sampled/train/sample/tweettrains.csv","w")
for (i in 1: length(fullTw)){
  if (trainsample_index[i]==1) { cat(fullTw[i],file = conTwTrain,sep = "\n")}
  
}
close(conTwTrains)
rm(fullTw)

#news
conNe<- file("en_US/en_US.news.txt")
fullNe<- readLines(conNe)
close(conNe)
conNeTest<- file("sampled/test/newstest.csv","w")
for (i in 1:length(fullNe)){
  if (testIndex[i]==1) { cat(fullNe[i],file = conNeTest,sep = "\n")}
  
}
close(conNeTest)

conNeVal<- file("sampled/val/newsval.csv","w")
for (i in 1: length(fullNe)){
  if (valIndex[i]==1) { cat(fullNe[i],file = conNeVal,sep = "\n")}
  
}
close(conNeVal)

conNeTrain<- file("sampled/train/newstrain.csv","w")
for (i in 1: length(fullNe)){
  if (trainIndex[i]==1) { cat(fullNe[i],file = conNeTrain,sep = "\n")}
  
}
close(conNeTrain)

#sample news
conNeTrains<- file("sampled/train/sample/newstrain.csv","w")
for (i in 1: length(fullNe)){
  if (trainsample_index[i]==1) { cat(fullNe[i],file = conNeTrains,sep = "\n")}
  
}
close(conNeTrains)

rm(fullNe)





#build corpus and divide into train test/validate
corpus<-Corpus(DirSource(directory="Sampled/train/sample"))
corpus<- quanteda:: corpus(corpus)


#build ngrams and write them to rds


unigram <- dfm(corpus, concatenator = " ", ngrams = 1,
               tolower = TRUE, remove_punct = TRUE,remove_symbols = TRUE,
               remove_separators = TRUE,
               #remove_twitter = TRUE,
               remove_hyphens = TRUE,
               remove_url = TRUE
              )

bigram <- dfm(corpus, concatenator = " ", ngrams = 2,
              tolower = TRUE, remove_punct = TRUE,remove_symbols = TRUE,
              remove_separators = TRUE,
              #remove_twitter = TRUE,
              remove_hyphens = TRUE,
              remove_url = TRUE
)

trigram <- dfm(corpus, concatenator = " ", ngrams = 3,
               tolower = TRUE, remove_punct = TRUE,remove_symbols = TRUE,
               remove_separators = TRUE,
               #remove_twitter = TRUE,
               remove_hyphens = TRUE,
               remove_url = TRUE
)

quatgram <- dfm(corpus, concatenator = " ", ngrams = 4,
               tolower = TRUE, remove_punct = TRUE,remove_symbols = TRUE,
               remove_separators = TRUE,
               #remove_twitter = TRUE,
               remove_hyphens = TRUE,
               remove_url = TRUE
)

#pentgram <- dfm(corpus, concatenator = " ", ngrams = 5,
#               tolower = TRUE, remove_punct = TRUE,remove_symbols = TRUE,
#               remove_separators = TRUE,
#               #remove_twitter = TRUE,
#               remove_hyphens = TRUE,
#               remove_url = TRUE
#)

unigramF <- data.frame(ngram = unigram@Dimnames$features, frequency = colSums(unigram),stringsAsFactors = FALSE)
unigramF$probability<-unigramF$frequency/sum(unigramF$frequency)
unigramF <- subset(unigramF, frequency >= 5)
unigramF <- arrange(unigramF, desc(frequency))
#write to rds for later use
write_rds(unigramF,"rds/unigram.rds")

bigramF <- data.frame(ngram = bigram@Dimnames$features, frequency = colSums(bigram),stringsAsFactors = FALSE)
bigramF$probability<-bigramF$frequency/sum(bigramF$frequency)
bigramF <- subset(bigramF, frequency >= 5)
bigramF <- arrange(bigramF, desc(frequency))
bigramF<-bigramF %>%
  separate(ngram,c("sentence","prediction")," ",remove=FALSE)
#write to rds for later use
write_rds(bigramF,"rds/bigram.rds")

trigramF <- data.frame(ngram = trigram@Dimnames$features, frequency = colSums(trigram),stringsAsFactors = FALSE)
trigramF$probability<-trigramF$frequency/sum(trigramF$frequency)
trigramF <- subset(trigramF, frequency >= 5)
trigramF <- arrange(trigramF, desc(frequency))
trigramF<-trigramF %>%
  separate(ngram,c("word1","word2","prediction")," ",remove=FALSE)
trigramF$sentence <- paste(trigramF$word1, trigramF$word2)
trigramF<- trigramF[,!names(trigramF) %in% c("word1","word2")]
#write to rds for later use
write_rds(trigramF,"rds/trigram.rds")

quatgramF <- data.frame(ngram = quatgram@Dimnames$features, frequency = colSums(quatgram),stringsAsFactors = FALSE)
quatgramF$probability<-quatgramF$frequency/sum(quatgramF$frequency)
quatgramF <- subset(quatgramF, frequency >= 5)
quatgramF <- arrange(quatgramF, desc(frequency))
quatgramF<-quatgramF %>%
  separate(ngram,c("word1","word2","word3","prediction")," ",remove=FALSE)
quatgramF$sentence <- paste(quatgramF$word1, quatgramF$word2, quatgramF$word3)
quatgramF<- trigramF[,!names(quatgramF) %in% c("word1","word2","word3")]
#write to rds for later use
write_rds(quatgramF,"rds/quatgram.rds")


