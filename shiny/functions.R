#create corpus
q.corpus<- function(x){
    corpus(unlist(segments(x,'sentences')))
  
}




#quantede tokenizer
q.tokenize <- function(x, ngramsize = 1, simplify = T){
  tolower (
      quanteda:: tokens(x,
                          removeNumbers = T ,
                          removePunct = T,
                          remove_symbols = TRUE,
                          remove_separators = TRUE,
                          #remove_twitter = TRUE,
                          remove_hyphens = TRUE,
                          remove_url = TRUE, ngrams = ngramsize,
                          concatenator = " "
      )
  )
}

#clean input
clean_input <- function(inp){
  inp<- corpus(tolower(inp))
  tokens<- tokens(inp,
                    removeURL=T,
                    removeNumbers = T,
                    remove_punct = T,
                    remove_symbols= T,
                    remove_separators = T,
                    remove_twitter = T,
                    remove_hyphens= T,
                    verbose=F)
 # mask<-tokens[[1]] %in% ""
  tokens[[1]]
}




#predict the ngram from the cleaned input
ngram_predict<- function(inp,ngram=4){
  ngram <- min(ngram-1,length(inp))
  inp<-tail(inp,ngram)
  inp<-paste(inp,collapse = " ")
  
  res<- list()
  res$bigram<-  word(inp,-1,-1, sep = fixed(" "))
  res$trigram<- word(inp,-2,-1, sep = fixed(" "))
  res$quatgram<-word(inp,-3,-1, sep = fixed(" "))
    
  res
  
}


#nextWord

nextWord<- function(txt,no_results = 5){
  
  #process input
  txt<-clean_input(txt)
  numWords<- length(txt)
  ngrams<-ngram_predict(txt)
  
    #search the right ngrams
  #unigrams<-readRDS("rds/unigram.rds")
  #bigrams<-readRDS("rds/bigram.rds")
  #trigrams<-readRDS("rds/trigram.rds")
  #quatgrams<-readRDS("rds/quatgram.rds")
  
    if(numWords >=3 &&(!exists("predicion") || nrow(prediction) < no_results) ) {
      quat_pred<- quatgrams[quatgrams$sentence == ngrams$quatgram,]
      if (!exists("prediction")) {
        prediction <- head(quat_pred, no_results)
      } else {
        prediction <- rbind(prediction, head(quat_pred, no_results))  
      }
     
    }   
      if(numWords >= 2 && (!exists("prediction") || nrow(prediction) < no_results)) {
        tri_pred <- trigrams[trigrams$sentence == ngrams$trigram, ]
        if (!exists("prediction")) {
          prediction <- head(tri_pred, no_results)
        } else {
          prediction <- rbind(prediction, head(tri_pred, no_results))  
        }
      }
      if(numWords >=1 &&(!exists("predicion") || nrow(prediction) < no_results)) {
        bi_pred <- bigrams[bigrams$sentence == ngrams$bigram, ]
        if (!exists("prediction")) {
          prediction <- head(bi_pred, no_results)
        } else {
          prediction <- rbind(prediction, head(bi_pred, no_results))  
        }
      }
      
      if (!exists("prediction")) {
        #return(rep("the", 5))
        prediction<-head(unigrams,no_results)
        prediction <- prediction[order(prediction$probability, decreasing = TRUE),]
      } else {
        prediction <- prediction[order(prediction$probability, decreasing = TRUE),]
        #prediction <- prediction$prediction
        #prediction_length <- length(prediction)
        #if (prediction_length < 5) {
        #  prediction <- c(prediction, rep("the", 5 - prediction_length))
        #}
        return(prediction)
      }
  }
     

  


