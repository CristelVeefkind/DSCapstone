# global.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Shiny script for loading data into global environment
# 2016-02-05

# Libraries and options ####
library(shiny)
library(tm)
library(quanteda)
library(stringr)
library(wordcloud)
library(RColorBrewer)



#set directory
setwd("~/Documents/DS_final/shiny")
# Load data  ####
#search the right ngrams
unigrams<-readRDS("../rds/unigram.rds")
unigrams$senctence<- unigrams$ngram
unigrams$prediction<- unigrams$ngram
bigrams<-readRDS("../rds/bigram.rds")
trigrams<-readRDS("../rds/trigram.rds")
quatgrams<-readRDS("../rds/quatgram.rds")

#load functions
source("functions.R")