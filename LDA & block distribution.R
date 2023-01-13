library(readtext)
library(quanteda)
library(topicmodels)
library(ggplot2)
library(tidyverse)
library(tm)

# converting docx files in a folder into a corpus for LDA topic modeling
lts<- readtext("D:/data_sets/shareholder_letters_calls/shareholder_letter_sp_100_2010_2017/*.docx",
               docvarsfrom="filenames",
               docvarnames=c("firm","year"),
               dvsep="_",
               encoding="UTF-8")
lts$firm_year<-with(lts, paste(firm,year,sep="_"))
corp<-corpus(lts)

# Pre-processing a corpus for effective LDA implementation
corpus <- corpus_reshape(corp,to="sentences")
dfm <- dfm(corpus, control = list(stemming = TRUE, stopwords = TRUE,
                                       minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))
dfm<-dfm_trim(dfm,min_termfreq = 0.25,
              max_termfreq = 0.99,
              termfreq_type = c("quantile"))

# converting document-feature matrix into document-term matrix
dtm = convert(dfm, to = "topicmodels") 
set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 14,  control = list(alpha = 0.1))

# Check out which firm's shareholder letters include which topics through the visualization of block distribution:
letter = docvars(dfm)[match(rownames(dtm), docnames(dfm)),]
sl = aggregate(posterior(m)$topics, by=letter["firm_year"], mean)
rownames(sl) = sl$firm_year
heatmap(as.matrix(sl[-1]))

