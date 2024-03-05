library(wordcloud)
library(wordcloud2)
library(tm)
library(SnowballC)
library(htmlwidgets)

wordcloud_maker <- function(df, var, extra_stopwords = NULL) {
  text <- df[, var]
  docs <- Corpus(VectorSource(text))
    
  #Replace special characters -----------------------------------------------------
  to_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  docs <- tm_map(docs, to_space, "/")
  docs <- tm_map(docs, to_space, "@")
  docs <- tm_map(docs, to_space, "'")
  
  #Remove punctuation
  docs <- tm_map(docs, removePunctuation)
  
  #Remove french common stopwords
  docs <- tm_map(docs, removeWords, c(stopwords("french") + extra_stopwords))
  
  
  
  #Remove numbers
  docs <- tm_map(docs, removeNumbers)
  
  #Make all lower case
  docs <- tm_map(docs, content_transformer(tolower))
  
  #ELiminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  
  #Term_Document Matrix ----------------------------------------------------------
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  
  #Wordcloud ---------------------------------------------------------------------
  
  set.seed(10)
  
  hw <- wordcloud2(d,size = 2)
  
  saveWidget(hw, "wordcloud_test.html", selfcontained = FALSE)
  webshot::webshot("wordcloud_test.html", paste0("plot/", var, ".png"), delay = 6, vwidth = 595, vheight = 446)
  
}
