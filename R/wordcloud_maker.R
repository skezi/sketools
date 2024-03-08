
wordcloud_maker = function (df,var) {
  
  text = df[,var]
  docs = VCorpus(VectorSource(text))
  
  #Replace special characters -----------------------------------------------------
  toSpace = content_transformer(function(x, pattern)gsub(pattern, " ", x))
  docs = tm_map(docs, toSpace, "/")
  docs = tm_map(docs, toSpace, "@")
  docs = tm_map(docs, toSpace, "'")
  

  docs = tm_map(docs, removePunctuation)
  
  docs = tm_map(docs, removeNumbers)
  
  docs = tm_map(docs, content_transformer(tolower))

  docs = tm_map(docs, stripWhitespace)
  
  
  #Term_Document Matrix ----------------------------------------------------------
  
  dtm = TermDocumentMatrix(docs)
  m = as.matrix(dtm)
  v = sort(rowSums(m), decreasing = TRUE)
  d = data.frame(word = names(v), freq = v)
  
  #Wordcloud ---------------------------------------------------------------------
  
  png(paste0("plot/",var,".png"), width = 430, height = 430)
  
  my_plot = wordcloud(d$word,
                      d$freq,
                      c(3,.3),
                      max.words = 100,
                      random.order=FALSE,
                      rot.per=0.1,
                      use.r.layout=FALSE,
                      min.freq = 1,
                      colors=brewer.pal(8, "Dark2"))
  
  dev.off()
  
  return(my_plot)
  
}
