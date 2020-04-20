# URL_Sentiment-Analysis
Crawl URL, Term Freq plot, Word Cloud, Emotion Analysis &amp; Save all charts to WD 


```{r load required libraries, echo=TRUE, warning=FALSE, message=FALSE, eval=TRUE}

# Clear Environment 
rm(list = ls())

# Load Required Libraries 
library(rvest)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(SemNetDictionaries)
library(SemNetCleaner)
library(SentimentAnalysis)
library(xml2)
library(wordcloud)
library(wordcloud2)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(syuzhet)
library(tibble)

```


```{r define function, echo=TRUE, warning=FALSE, message=TRUE, eval=TRUE}

wordfreq <- function(pagedata,company){
  pagedata <- html_text(pagedata.html)                  # Convert url data to text 
  pagedata <- tolower(pagedata)                         # convert to lower case
  pagedata <- removePunctuation(pagedata)               # remove all punctuations mark 
  pagedata <- removeNumbers(pagedata)                   # remove all numbers 
  pagedata <- stripWhitespace(pagedata)                 # remove whitespaces
  pagedata <- removeWords(pagedata,stopwords("en"))     # remove stopwords
  pagedata <- gsub(' +',' ',pagedata)                   # replace double spacing with single
  
  char_vec <- unlist(strsplit(pagedata, split = ' '))   # Create character vector
  char_vec <- char_vec[char_vec!= ""]                   # remove empty spaces
  char_vec <- na.omit(char_vec)                         # remove 'na' values
  char_vec <- as.data.frame(char_vec)                   # Convert to data frame 
  names(char_vec)[1] <- paste("keywords")               # rename Column
  char_vec$keywords <- as.character(char_vec$keywords)  # Convert to characters
  
  corpus <- Corpus(VectorSource(char_vec$keywords))     # turn into corpus for further processing
  tdm <- TermDocumentMatrix(corpus)                     # create term document matric from the corpus
  
  term_freq <- rowSums(as.matrix(tdm))                  # Calculate Frequency of various terms
  term_freq <- sort(term_freq, decreasing = T)          # Sort term_frequency in descending order
  
  print(head(term_freq))
  
  # Save CSV of Term Frequency Table
  filetosave <- term_freq
  name_csv <- paste(company,'_termfreq','.csv')
  write.csv(filetosave, file = name_csv)

  # Create & Save Bar Plot 
  barplot(
    term_freq[1:20],
    col = rainbow(20),
    las = 2,
    ylab = "Word Frequency",
    main = paste(url),
    horiz = F
  )
  
  name_bar <- paste(company,'_wordfreq','.png')
  png(name_bar)  
    barplot(
      term_freq[1:20],
      col = rainbow(20),
      las = 2,
      ylab = "Word Frequency",
      main = paste(url),
      horiz = F
    )
  dev.off()
  

  # Create and Save Word Cloud
  word_freqs <- data.frame(term = names(term_freq),     # Create data frame from term frequency
                           num = term_freq)

  pal <- brewer.pal(8,"Dark2")                          # Decide on a color theme 
  wordcloud(word_freqs$term,word_freqs$num,max.words = 20,colors = pal,random.order=FALSE)   # Wordcloud 


  name_wrdcld <- paste(company,'_wordcloud','.png')
  png(name_wrdcld,width = 15,height = 10,units = "in",res = 300)
  wordcloud(word_freqs$term,word_freqs$num,max.words = 20,colors = pal,random.order=FALSE)
  dev.off()

  
  # Emotion Analysis

  name_emot <- paste(company,'_emotion','.png')
  title_emot <- paste(company,'- Emotion Analysis')

  senti1 <- get_nrc_sentiment(pagedata)
  senti2 <- as.data.frame(colSums(senti1))
  senti3 <- rownames_to_column(senti2)
  colnames(senti3) <- c("emotion", "count")

 senti_plot <- ggplot(senti3, aes(x = emotion, y = count, fill = emotion)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position="none",       panel.grid.major = element_blank()) + labs( x = "Emotion", y = "Total Count") + ggtitle(title_emot) + theme(plot.title = element_text(hjust=0.5))

 ggsave(name_emot,senti_plot) 
 return(senti_plot)

}
