keyword <- function() {
  
  # Check for packages
  if (!is.element('tm', installed.packages()[,1])) { 
          install.packages('tm')
  } else { 
          library(tm)
  }
  
  if (!is.element('RWeka', installed.packages()[,1])) { 
          install.packages('RWeka')
  } else { 
          library(RWeka)
  }
  
  if (!is.element('SnowballC', installed.packages()[,1])) { 
          install.packages('SnowballC')
  } else { 
          library(SnowballC)
  }

  print('For questions and debug errors, please contact Adrian. :)')

  # Interactive input
  x <- readline(as.character('Input text here: '))
  word <- readline('Input 1 for one-word keyword and 2 for two-word keyphrase: ')
  
  # Two-Phrase tokenizer Function
  twophrase.tokenizer <- function(x) { NGramTokenizer(x, Weka_control(min = 2, max = 2)) }
  
  if (word == 2) {
    x <- Corpus(VectorSource(x))
    x <- tm_map(x, stripWhitespace) # Whitespace
    x <- tm_map(x, tolower) # Convert to lowercase
    x <- tm_map(x, removePunctuation) # Remove punctuation
    x <- tm_map(x, removeNumbers) # Remove numbers
    x <- tm_map(x, removeWords, stopwords(kind='english')) # Remove english stop words
    x <- tm_map(x, removeWords, stopwords(kind='german')) # Remove german stop words
    x <- tm_map(x, removeWords, stopwords(kind='spanish')) # Remove spanish stop words
    x <- tm_map(x, removeWords, stopwords(kind='SMART')) # Remove smart stop words
    x <- tm_map(x, stemDocument) # Stemming
    
    tdm <- TermDocumentMatrix(x, control = list(tokenize = twophrase.tokenizer))
    key <- sort(rowSums(as.matrix(tdm)), decreasing = T) # Sum of frequency of words
    key.df <- data.frame(keyword = names(key), freq = key) # Convert keyword frequency to DF
    
    print(head(key.df, n=20))
    write.csv(key.df, file='./2-Word Keyphrase.csv')
    print('The two-word keyphrase file has been saved in your working directory.')
    
    } else if (word == 1) {
      x <- Corpus(VectorSource(x))
      x <- tm_map(x, stripWhitespace) # Whitespace
      x <- tm_map(x, tolower) # Convert to lowercase
      x <- tm_map(x, removePunctuation) # Remove punctuation
      x <- tm_map(x, removeNumbers) # Remove numbers
      x <- tm_map(x, removeWords, stopwords(kind='english')) # Remove english stop words
      x <- tm_map(x, removeWords, stopwords(kind='german')) # Remove german stop words
      x <- tm_map(x, removeWords, stopwords(kind='spanish')) # Remove spanish stop words
      x <- tm_map(x, removeWords, stopwords(kind='SMART')) # Remove smart stop words
      x <- tm_map(x, stemDocument) # Stemming
      
      tdm <- TermDocumentMatrix(x)
      key <- sort(rowSums(as.matrix(tdm)), decreasing = T) # Sum of frequency of words
      key.df <- data.frame(keyword = names(key), freq = key) # Convert keyword frequency to DF
      
      print(head(key.df, n=20))
      write.csv(key.df, file='./1 Word Keyword.csv')
      print('The one-word keyword file has been saved in your working directory.')
      
    } else { print('Please input 1 or 2 only.') }
}