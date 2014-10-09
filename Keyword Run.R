# Code block to install packages
is.installed <- function(x) {
        is.element(x, installed.packages()[,1])
} 

ifelse(!is.installed('tm'), install.packages('tm'), require(tm))
ifelse(!is.installed('RWeka'), install.packages('RWeka'), require(RWeka))

twophrase.tokenizer <- function(x) { 
        NGramTokenizer(x, Weka_control(min = 2, max = 2)) 
}

keyword <- read.csv(file='clipboard', sep='\t', header=F)
keyword <- Corpus(DataframeSource(keyword))
keyword <- tm_map(keyword, stripWhitespace) # Whitespace
# keyword <- tm_map(keyword, tolower) # Convert to lowercase - temp broken after update
keyword <- tm_map(keyword, removePunctuation) # Remove punctuation
keyword <- tm_map(keyword, removeNumbers) # Remove numbers
keyword <- tm_map(keyword, removeWords, stopwords(kind='en')) # Remove stop words
keyword <- tm_map(keyword, removeWords, stopwords(kind='SMART')) # Remove stop words
keyword <- tm_map(keyword, stemDocument) # Stemming

# Build Term Document Matrix - using 2 phrase tokenizer
tdm <- TermDocumentMatrix(keyword, control = list(tokenize = twophrase.tokenizer))
frequency <- sort(rowSums(as.matrix(tdm)), decreasing = T)
frequency.df <- data.frame(keyword = names(frequency), freq = frequency) 
head(frequency.df, n=100)

dtm <- DocumentTermMatrix(keyword, control = list(tokenize = twophrase.tokenizer))
dtm.df <- as.matrix(dtm)
write.csv(dtm.df, file="DTM.csv")