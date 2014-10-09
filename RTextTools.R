library(RTextTools)

keyword <- read.csv(file='clipboard', sep='\t', header=F)
keyword <- create_matrix(keyword, 
                        stemWords = T, 
                        language = 'english',
                        removeStopwords = T,
                        removePunctuation = T,
                        removeNumbers = T,
                        minWordLength = 3,
                        stripWhitespace = T,
                        toLower = T, 
                        weighting=tm::weightTfIdf)

keyword.df <- as.matrix(keyword)
write.csv(keyword.df, file="DTM.csv")



keyword <- read.csv(file='clipboard', sep='\t', header=F)
keyword <- Corpus(DataframeSource(keyword))
keyword <- tm_map(keyword, stripWhitespace) # Whitespace
# keyword <- tm_map(keyword, tolower) # Convert to lowercase - temp broken after update
keyword <- tm_map(keyword, removePunctuation) # Remove punctuation
keyword <- tm_map(keyword, removeNumbers) # Remove numbers
keyword <- tm_map(keyword, removeWords, stopwords(kind='en')) # Remove stop words
keyword <- tm_map(keyword, removeWords, stopwords(kind='SMART')) # Remove stop words
keyword <- tm_map(keyword, stemDocument) # Stemming