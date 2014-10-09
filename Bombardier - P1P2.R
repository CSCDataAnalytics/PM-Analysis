########################### Libraries ########################### 
# Text Analysis
library(tm)
library(RWeka)

# Visualization
library(wordcloud)
cloudcolor <- brewer.pal(8, "Paired")

# Data preparation
library(xlsx)

##### Two-Phrase tokenizer Function ####
twophrase.tokenizer <- function(x) { NGramTokenizer(x, Weka_control(min = 2, max = 2)) }

# Load files
bombardier <- read.csv(file='Bombardier.csv', sep=',', header=T)
bombardier.p1p2 <- subset(bombardier, SysPri=='1-Critical' | SysPri=='2-High')
bombardier.p3p4 <- subset(bombardier, SysPri=='3-Medium' & Sample==1 | SysPri=='4-Low' & Sample==1)
bombardier.stop <- read.csv(file='Bombardier - Stopwords.csv', sep=',', header=T)

########################### TM package ########################### 

# Build corpus (Description) - no stemming applied - P1 and P2
bombardier.corpus <- Corpus(VectorSource(bombardier.p1p2$Descript))
bombardier.corpus <- tm_map(bombardier.corpus, stripWhitespace) # Whitespace
bombardier.corpus <- tm_map(bombardier.corpus, tolower) # Convert to lowercase
bombardier.corpus <- tm_map(bombardier.corpus, removePunctuation) # Remove punctuation
bombardier.corpus <- tm_map(bombardier.corpus, removeNumbers) # Remove numbers
bombardier.corpus <- tm_map(bombardier.corpus, removeWords, stopwords(kind='en')) # Remove stop words
bombardier.corpus <- tm_map(bombardier.corpus, removeWords, stopwords(kind='SMART')) # Remove stop words
bombardier.corpus <- tm_map(bombardier.corpus, removeWords, bombardier.stop) # Remove additional stop words
bombardier.corpus <- tm_map(bombardier.corpus, stemDocument) # Stemming

# Build corpus (Description) - no stemming applied - P3 and P4
bombardier.corpus <- Corpus(VectorSource(bombardier.p3p4$Descript))
bombardier.corpus <- tm_map(bombardier.corpus, stripWhitespace) # Whitespace
bombardier.corpus <- tm_map(bombardier.corpus, tolower) # Convert to lowercase
bombardier.corpus <- tm_map(bombardier.corpus, removePunctuation) # Remove punctuation
bombardier.corpus <- tm_map(bombardier.corpus, removeNumbers) # Remove numbers
bombardier.corpus <- tm_map(bombardier.corpus, removeWords, stopwords(kind='en')) # Remove stop words
bombardier.corpus <- tm_map(bombardier.corpus, removeWords, stopwords(kind='german')) # Remove stop words
bombardier.corpus <- tm_map(bombardier.corpus, removeWords, stopwords(kind='SMART')) # Remove stop words
bombardier.corpus <- tm_map(bombardier.corpus, removeWords, bombardier.stop) # Remove additional stop words
bombardier.corpus <- tm_map(bombardier.corpus, stemDocument) # Stemming

# Build Term Document Matrix - using 1 word only
bombardier.tdm <- TermDocumentMatrix(bombardier.corpus)
bombardier.frequency <- sort(rowSums(as.matrix(bombardier.tdm)), decreasing = T) # Sum of frequency of words
bombardier.frequency.df <- data.frame(keyword = names(bombardier.frequency), freq = bombardier.frequency) # Convert keyword frequency to DF
write.csv(bombardier.frequency.df, file='./Bombardier p1p2 - 1 word.csv')

# Build Term Document Matrix - using 2 phrase tokenizer - P1 and P2
bombardier.tdm <- TermDocumentMatrix(bombardier.corpus, control = list(tokenize = twophrase.tokenizer))
bombardier.frequency <- sort(rowSums(as.matrix(bombardier.tdm)), decreasing = T) # Sum of frequency of words
bombardier.frequency.df <- data.frame(keyword = names(bombardier.frequency), freq = bombardier.frequency) # Convert keyword frequency to DF
write.csv(bombardier.frequency.df, file='./Bombardier p1p2 - 2 words.csv')

# Build Term Document Matrix - using 2 phrase tokenizer - P3 and P4
bombardier.tdm <- TermDocumentMatrix(bombardier.corpus, control = list(tokenize = twophrase.tokenizer))
bombardier.frequency <- sort(rowSums(as.matrix(bombardier.tdm)), decreasing = T) # Sum of frequency of words
bombardier.frequency.df <- data.frame(keyword = names(bombardier.frequency), freq = bombardier.frequency) # Convert keyword frequency to DF
write.csv(bombardier.frequency.df, file='./Bombardier p3p4 - 2 words.csv')

# Visualize using wordcloud
wordcloud(bombardier.frequency.df$keyword, bombardier.frequency.df$freq, scale=c(8,.2), 
          min.freq=1, max.words=Inf, random.order=F, rot.per=.1, colors=cloudcolor)

# Build Document Term Matrix
bombardier.dtm <- DocumentTermMatrix(bombardier.corpus)

bombardier.dtm.dense = melt(bombardier.dtm, value.name = "count")

# Build Document Frequency based on DTM
bombardier.dtm.weight <- weightTfIdf(bombardier.dtm)
bombardier.dtm.weight.matrix <- as.matrix(bombardier.dtm.weight)

# Clustering using Euclidean - warning, big matrix takes a while to calculate
bombardier.distancematrix <- dist(bombardier.dtm.weight.matrix, method='euclidean')
bombardier.tree <- hclust(bombardier.distancematrix, method='ward')
plot(bombardier.tree)

bombardier.tree <- cutree(bombardier.tree, k=5) # cut tree into 5 clusters

########################### RTextTools ########################### 

# Create corpus without stemming - count using term frequency
bombardier.corpus <- create_matrix(bombardier.p1p2$Description, 
                                   stemWords = F, 
                                   language = 'english',
                                   removeStopwords = T,
                                   removePunctuation = T,
                                   removeNumbers = T,
                                   toLower = T,
                                   stripWhitespace = T,
                                   minWordLength = 3,
                                   ngramLength = 2,
                                   weighting = tm::weightTf)

# Frequency count
bombardier.simpleparse <- as.simple_sparse_array(bombardier.corpus)
bombardier.frequency <- sparseMatrix(i=bombardier.corpus$i, j=bombardier.corpus$j, 
                                     x=bombardier.corpus$v, dimnames=bombardier.corpus$dimnames,
                                     dims=c(bombardier.corpus$nrow, bombardier.corpus$ncol))
bombardier.frequency.sum <- as.data.frame(bombardier.frequency, stringsAsFactors=F)
bombardier.frequency.sum <- readMat(bombardier.frequency)

# For text regression
bombardier.array <- as.array(bombardier.corpus)




df.rtexttools.summary <- readMat(df.rtexttools.matrix)

bombardier.frequency <- sort(rowSums(as.matrix(bombardier.corpus)), decreasing = T) # Sum of frequency of words
bombardier.frequency.df <- data.frame(keyword = names(bombardier.frequency), freq = bombardier.frequency) # Convert keyword frequency to DF

# Classify Models
bombardier.container <- create_container(bombardier.corpus, bombardier.p1p2$Category, trainSize=1:75, testSize=76:100, virgin=F)
bombardier.models <- train_models(bombardier.container, algorithms=c("MAXENT","SVM"))
bombardier.results <- classify_models(bombardier.container, bombardier.models)

# Create Analytics
bombardier.analytics <- create_analytics(bombardier.container, bombardier.results)


##### OPTIONS #####

# Others

library(tau)
library(RTextTools)
library(Matrix)
library(slam)

library(stringr)
library(qdap)
library(SnowballC)
library(RTextTools)


# Reshape data - use this if needed for matrix
bombardier.reshape <- reshape(data = bombardier.p1p2, v.names = 'SubGrp', timevar = 'SubGrp', idvar = 'Descript', direction='wide')

# Use stemming if necessary
bombardier.corpus <- tm_map(bombardier.corpus, stemDocument) # Stemming



# Load files
bombardier.autoalerts <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='AutoAlerts', colClasses='character')
bombardier.idadmin <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='IDAdmin', colClasses='character')
bombardier.informational <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Informational', colClasses='character')
bombardier.midrange <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Midrange', colClasses='character')
bombardier.network <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Network', colClasses='character')
bombardier.noaction <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='NoAction', colClasses='character')
bombardier.password <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Password', colClasses='character')
bombardier.printer <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Printer', colClasses='character')
bombardier.restore <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Restore', colClasses='character')
bombardier.server <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Server', colClasses='character')
bombardier.servicerequest <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='ServiceRequest', colClasses='character')
bombardier.voice <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Voice', colClasses='character')
bombardier.hardware <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Hardware', colClasses='character')
bombardier.software <- read.xlsx(file='bombardier - p1p2.xlsx', sheetName='Software', colClasses='character')

bombardier.p1p2 <- is.vector(Alerts=bombardier.autoalerts,
                             IDAdmin=bombardier.idadmin,
                             Informational=bombardier.informational,
                             Midrange=bombardier.midrange,
                             Network=bombardier.network,
                             NoAction=bombardier.noaction,
                             Password=bombardier.password,
                             Printer=bombardier.printer,
                             Restore=bombardier.restore,
                             Server=bombardier.server,
                             ServiceRequest=bombardier.servicerequest,
                             Voice=bombardier.voice,
                             Hardware=bombardier.hardware,
                             Software=bombardier.software)
