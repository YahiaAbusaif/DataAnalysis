rm(list=ls())

library(tm)
library(wordcloud)

df = read.csv('movie_reviews.csv', header=TRUE)

str(df)
head(df)

pcorpus = Corpus(VectorSource(df$text))

pcorpus = tm_map(pcorpus, tolower)
pcorpus = tm_map(pcorpus, removePunctuation)
pcorpus = tm_map(pcorpus, removeNumbers)
pcorpus = tm_map(pcorpus, stripWhitespace)
pcorpus = tm_map(pcorpus, removeWords, stopwords("english"))

pdtm = DocumentTermMatrix(pcorpus)

inspect(pdtm)
# Sparsity = 3006202016 * 100 / (3006202016 + 689184) ~= 100%

pdtm = removeSparseTerms(pdtm, 0.9999)

# Higher than 65. > 65
pfreq = findFreqTerms(pdtm, lowfreq=66)
pfreq

# Assume find with at least 0.01 correlation
findAssocs(pdtm, c("movie", "live"), c(0.05, 0.05))
pdtm = removeSparseTerms(pdtm, 0.999)

pdtm2 <- as.matrix(pdtm)

wordFreq = sort(colSums(pdtm2), decreasing=TRUE)

head(wordFreq, n=5)

wordcloud(names(wordFreq), wordFreq, max.words=100)
