rm(list=ls())

library('tm')
library("wordcloud")

#1. Import the data in 'movie_reviews.csv' into a dataframe.
df = read.csv('movie_reviews.csv')

#2. Inspect the dataframe to get familiar with the dataset.
df
str(df)
summary(df)

#3. Construct a corpus object from the 'text' column in the dataframe.
pcorpus = Corpus(VectorSource(df$text))

#4. Convert all the words of the corpus from uppercase to lowercase
pcorpus = tm_map(pcorpus, tolower)

#5. Remove punctuation marks, numbers, and white spaces from the corpus.
pcorpus = tm_map(pcorpus, removePunctuation)
pcorpus = tm_map(pcorpus, removeNumbers)
pcorpus = tm_map(pcorpus, stripWhitespace)

#6. Remove stopping words (in English language) from the corpus.
pcorpus = tm_map(pcorpus, removeWords, "english")

#7. Construct the document term matrix.
pdtm = DocumentTermMatrix(pcorpus)

#8. Inspect the document term matrix and determine its sparsity percentage.
inspect(pdtm)
str(pdtm)
pdtm[['i']]
# Sparsity: 99%

#9. Remove sparse terms below a sparsity threshold 0.999
pdtm = removeSparseTerms(pdtm, 0.999)

#10. Find words of frequencies higher than 65.
pfreq = findFreqTerms(pdtm, 65)
pfreq

#11. Find the associations of words "movie" and "live".
findAssocs(pdtm, c("movie", "live"), c(0.05, 0.05))

#12. Construct a normal matrix from the term-document matrix 
#and sort the words by their frequency in a descending order.
pdtm2 = as.matrix(pdtm)
pfrequency = colSums(pdtm2)
pfrequency = sort(pfrequency, decreasing = TRUE)

#13. Display the most frequent five words in the corpus.
head(pfrequency, 5)

#14. Display the word cloud of the first 100 words.
pwords = names(pfrequency)
wordcloud(pwords, pfrequency, random.order = FALSE, max.words = 100)
