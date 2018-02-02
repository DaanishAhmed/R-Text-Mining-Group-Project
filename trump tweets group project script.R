# DATA 630 Group Project
# Written by Group 2
# Ed Perry, Daanish Ahmed, and Abdourahmane Bah
# Semester Summer 2017
# August 10, 2017
# Professor Edward Herranz

# This R script involves performing text mining analysis on a dataset containing 
# President (then candidate) Donald J. Trump's Twitter tweets during the 2016 
# presidential election.  The text mining processes include creation of a word 
# cloud showing his most frequently-used words as well as a bar graph with the 
# ten most common words and their frequencies.  The last algorithm is keyword-
# based association analysis, which will involve finding the terms that frequently 
# appear with the input words and determining the relationship between these words.



# Loading the data and initializing packages.

# Directory (Change this to the directory you are using.)
setwd("~/Class Documents/2016-17 Summer/DATA 630/R/Group Project")

# Install the packages for text mining (remove #'s if you haven't installed them yet).

# Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
#             "cluster", "igraph", "fpc")
# install.packages(Needed, dependencies = TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

# Loads the packages we need.
library(tm)
library(SnowballC)
library(wordcloud)

# Loads the data.
trump <- read.csv("Donald-Trump_7375-Tweets-Excel.csv", head=TRUE, sep=",")

# Views the dataset.
View(trump)

# Shows the initial structure of the dataset.
str(trump)

# End of loading the data.



# Preprocessing

# Converts "Date" variable to date type.
trump$Date <- as.Date(trump$Date, format = "%m/%d/%Y")
trump$Time <- NULL

# Removal of other unnecessary variables.
trump$X <- NULL
trump$X.1 <- NULL

# Shows the descriptive statistics of the dataset.
summary(trump)

# Shows the first 10 entries, excluding tweet text and URL.
head(trump[, -c(2, 7)], 10)

# This function reveals there are no missing values (NAs).
apply(trump, 2, function(trump) sum(is.na(trump)))

# Converts tweet text into corpus type.
trump_corpus <- Corpus(VectorSource(trump$Tweet_Text))

# Examines the first tweet.
inspect(trump_corpus[1])

# Additional stopwords and unnecessary words to remove.
stop = c("just", "good", "watch", "time", "join", "get", "big", "going", "much", "said", 
         "like", "will", "now", "new", "can", "amp", "doesnt", "gave", "means", "one")

# Changes all letters to lowercase, removes numbers and punctuation, removes all 
# stopwords, and strips whitespace.
trump_corpus = tm_map(trump_corpus, content_transformer(tolower))
trump_corpus = tm_map(trump_corpus, removeNumbers)
trump_corpus = tm_map(trump_corpus, removePunctuation)
trump_corpus = tm_map(trump_corpus, removeWords, c("the", "and", stop, stopwords("english")))
trump_corpus =  tm_map(trump_corpus, stripWhitespace)

# Verifies that the first tweet has been preprocessed.
inspect(trump_corpus[1])

# End of preprocessing section.



# This section covers the creation of word clouds to visualize the most frequent 
# words found in Trump's tweets.

# Analyze the textual data using a document-term matrix.
trump_dtm <- DocumentTermMatrix(trump_corpus)
trump_dtm

# Examine the document.
inspect(trump_dtm [20:25, 20:25])

# Reduce the dimension of the Document-Term Matrix (DTM).
trump_dtm <- removeSparseTerms(trump_dtm, 0.99)
trump_dtm

# Verify that the document's sparsity has decreased.
inspect(trump_dtm[1,1:20])

# Obtains the most frequent words, sorted in descending order by count.
findFreqTerms(trump_dtm, 5)
freq = data.frame(sort(colSums(as.matrix(trump_dtm)), decreasing = TRUE))

# Creates a word cloud using Trump's most frequent words, with a 30 word maximum.
wordcloud(rownames(freq), freq[,1], max.words = 30, random.order=TRUE, colors = brewer.pal(1, "Dark2"))

# Use term frequency-inverse document frequency (TD-IDF) for more relevant results.
trump_dtm_tfidf <- DocumentTermMatrix(trump_corpus, control = list(weighting = weightTfIdf))
trump_dtm_tfidf = removeSparseTerms(trump_dtm_tfidf, 0.97)

# Obtains the most frequent words, sorted in descending order by count.
freq = data.frame(sort(colSums(as.matrix(trump_dtm_tfidf)), decreasing=TRUE))

# Creates a second word cloud that includes fewer irrelevant words.
wordcloud(rownames(freq), freq[,1], max.words=30, colors=brewer.pal(3, "Dark2"))

# End of generating word clouds.



# This section covers the creation of a barplot showing the most common words 
# and how often they are used.

# Creates a term-document matrix (TDM).
trump_tdm <- TermDocumentMatrix(trump_corpus)

# Sorts the word frequency in descending order.
freq <- sort(rowSums(as.matrix(trump_tdm)), decreasing=TRUE)

# Shows the 10 most common words and their frequencies.
freq[1:10]

# Creates a bar plot of the 10 most common words.
barplot(freq[1:10], col = "red", las = 2)

# End of generating bar plot.



# This section of code covers association mining, which involves finding words that 
# commonly appear together.  It is useful for finding phrases such as "lyin' Ted," 
# "little Marco," etc.  It is also useful for understanding Trump's attitudes
# towards issues such as jobs and coal mining.

# It was obtained from "Introduction to Text Mining with R for Information Professionals" 
# (https://rpubs.com/bgonzo/textmining).

# Finds common words associated with "Ted"
findAssocs(trump_tdm, term = "ted", 0.2)

# Finds common words associated with "Marco"
findAssocs(trump_tdm, term="marco", 0.15)

# Finds common words associated with "Hillary"
findAssocs(trump_tdm, term="hillary", 0.1)

# Finds common words associated with "Sanders"
findAssocs(trump_tdm, term="sanders", 0.15)

# Finds common words associated with "Paul"
findAssocs(trump_tdm, term="paul", 0.2)

# Finds common words associated with "jobs"
findAssocs(trump_tdm, term="jobs", 0.15)

# Finds common words associated with "coal"
findAssocs(trump_tdm, term="coal", 0.3)

# End of association mining section.

# End of script.

