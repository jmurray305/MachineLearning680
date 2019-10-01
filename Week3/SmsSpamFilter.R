rm(list = ls()) 
graphics.off()
cat("\014") 

# source("C:\\Users\\pandr\\OneDrive\\R_code\\installPackages.R")
setwd('/Users/justinmurray/Desktop/ML680/Week3/')
sms_raw = read.csv("SMSSpamCollection.txt", quote = "", 
                row.names = NULL, 
                stringsAsFactors = FALSE,
                sep = '\t',
                header = FALSE)

str(sms_raw)

# Transform the dataset into a  representation a computer can understand. Transforming the data set into a  BAG-OF-WORDS
# ignores word order and simply provides a variable indication whether a word appears.

#change V1=Type into a factore 
sms_raw$V1 = factor(sms_raw$V1)

#we can see that about 13% of are labeled as spam
table(sms_raw$V1)

## Data preparation - cleaning and standardizing text data
library(tm)

sms_corpus = VCorpus(VectorSource(sms_raw$V2))
print(sms_corpus)

inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)


#Standardize message to use only lowercase characters.
sms_corpus_clean = tm_map(sms_corpus,
                          content_transformer(tolower))
# remove numbers 
sms_corpus_clean = tm_map(sms_corpus_clean, 
                          removeNumbers)

#remove stop words
sms_corpus_clean = tm_map(sms_corpus_clean,
                          removeWords, stopwords())

#remove punctuation
sms_corpus_clean = tm_map(sms_corpus_clean, removePunctuation)

#Stem words
library(SnowballC)
sms_corpus_clean = tm_map(sms_corpus_clean,stemDocument)

#Remove whitespace
sms_corpus_clean = tm_map(sms_corpus_clean, stripWhitespace)

sms_dtm = DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 = DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,
                                                         removeNumbers = TRUE,
                                                         stopwords = TRUE,
                                                         removePunctuation = TRUE,
                                                         stemming = TRUE
                                                         ))
sms_dtm
sms_dtm2


#Creating Training and test datasets
sms_dtm_train = sms_dtm[1:4180, ]
sms_dtm_test = sms_dtm[4181:5574, ]
sms_train_labels = sms_raw[1:4180, ]$V1
sms_test_labels = sms_raw[4181:5574, ]$V1
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))


#creating a word cloud
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

spam = subset(sms_raw, V1 == "spam")
ham = subset(sms_raw, V1 == "ham")

wordcloud(spam$V2, max.words = 30, scale = c(3, 0.5))
wordcloud(ham$V2, max.words = 50, scale = c(3, 0.5))

sms_freq_words = findFreqTerms(sms_dtm_train, 5)
sms_dtm_freq_train = sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test = sms_dtm_test[ , sms_freq_words]

#Convert to Yes/No
convert_count = function(x){
  x = ifelse(x>0, "Yes","No")
}

sms_train = apply(sms_dtm_freq_train, MARGIN = 2,
                  convert_count)

sms_test = apply(sms_dtm_freq_test, MARGIN = 2,
                 convert_count)
#training a model
library(e1071)

sms_classifier = naiveBayes(sms_train, sms_train_labels)


# Eval the model preformance
sms_test_pred = predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted','actual'))

sms_classifier2 = naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)

sms_test_pred2 = predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))



