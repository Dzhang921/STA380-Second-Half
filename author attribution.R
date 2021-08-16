
# Import libraries
library(tm)
library(tidyverse)
library(slam)
library(proxy)



# Rolling all the dictionaries into one corpus
author_dirs = Sys.glob('./ReutersC50/C50train/*')
file_list = NULL
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=23)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# Create a function to read the content
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

# Apply the function to all the files
all_docs = lapply(file_list, readerPlain)

# Clean the names of the file list
mynames = file_list %>%
  { strsplit(., '/', fixed=TRUE) } %>%
  { lapply(., tail, n=2) } %>%
  { lapply(., paste0, collapse = '') } %>%
  unlist

names(all_docs) = mynames
names(all_docs) = sub('.txt', '', names(all_docs))

my_corpus = Corpus(VectorSource(all_docs))

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

X_train = DocumentTermMatrix(my_corpus)

X_train = removeSparseTerms(X_train, 0.975)
X_train

# Now a dense matrix
X_train = as.matrix(X_train)

smooth_count = 1/nrow(X_train)
w_train = rowsum(X_train + smooth_count, labels)
w_train = w_train/sum(w_train)
w_train = log(w_train)

# Create test set
author_dirs = Sys.glob('./ReutersC50/C50test/*')
file_list = NULL
labels = NULL
author_names = NULL
test_labels = NULL

for(author in author_dirs) {
  author_name = substring(author, first=22)
  author_names = append(author_names, author_name)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  test_labels = append(test_labels, rep(author_name, length(files_to_add)))
}

# Create a function to read the content
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

# Apply the function to all the files
all_docs = lapply(file_list, readerPlain)

# Clean the names of the file list
mynames = file_list %>%
  { strsplit(., '/', fixed=TRUE) } %>%
  { lapply(., tail, n=2) } %>%
  { lapply(., paste0, collapse = '') } %>%
  unlist

names(all_docs) = mynames
names(all_docs) = sub('.txt', '', names(all_docs))

my_corpus = Corpus(VectorSource(all_docs))


# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

X_test = DocumentTermMatrix(my_corpus, list(dictionary=colnames(X_train)))
X_test = as.matrix(X_test)

#Prediction for the test set
predict = NULL
for (i in 1:nrow(X_test)) {
  # get maximum Naive Bayes log probabilities
  max = -(Inf)
  author = NULL
  for (j in 1:nrow(w_train)) {
    result = sum(w_train[j,]*X_test[i,])
    if(result > max) {
      max = result
      author = rownames(w_train)[j]
    }
  }
  predict = append(predict, author)
}





predict_results = table(test_labels,predict)


#Accuracy
sum(test_labels == predict)/length(predict)


#RandomForest
library(plyr)
library(randomForest)
library(caret)

X_train.rf = as.matrix(X_train)
X_test.rf = as.matrix(X_test)
X_train.rf = as.data.frame(X_train.rf)

labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=23)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

rf.model = randomForest(x=X_train.rf, y=as.factor(labels), mtry=4, ntree=20)
rf.pred = predict(rf.model, data=X_test.rf)
rf.table = as.data.frame(table(rf.pred,labels))
new = subset(rf.table, rf.pred == labels)
sum(new$Freq)/2500
