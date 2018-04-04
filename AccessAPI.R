library(httr)
library(jsonlite)
library(text2vec)
library(data.table)
library(magrittr)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

#access viziometrics api
vizio_api = function(path) {
  url = modify_url(paste("http://viziometrics.org/api/pmc/image/search/?keywords=cancer&qrandom=true&number=1500",path,sep=""))
  fromJSON(url)
}
sample1=vizio_api("")

#sample1= sample1 %>% filter(class_name!="photo")%>%mutate(id = row_number())


#text analysis with text2vec package

setDT(sample1)
setkey(sample1,id)
set.seed(100)
all_ids=sample1$id
train_ids = sample(all_ids, nrow(sample1)/4)
test_ids = setdiff(all_ids, train_ids)
train = sample1[J(train_ids)]
test = sample1[J(test_ids)]

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(train$caption, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = TRUE)
stop_words = c("with", "and", "the", "it", "in", "of", "an", "for", "on", "it's","to", "its","a","as","was","were","are","by")
vocab = create_vocabulary(it_train,stopwords = stop_words)

train_tokens = train$caption %>% 
  prep_fun %>% 
  tok_fun
it_train = itoken(train_tokens, 
                  ids = train$id)

vocab = create_vocabulary(it_train,stopwords = stop_words)

vectorizer = vocab_vectorizer(vocab)
t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

dim(dtm_train)
vocab=vocab%>%arrange(desc(term_count),desc(doc_count))

study_wordcloud=wordcloud(words = vocab$term, freq = vocab$term_count, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#write.csv(sample1, file = "keyword_study.csv")
