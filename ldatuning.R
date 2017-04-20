
setwd("/home/droste/Dropbox/Dokumente/doctorate/ecosystem services/data/output/ES")
literature <- read.csv("literature_ES.csv", sep = ";")
#data("AssociatedPress", package="topicmodels")

library("ldatuning")
library("SnowballC")
library("tm")

dfCorpus <- Corpus(VectorSource(literature$Abstract))
dfCorpus = tm_map(dfCorpus, content_transformer(tolower))
dfCorpus = tm_map(dfCorpus, content_transformer(removePunctuation))
dfCorpus = tm_map(dfCorpus, content_transformer(removeNumbers))
dfCorpus = tm_map(dfCorpus, removeWords, stopwords("SMART"))
dfCorpus = tm_map(dfCorpus, stemDocument)
dfFrequencies = DocumentTermMatrix(dfCorpus) 
dfSparse = removeSparseTerms(dfFrequencies, 0.995)
narrativeSparse = as.data.frame(as.matrix(dfSparse))
colnames(narrativeSparse) = make.names(colnames(narrativeSparse))
narrativeSparse<-narrativeSparse[rowSums(narrativeSparse)>0,]

result <- FindTopicsNumber(
  narrativeSparse,
  topics = seq(from = 2, to = 402, by = 50),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4,
  verbose = TRUEu
)
FindTopicsNumber_plot(result)
result$topics[which.max(result$Griffiths2004)]
dev.print(png, "/plots/ldatuning_Griffiths2004.png", width=600, height=600)


