library(tm)
library(slam)
library(topicmodels)
library(RODBC)
library(jiebaR)

conn <- odbcConnect(dsn = 'localhost', uid = 'root', pwd = 'root')
data <- sqlQuery(conn, "SELECT i_id, t_article, i_tag FROM shangshi_zixun where i_tag in ('A', 'B', 'C', 'D', 'E', 'F', 
                 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R','S', '-1', '0')", stringsAsFactors = F)
cutter <- worker(type = 'mix')
datalist <- sapply(data$t_article, FUN = function(x) list(cutter[x]))
names(datalist) <- paste(data$i_id, data$i_tag, sep = "_")
datalist <- sapply(datalist, FUN = function(x) list(list(x[grep('\\s', x, invert = T)])))
odbcCloseAll()

corpus <- Corpus(VectorSource(datalist))
for (i in 1:length(corpus)){
  corpus[[i]]$content <- sub("c", "", corpus[[i]]$content)
  cat(i, '\n')
}
for (i in 1:length(corpus)){
  meta(corpus[[i]], tag = 'id') <- names(datalist)[i]
  cat(i, '\n')
}

control.tf <- list(removePunctuation = T, stripWhitespace = T, wordLengths = c(2, 10))
dtm <- DocumentTermMatrix(corpus, control.tf)

dict <- table(unlist(datalist))
dict <- names(dict[dict > 1])

dimension <- intersect(dict,Terms(dtm))
dimension <- dimension[-c(1:8)]

index <- match(dimension, Terms(dtm))
dtm <- dtm[, index]
dtm <- dtm[row_sums(dtm) > 0, ]

k = 50
topic_control = list(alpha = 50/k, estimate.beta = T, verbose = 0, prefix = tempfile(), 
                     save = 0, keep = 0, seed = as.integer(Sys.time()), nstart = 1, 
                     best = T, delta = 0.1, iter = 2000, burnin = 0, thin = 2000)
Topic_models <- LDA(x = dtm, k = k, method = 'Gibbs', control = topic_control)
