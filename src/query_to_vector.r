library("XML")
library("tm")
library("tmcn")

xmlfile <- xmlTreeParse("test/query-train.xml")
xmltop = xmlRoot(xmlfile)
print(xmltop)[1:2]

plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
plantcat_df <- data.frame(t(plantcat),row.names=NULL)
summary(plantcat_df)

plantcat_df[4,2:5]
d.corpus <- Corpus(VectorSource(plantcat_df[2,2:5]))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
tdm <- TermDocumentMatrixCN(d.corpus,control = list(wordLengths = c(1, 2)))
inspect(tdm)

temp <- as.matrix(inspect(tdm))
s_clean_query <- cbind(rownames(temp),rowSums(temp))
rownames(s_clean_query) <- rownames(1:nrow(s_clean_query))
s_clean_query



long_vec <- read.table('out/wordvec.txt')
vocab.all <- readLines('test/vocab.all')
vocab.all[12400:12500]

long.q <- vector(mode="character",length=nrow(long_vec))
for(i in 1:nrow(long_vec)){
  if(long_vec[i,2]==-1){ long.q[i] <- paste(vocab.all[long_vec[i,1]]) }
  else{ long.q[i] <- paste( vocab.all[long_vec[i,1]], vocab.all[long_vec[i,2]], sep = "") }
}
head(long.q)

loc      <- which(long.q %in% c(s_clean_query[,1]))
loc.word <- long.q[loc]

num.long.q <- vector(mode="integer",length=nrow(long_vec))
for(i in 1:length(loc)){
  num.long.q[loc[i]] <- as.numeric( s_clean_query[which(s_clean_query %in% loc.word[i]),2] ) 
}
num.long.q[which(num.long.q!=0)]





