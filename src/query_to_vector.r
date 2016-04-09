library("XML")
library("tm")
library("tmcn")
library("irlba")
library("futile.matrix")
library("ggplot2")

### READ ALL REQUIRE DATA
xmlfile <- xmlTreeParse("test/query-test.xml")
long_vec <- read.table('out/wordvec.txt')
vocab.all <- readLines('test/vocab.all')
vocab.all <- vocab.all[-1]
stop.word <- readLines('test/stopword.tw')
m <- read.table('out/rev.txt', sep =",", header = FALSE)

# Read XML
xmltop = xmlRoot(xmlfile)
plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
plantcat_df <- data.frame(t(plantcat),row.names=NULL)
# summary(plantcat_df)


### STOPWORD PREPROCESSING
long.q <- vector(mode="character",length=nrow(long_vec))
for(i in 1:nrow(long_vec)){
  if(long_vec[i,2]==-1){ long.q[i] <- paste(vocab.all[long_vec[i,1]]) }
  else{ long.q[i] <- paste( vocab.all[long_vec[i,1]], vocab.all[long_vec[i,2]], sep = "") }
}
stopw.index <- which(long.q %in% stop.word)
long.q <- long.q[-stopw.index]
# length(long.q)


### TRAIN MODEL SVD
m.rev <- sparseMatrix(i=m[,1], j=m[,2], x=m[,3])
m.rev <- m.rev[-stopw.index,] # stopword
# s <- irlba(m.rev, nv = 100) 

all_ranking_list <- data.frame() 

for(NUM_OF_QUERY in 1:nrow(plantcat_df)){

### tw query extraction
d.corpus <- Corpus(VectorSource(plantcat_df[NUM_OF_QUERY,c(2,3,5)]))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
tdm <- TermDocumentMatrixCN(d.corpus,control = list(wordLengths = c(1, 2)))
# inspect(tdm)
temp <- as.matrix(inspect(tdm))
s_clean_query <- cbind(rownames(temp),rowSums(temp))
rownames(s_clean_query) <- rownames(1:nrow(s_clean_query))
s_clean_query

# short vector to long vector
loc <- which(long.q %in% c(s_clean_query[,1]))
loc.word <- long.q[loc]

num.long.q <- vector(mode="integer",length=length(long.q))
for(i in 1:length(loc)){
  num.long.q[loc[i]] <- as.numeric( s_clean_query[which(s_clean_query == loc.word[i]),2] ) 
}
show <- cbind(long.q[which(num.long.q!=0)],num.long.q[which(num.long.q!=0)])
show

### SVD : QUERY to new space
# sample.query <- num.long.q  # m.rev[,1]
# query.rev <- solve(diag(s$d)) %*% t(s$u) %*% as.matrix(sample.query)
query.rev <- num.long.q

# ### PLOT
# plot(s$v[,1],s$v[,2], col = "blue")
# points(t(query.rev), col = "red")


### Ranking : Cos simularity
cosine_sim <- function(x,y) x %*% y # / sqrt(x%*%x * y%*%y)
# doc.vec <- t(s$v)
doc.vec <- m.rev
cos_list <- vector(length=0, mode='double')
print(c("bottleneck start", NUM_OF_QUERY))
for(i in 1:(length(long.q)/32)){ # parallel idea
  cos_list <- c(cos_list,apply(doc.vec[,(32*(i-1)+1):(32*i)],2,cosine_sim,y = as.vector(query.rev)))
  # cos_list[i] <- cosine_sim(as.vector(query.rev) ,doc.vec[,i])
}
rank <- order(cos_list, decreasing = TRUE)
print(c("bottleneck end", NUM_OF_QUERY))

### Output Ranking Ans
file.list <- read.table("test/file-list", header = FALSE)
rank.answer <- c(tolower(substr(file.list[rank[1:100],1],17,31)))
topic.num <- substr( plantcat_df[NUM_OF_QUERY,1] , 15,17)
ans.out <- cbind(rep(topic.num,length(rank.answer)),rank.answer)


all_ranking_list <- rbind(all_ranking_list,ans.out)
}

write.table(all_ranking_list, file="out/ranking_list.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)



### Compare to REAL answer
ans.train <- read.table("test/ans-train")
ans.train.1 <- ans.train[which(ans.train$V1==NUM_OF_ANS),2]
sum(rank.answer %in% ans.train.1)


