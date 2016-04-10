
all_ranking_list <- data.frame() 
for(NUM_OF_QUERY in 1:nrow(plantcat_df)){

### help segment
str <- as.character(plantcat_df[NUM_OF_QUERY,c(2)]) 
str2 <- as.character(plantcat_df[NUM_OF_QUERY,c(3)]) 
str3 <- as.character(plantcat_df[NUM_OF_QUERY,c(5)]) 
insertWords(segmentCN(str))
insertWords(segmentCN(str2))
insertWords(segmentCN(str3))

d.corpus <- Corpus(VectorSource(plantcat_df[NUM_OF_QUERY,c(5)]))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
tdm <- TermDocumentMatrixCN(d.corpus,control = list(wordLengths = c(1, 2)))
temp <- as.matrix(inspect(tdm))

### Clean Query
s_clean_query <- cbind(rownames(temp),rowSums(temp))
rownames(s_clean_query) <- rownames(1:nrow(s_clean_query))
if( length(which(s_clean_query[,1] %in% stop.word)) == 0 ){
  s_query_fin <- s_clean_query[which(s_clean_query[,1] %in% long.q),]
}
else{
  s_query_stop <- s_clean_query[-which(s_clean_query[,1] %in% stop.word),]
  s_query_fin <- s_query_stop[which(s_query_stop[,1] %in% long.q),]
}


### Find Query Index in All_term (long.q)
loc <- which(long.q %in% c(s_query_fin[,1]))
loc.word <- long.q[loc]
num.long.q <- vector(mode="integer",length=length(long.q))
for(i in 1:length(loc)){
  num.long.q[loc[i]] <- as.numeric( s_query_fin[which(s_query_fin[,1] == loc.word[i]),2] ) 
}


### Ranking : BM25
query.temp <- as.numeric(s_query_fin[,2])
k3 <- 2
query.rev <- ((k3+1)*query.temp) / (k3+query.temp)
query.rev <- query.temp

innerproduct <- function(x,y) x %*% y # / sqrt(x%*%x * y%*%y)

doc.vec <- m.rev[which(num.long.q!=0),]
cos_list <- apply(doc.vec[,],2,innerproduct,y = as.numeric(query.rev))
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


### Compare to answer
# ans.train <- read.table("test/ans-train")
# ans.train.1 <- ans.train[which(ans.train$V1==NUM_OF_ANS),2]
# sum(rank.answer %in% ans.train.1)

