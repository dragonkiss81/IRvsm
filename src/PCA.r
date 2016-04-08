library("irlba")
library("futile.matrix")
library("ggplot2")

### PART A : SVD
m <- read.table('out/rev.txt', sep =",", header = FALSE)
m.rev <- sparseMatrix(i=m[,1], j=m[,2], x=m[,3])
m.rev <- m.rev[-stopw.index,] # stopword
s <- irlba(m.rev, nv = 5) 

### PART B : QUERY to new space
sample.query <- num.long.q  # m.rev[,1]
query.rev <- solve(diag(s$d)) %*% t(s$u) %*% as.matrix(sample.query)

### PLOT
plot(s$v[,1],s$v[,2], col = "blue")
points(t(query.rev), col = "red")

### Cos simularity
cosine_sim <- function(x,y) x %*% y / sqrt(x%*%x * y%*%y)
doc.vec <- t(s$v)
cos_list <- vector(len=ncol(doc.vec), mode='double')
for(i in 1:ncol(doc.vec)){
  cos_list[i] <- cosine_sim(as.vector(query.rev) ,doc.vec[,i])
}
rank <- order(cos_list, decreasing = TRUE)
head(cos_list[rank])
tail(cos_list[rank])

rank[1:1000]

file.list <- read.table("test/file-list", header = FALSE)
rank.answer <- tolower(substr(file.list[rank[1:100],1], 17,31))

ans.train <- read.table("test/ans-train")
ans.train.1 <- ans.train[which(ans.train$V1==1),2]

sum(rank.answer %in% ans.train.1)

