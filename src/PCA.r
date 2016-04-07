library("irlba")
library("futile.matrix")
library("ggplot2")

### PART A : SVD
m <- read.table('reshape_one.txt', sep =",", header = FALSE)
m.rev <- sparseMatrix(i=m[,1], j=m[,2], x=m[,3])
s <- irlba(m.rev, nv = 50) 

### PART B : QUERY to new space
sample.query <- m.rev[,1]
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

