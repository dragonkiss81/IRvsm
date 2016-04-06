library("lsa")

# READ DATA
A <- read.table("linear_sample.txt")
A.rev <- as.matrix(A)

# PART A : TF / IDF
gw.idf <- gw_idf(A.rev[,])
colsum <- colSums(A.rev[,])

A.reweight <- matrix(nrow=nrow(A.rev),ncol=ncol(A.rev))
for(i in 1:ncol(A.rev)){ 
  A.reweight[,i] <- (A.rev[,i]/colsum[i]) * gw.idf
}

# PART B : SVD
S <- svd(A.reweight)
main.compon <- 2
S$u <- S$u[,1:main.compon]
S$d <- S$d[1:main.compon] 
S$v <- S$v[,1:main.compon]

# PART C : QUERY
sample.query <- c(0,0,0,1,0,1,1,0,0)
query.rev <- solve(diag(S$d)) %*% t(S$u) %*% as.matrix(sample.query)

# PLOT
plot(-1:1, -1:1, type = "n")
points(S$v[,1],S$v[,2], col = "blue")
points(t(query.rev), col = "red")

### Cos simularity
cosine_sim <- function(x,y) x %*% y / sqrt(x%*%x * y%*%y)
doc.vec <- t(S$v)
cos_list <- vector(len = ncol(doc.vec))
for(i in 1:ncol(doc.vec)){
  cos_list[i] <- cosine_sim(as.vector(query.rev) ,doc.vec[,i])
}
print(cos_list)





