library("irlba")
library("futile.matrix")
library("SparseM")
library("rARPACK")
library("bigpca")
library("bigmemory")
library("bigalgebra")
library("lsa")
library("ggplot2")

### PCA
# m <- read.matrix('reshape.txt',  skip = 0,  header = FALSE)
# In length(rows) * length(cols) : NAs produced by integer overflow

# m <- readMM('reshape.txt')
# scan() expected 'an integer', got '0,33688,1'

# m <- import.big.data('reshape.txt')
# prv.big.matrix(m)
# pca <- big.PCA(m)

m <- read.table('reshape_one.txt', sep =",", header = FALSE)
m.rev <- sparseMatrix(i=m[,1], j=m[,2], x=m[,3])
s <- irlba(m.rev, nv = 50) 

#s <- svd(m) 
# main.compon <- 2
# s$u[,(main.compon+1):ncol(s$u)] <- 0
# s$d[(main.compon+1):ncol(s$d)] <- 0 
# s$v[,(main.compon+1):ncol(s$v)] <- 0

Temp <- s$u %*% diag(s$d)
A <-  crossprod(Temp, t(s$v))

crossprod(A,B)


Temp <- diag(s$d) %*% t(s$v)
View(A)


### Cos simularity
cosine_sim <- function(x,y) x %*% y / sqrt(x%*%x * y%*%y)

fake.query1 <- c(1,0,1,1,0)
# fake.query2 <- c(0,0,0,0,1)

cos_list <- vector(len = ncol(A))
for(i in 1:ncol(A)) cos_list[i] <- cosine_sim(fake.query1 ,A[,i])
print(cos_list)



# http://www.cnblogs.com/wentingtu/archive/2012/03/30/2425582.html

# SQLiteDF by Miguel Manese (“our” Google Summer of Codeproject 2006).
#Description: Transparently stores data frames & matrices into SQLite tables.

# https://cran.r-project.org/web/packages/irlba/irlba.pdf

# Sparse Matrices for Large Data Modeling
# https://www.r-project.org/conferences/useR-2007/program/presentations/maechler.pdf

# http://www.bytemining.com/2010/05/hitting-the-big-data-ceiling-in-r/