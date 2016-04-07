# some error , but good try!
library("SparseM")
library("rARPACK")
library("bigpca")
library("bigmemory")
library("bigalgebra")
library("lsa")

m <- read.matrix('reshape.txt',  skip = 0,  header = FALSE)
In length(rows) * length(cols) : NAs produced by integer overflow

m <- readMM('reshape.txt')
scan() expected 'an integer', got '0,33688,1'

m <- import.big.data('reshape.txt')
prv.big.matrix(m)
pca <- big.PCA(m)


# references
http://www.cnblogs.com/wentingtu/archive/2012/03/30/2425582.html

SQLiteDF by Miguel Manese (“our” Google Summer of Codeproject 2006).
Description: Transparently stores data frames & matrices into SQLite tables.

https://cran.r-project.org/web/packages/irlba/irlba.pdf

Sparse Matrices for Large Data Modeling
https://www.r-project.org/conferences/useR-2007/program/presentations/maechler.pdf

http://www.bytemining.com/2010/05/hitting-the-big-data-ceiling-in-r/

用R進行中文 text Mining
http://rstudio-pubs-static.s3.amazonaws.com/12422_b2b48bb2da7942acaca5ace45bd8c60c.html

removePunctuation = TRUE,
	removeNumbers = TRUE, 
	stopwords=FALSE)
