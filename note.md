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





### XML

# http://rstudio-pubs-static.s3.amazonaws.com/12422_b2b48bb2da7942acaca5ace45bd8c60c.html

R and the web (for beginners), Part II: XML in R
# http://www.r-bloggers.com/r-and-the-web-for-beginners-part-ii-xml-in-r/

Basic Text Mining in R
# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

qdap-tm Package Compatibility
# https://cran.r-project.org/web/packages/qdap/vignettes/tm_package_compatibility.pdf

中文文本挖掘包tm、tmcn、Rwordseg、Rweibo的安装
# http://blog.csdn.net/andy_henry/article/details/25929019

R 爬虫
# http://mylearnho.blogspot.tw/2015/09/r.html


指令TermDocumentMatrix處理中文發生問題(已解決)
# http://mylearnho.blogspot.tw/2015/09/termdocumentmatrix.html

# java installation
sudo apt-get install default-jre
sudo apt-get install default-jdk
https://www.digitalocean.com/community/tutorials/how-to-install-java-on-ubuntu-with-apt-get