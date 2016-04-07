library("XML")

xmlfile <- xmlTreeParse("query-train.xml")
xmltop = xmlRoot(xmlfile)
print(xmltop)[1:2]

plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
plantcat_df <- data.frame(t(plantcat),row.names=NULL)
plantcat_df[1:5,1:4]

summary(plantcat_df)

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