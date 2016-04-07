library("XML")
library("tm")
library("tmcn")

xmlfile <- xmlTreeParse("query-train.xml")
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
# http://rstudio-pubs-static.s3.amazonaws.com/12422_b2b48bb2da7942acaca5ace45bd8c60c.html
