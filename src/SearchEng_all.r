# install.packages("tm")
# install.packages("slam")
# install.packages("rJava")
# install.packages("~/Downloads/tmcn_0.1-4.tar.gz", repos=NULL, type="source") 
# install.packages("~/Downloads/Rwordseg_0.2-1.tar.gz", repos=NULL, type="source")

library("tm")
library("rJava")
library("Rwordseg")
library("tmcn")
library("slam")

##  有關中文的套件: Rwordseg tmcn
##  用指令會有無法安裝的可能(滿常發生)
##  直接到 https://r-forge.r-project.org/R/?group_id=1571 及
##         https://r-forge.r-project.org/R/?group_id=1054 下載 .zip 檔
##  放入 R 的套件資料夾    
##  注意:載入 Rwordseg 前需要先載入 rJava 套件

######################################################################
##  Product TermDocumentMatrix for Chinese on R after version 3.0.2
##  Modified command "words" on package NLP 

wordsCN<-function(x,...){
  words<-unlist(segmentCN(x$content))
  return(words)
}
##  Modified command "termFreq" on package tm
termFreqCN<-
  function (doc, control = list()) 
  {
    stopifnot(inherits(doc, "TextDocument"), is.list(control))
    .tokenize <- control$tokenize
    if (is.null(.tokenize) || identical(.tokenize, "wordsCN")) 
      .tokenize <- wordsCN
    else if (identical(.tokenize, "MC")) 
      .tokenize <- MC_tokenizer
    else if (identical(.tokenize, "scan")) 
      .tokenize <- scan_tokenizer
    else if (NLP::is.Span_Tokenizer(.tokenize)) 
      .tokenize <- NLP::as.Token_Tokenizer(.tokenize)
    if (is.function(.tokenize)) 
      txt <- .tokenize(doc)
    else stop("invalid tokenizer")
    .tolower <- control$tolower
    if (is.null(.tolower) || isTRUE(.tolower)) 
      .tolower <- tolower
    if (is.function(.tolower)) 
      txt <- .tolower(txt)
    .removePunctuation <- control$removePunctuation
    if (isTRUE(.removePunctuation)) 
      .removePunctuation <- removePunctuation
    else if (is.list(.removePunctuation)) 
      .removePunctuation <- function(x) do.call(removePunctuation, 
                                                c(list(x), control$removePunctuation))
    .removeNumbers <- control$removeNumbers
    if (isTRUE(.removeNumbers)) 
      .removeNumbers <- removeNumbers
    .stopwords <- control$stopwords
    if (isTRUE(.stopwords)) 
      .stopwords <- function(x) x[is.na(match(x, stopwords(meta(doc, 
                                                                "language"))))]
    else if (is.character(.stopwords)) 
      .stopwords <- function(x) x[is.na(match(x, control$stopwords))]
    .stemming <- control$stemming
    if (isTRUE(.stemming)) 
      .stemming <- function(x) stemDocument(x, meta(doc, "language"))
    or <- c("removePunctuation", "removeNumbers", "stopwords", 
            "stemming")
    nc <- names(control)
    n <- nc[nc %in% or]
    for (name in sprintf(".%s", c(n, setdiff(or, n)))) {
      g <- get(name)
      if (is.function(g)) 
        txt <- g(txt)
    }
    if (is.null(txt)) 
      return(setNames(integer(0), character(0)))
    dictionary <- control$dictionary
    tab <- if (is.null(dictionary)) 
      table(txt)
    else table(factor(txt, levels = dictionary))
    if (names(tab[1])=="") tab <- tab[-1]
    bl <- control$bounds$local
    if (length(bl) == 2L && is.numeric(bl)) 
      tab <- tab[(tab >= bl[1]) & (tab <= bl[2])]
    nc <- nchar(names(tab), type = "chars")
    wl <- control$wordLengths
    lb <- if (is.numeric(wl[1])) wl[1] else 3
    ub <- if (is.numeric(wl[2])) wl[2] else Inf
    tab <- tab[(nc >= lb) & (nc <= ub)]
    storage.mode(tab) <- "integer"
    class(tab) <- c("term_frequency", class(tab))
    tab
  }

## Useful for TermDocumentMatrix
TermDocumentMatrix_classes <-
  c("TermDocumentMatrix", "simple_triplet_matrix")
## Useful for TermDocumentMatrix
.TermDocumentMatrix <-
  function(x, weighting)
  {
    x <- as.simple_triplet_matrix(x)
    if(!is.null(dimnames(x)))
      names(dimnames(x)) <- c("Terms", "Docs")
    class(x) <- TermDocumentMatrix_classes
    ## <NOTE>
    ## Note that if weighting is a weight function, it already needs to
    ## know whether we have a term-document or document-term matrix.
    ##
    ## Ideally we would require weighting to be a WeightFunction object
    ## or a character string of length 2.  But then
    ##   dtm <- DocumentTermMatrix(crude,
    ##                             control = list(weighting =
    ##                                            function(x)
    ##                                            weightTfIdf(x, normalize =
    ##                                                        FALSE),
    ##                                            stopwords = TRUE))
    ## in example("DocumentTermMatrix") fails [because weightTfIdf() is
    ## a weight function and not a weight function generator ...]
    ## Hence, for now, instead of
    ##   if(inherits(weighting, "WeightFunction"))
    ##      x <- weighting(x)
    ## use
    if(is.function(weighting))
      x <- weighting(x)
    ## and hope for the best ...
    ## </NOTE>
    else if(is.character(weighting) && (length(weighting) == 2L))
      attr(x, "weighting") <- weighting
    else
      stop("invalid weighting")
    x
  }
##  Modified command "TermDocumentMatrix" on package tm
##  and defined "TermDocumentMatrixCN"
TermDocumentMatrixCN<-
  function (x, control = list()) 
  {
    stopifnot(is.list(control))
    tflist <- lapply(unname(content(x)), termFreqCN, control)
    tflist <- lapply(tflist, function(y) y[y > 0])
    v <- unlist(tflist)
    i <- names(v)
    allTerms <- sort(unique(as.character(if (is.null(control$dictionary)) i else control$dictionary)))
    i <- match(i, allTerms)
    j <- rep(seq_along(x), sapply(tflist, length))
    docs <- as.character(meta(x, "id", "local"))
    if (length(docs) != length(x)) {
      warning("invalid document identifiers")
      docs <- NULL
    }
    m <- simple_triplet_matrix(i = i, j = j, v = as.numeric(v), 
                               nrow = length(allTerms), ncol = length(x), dimnames = list(Terms = allTerms, 
                                                                                          Docs = docs))
    bg <- control$bounds$global
    if (length(bg) == 2L && is.numeric(bg)) {
      rs <- row_sums(m > 0)
      m <- m[(rs >= bg[1]) & (rs <= bg[2]), ]
    }
    weighting <- control$weighting
    if (is.null(weighting)) 
      weighting <- weightTf
    .TermDocumentMatrix(m, weighting)
  }

library("XML")
library("tm")
library("tmcn")
library("irlba")
library("futile.matrix")
library("ngram")
library("Rwordseg")

### Read All Require Data
xmlfile <- xmlTreeParse("test/query-test.xml")
long_vec <- read.table('out/wordvec.txt')
vocab.all <- readLines('test/vocab.all')
vocab.all <- vocab.all[-1]
stop.word <- readLines('test/stopword.tw')
m <- read.table('out/rev.txt', sep =",", header = FALSE)
m.rev <- sparseMatrix(i=m[,1], j=m[,2], x=m[,3]) 
#if use svd : s <- irlba(m.rev, nv = 100) 

xmltop = xmlRoot(xmlfile) # Read XML
plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
plantcat_df <- data.frame(t(plantcat),row.names=NULL)
# summary(plantcat_df)


### Generate All Term
long.q <- vector(mode="character",length=nrow(long_vec))
for(i in 1:nrow(long_vec)){
  if(long_vec[i,2]==-1){ long.q[i] <- paste(vocab.all[long_vec[i,1]]) }
  else{ long.q[i] <- paste( vocab.all[long_vec[i,1]], vocab.all[long_vec[i,2]], sep = "") }
} # length(long.q)


### Query extraction
insertWords(c('白色', '恐怖', '受难', '行政', '国防', '立法', '匪谍',
              '罚锾',
              '科省', '柯省', '科索', '柯索', '难民', '马其', '其顿', '土耳', '耳其', '阿尔', '巴尼',
              '中美',
              '两税', '业升', '抵减', '经济', '财政', '经建', '行政',
              '护盘', '公积', '资本', '证期', '董监', '央行', '增值', '印花', '证交',
              '麦可', '乔丹', '乔登', '代言', '耐吉', '史腾', '博德',
              '晓燕', '白案', '掳人', '勒赎', '合议', '进兴', '志辉', '素真',
              '儿扶',
              '迪士', '士尼',
              '策进', '招策', '教育',
              '培训', '国小', '教学', '检核',
              '美浓',
              '栖兰', '桧木', '枯立', '倒木', '退辅', '农委', '森保', '林务',
              '黑面', '琵鹭', '曾文', '圣婴', '猩红', '登革', '革热', '肠病'))


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
    print(s_query_fin)
  }
  else{
    s_query_stop <- s_clean_query[-which(s_clean_query[,1] %in% stop.word),]
    s_query_fin <- s_query_stop[which(s_query_stop[,1] %in% long.q),]
    print(s_query_fin)
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

