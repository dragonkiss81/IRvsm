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

