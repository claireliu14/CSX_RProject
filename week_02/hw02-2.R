source('pttTestFunction.R')

id = c(1:10)
URL = paste0("https://www.ptt.cc/bbs/WomenTalk/index", id, ".html")  # paste預設會有空格，paste0則沒有
filename = paste0(id, ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, URL = URL, filename = filename)

rm(list=ls(all.names = TRUE))
library(NLP)        # install.packages("NLP")
library(tm)         # install.packages("tm")
library(jiebaRD)    # install.packages("jiebaRD")
library(jiebaR)     # install.packages("jiebaR") 中文文字斷詞
library(RColorBrewer)
library(wordcloud)  #install.packages("wordcloud")

filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "※")
docs <- tm_map(docs, toSpace, "◆")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "你")
docs <- tm_map(docs, toSpace, "推")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "看板")
docs <- tm_map(docs, toSpace, "作者")
docs <- tm_map(docs, toSpace, "發信站")
docs <- tm_map(docs, toSpace, "批踢踢實業坊")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs


mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[order(freqFrame$Freq,decreasing=TRUE), ]
library(knitr)
kable(head(freqFrame, 10), format = "markdown")


par(family=("Heiti TC Light"))
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.1),min.freq=50,max.words=150,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

library(stringr)
ptt.url <- "https://www.ptt.cc"
gossiping.url <- str_c(ptt.url, "/bbs/Gossiping") 
gossiping.url

gossiping.session <- html_session(url = gossiping.url) 
gossiping.session

gossiping.form <- gossiping.session %>% 
  html_node("form") %>%
  html_form()
gossiping.form

gossiping <- submit_form( 
  session = gossiping.session, 
  form = gossiping.form, 
  submit = "yes"
) 
gossiping