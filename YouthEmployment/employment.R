# 人力資源調查就業人數
xml.url <- "https://www.dgbas.gov.tw/public/data/open/Cen/MP0101A03.xml"
xml.content <- getURL(xml.url, .encoding="UTF-8")
xml.doc <- xmlParse(xml.content)
xml.df <- xmlToDataFrame(xml.doc)
xml.df[, -1] <- lapply(xml.df[, -1], function(x) as.numeric(as.character(x)))
xml.df[ xml.df == "" ] <- NA
xml.df <- xml.df[complete.cases(xml.df), ]
xml.df.year <- xml.df[!str_detect(xml.df[,1], "[0-9]M[0-9]"), c(1,2,5:15)] 
emp.young <- rowSums(xml.df.year[, c(3:5)])
emp.other <- rowSums(xml.df.year[, c(6:13)])
emp.year  <- as.numeric(as.character(unique(xml.df.year[, 1])))

# 青年就業趨勢
xml.df.month <- xml.df[str_detect(xml.df[,1], "[0-9]M[0-9]"), c(1,5:7)] %>% 
  separate(項目別_Iterm, c("year", "month"), "M")
xml.df.month$year <- xml.df.month$year %>% as.numeric()
emp.month.young <- rowSums(xml.df.month[, c(3:5)])

# 勞工生活及就業狀況調查
json.url <- "https://apiservice.mol.gov.tw/OdService/rest/datastore/A17000000J-020078-urh"
json.content <- getURL(json.url, .encoding="UTF-8")
sati.place <- fromJSON(json.content)$result$records
json.url <- "https://apiservice.mol.gov.tw/OdService/rest/datastore/A17000000J-020078-AA6"
json.content <- getURL(json.url, .encoding="UTF-8")
sati.time <- fromJSON(json.content)$result$records
json.url <- "https://apiservice.mol.gov.tw/OdService/rest/datastore/A17000000J-020078-bfP"
json.content <- getURL(json.url, .encoding="UTF-8")
sati.price <- fromJSON(json.content)$result$records
json.url <- "https://apiservice.mol.gov.tw/OdService/rest/datastore/A17000000J-020078-0t2"
json.content <- getURL(json.url, .encoding="UTF-8")
sati.load <- fromJSON(json.content)$result$records
json.url <- "https://apiservice.mol.gov.tw/OdService/rest/datastore/A17000000J-020078-oBu"
json.content <- getURL(json.url, .encoding="UTF-8")
sati.overall <- fromJSON(json.content)$result$records
sati.age.place <- subset(sati.place, 統計項目別 == "年齡")
sati.age.place$統計項目別 <- "工作場所"
sati.age.time <- subset(sati.time, 統計項目別 == "年齡")
sati.age.time$統計項目別 <- "工作時數"
sati.age.price <- subset(sati.price, 統計項目別 == "年齡")
sati.age.price$統計項目別 <- "工資"
sati.age.load <- subset(sati.load, 統計項目別 == "年齡")
sati.age.load$統計項目別 <- "工作負荷量"
sati.age.overall <- subset(sati.overall, 統計項目別 == "年齡")
sati.age.overall$統計項目別 <- "整體工作"
sati.age <- rbind(sati.age.place, sati.age.time, sati.age.price, sati.age.load, sati.age.overall)
#sati.age <- rbind(sati.age.place, sati.age.time, sati.age.load, sati.age.overall)

sati.age[ sati.age == "-" ] <- NA
sati.age$`樣本數（人）` <- as.integer(sati.age$`樣本數（人）`)
sati.age$`很滿意（%）` <- as.numeric(sati.age$`很滿意（%）`)
sati.age$`滿意（%）` <- as.numeric(sati.age$`滿意（%）`)
sati.age$`普通（%）` <- as.numeric(sati.age$`普通（%）`)
sati.age$`不滿意（%）` <- as.numeric(sati.age$`不滿意（%）`)
sati.age$`很不滿意（%）` <- as.numeric(sati.age$`很不滿意（%）`)
sati.data <- sati.age %>% gather("程度", value, -1:-2)
sati.data$程度 <- factor(sati.data$程度,
                       levels=c("很滿意（%）" , "滿意（%）", "普通（%）", 
                                "不滿意（%）", "很不滿意（%）", "樣本數（人）"),
                       ordered = TRUE)

# 歷年就業者之獲得現職方法
xml.url <- "https://www.dgbas.gov.tw/public/data/open/Cen/Mp0101A12.xml"
xml.content <- getURL(xml.url, .encoding="UTF-8")
xml.doc <- xmlParse(xml.content)
xml.get.job.df <- xmlToDataFrame(xml.doc)
xml.get.job.df[,-1] <- as.integer(gsub("\\s+", "", as.matrix(xml.get.job.df[,-1])))
xml.get.job.df <- xml.get.job.df %>% gather("方法", value, -1:-2)
xml.get.job.df$方法 <- str_split_fixed(xml.get.job.df$方法, "_", 2)[,1]
xml.get.job.df$項目別_Iterm  <- as.numeric(as.character(xml.get.job.df$項目別_Iterm))
xml.get.job.year.min <- min(xml.get.job.df$項目別_Iterm)
xml.get.job.year.max <- max(xml.get.job.df$項目別_Iterm)

# 大專畢業生就業概況分析
data.price <- read.table(file = "EMP_DATA/Student_RPT_19_price.txt", header = TRUE, fileEncoding='big5', sep=",")
data.class <- read.table(file = "EMP_DATA/Student_RPT_19_class.txt", header = TRUE, fileEncoding='big5', sep=",")
data.price <- data.price %>% gather("學歷", value, -1:-4)
data.price$value <- as.integer(gsub(",", "", data.price$value))
data.price <- data.price[complete.cases(data.price), ]
data.price <- data.price %>% 
  mutate(學門代碼=as.integer(學類代碼%/%100)) %>%
  left_join(data.class %>% select(學門代碼, 學門名稱), by="學門代碼")
min <- min(filter(data.price, 畢業年度==100 & 到職年==103 & 學歷=="碩士")$value)
max <- max(filter(data.price, 畢業年度==100 & 到職年==103 & 學歷=="碩士")$value)
