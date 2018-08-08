# 就業創業資料--------------
d2 = read_csv("Data_Nei/datappl.csv")
d2 <- na.omit(d2)
d2 <- data.frame(d2)
d22 <- d2[c(1,2,4)] 
d22$"年度" = as.numeric(d22$"年度")
d22$"女性獲貸人數.人." = as.numeric(d22$"女性獲貸人數.人.")
d22 <- na.omit(d22)
d22 <- data.frame(d22)
d22 <- na.omit(d22)
d2 <- na.omit(d2)
d2 <- data.frame(d2)
d222 <- d2[c(1,2,4,6)] 
d222$"合計" = as.numeric(d222$"合計")
d222$"年度" = as.numeric(d222$"年度")
d222$"女性獲貸人數.人." = as.numeric(d222$"女性獲貸人數.人.")
d222 <- na.omit(d222)
d222 <- data.frame(d222)
d222 <- na.omit(d222)
t1 = read_xlsx("Data_Nei/table1.xlsx")
t1 <- na.omit(t1)
t1 <- data.frame(t1)
t11 <- t1[1:16,2:4]*1000
t11 <- data.frame(t11)
t11 <- na.omit(t11)
alld2t1 <- cbind(t11, d222[1:16,])
t2 = read_xlsx("Data_Nei/table2.xlsx")
t2 <- na.omit(t2)
t2 <- data.frame(t2)
t22 <- t2[1:16,4]/t2[1:16,3]
dm = read_csv("Data_Nei/datamoney.csv")
dm <- na.omit(dm)
dm <- data.frame(dm)
dm1 <- dm[c(2,4)] 
dm1 <- na.omit(dm1)
dm1 = as.matrix(dm1)
dm1 = as.numeric(dm1)
mycols <- runif(5,min=1,max=length(colors())) 
j1 = read_xlsx("Data_Nei/jobdata.xlsx")
j22 <- j1[,-5:-17]
j22 <- data.frame(j22)
j33 <- j22 %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=6)))
j1 = read_xlsx("Data_Nei/jobdata.xlsx")
j2 <- j1[,-3:-4]
j2 <- data.frame(j2)
j3 <- j2 %>% gather("item",value,3:15) %>% 
  bind_cols(data.frame(item_id=rep(1:13,each=6)))
j22 <- j2[,-3:-11]
j222 <- j22[,-4:-6]
j222 <- data.frame(j222)
j222 <- j222 %>% gather("item",value,3) %>% 
  bind_cols(data.frame(item_id=rep(3,each=6)))
j101 = read_xlsx("Data_Nei/job101.xlsx")
j101 <- data.frame(j101)
j101 <- j101 %>% gather("item",value,3:10) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=8)))
j103 = read_xlsx("Data_Nei/job103.xlsx")
j103 <- data.frame(j103)
j103 <- j103 %>% gather("item",value,3:10) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=8)))
j105 = read_xlsx("Data_Nei/job105.xlsx")
j105 <- data.frame(j105)
j105 <- j105 %>% gather("item",value,3:10) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=8)))
ld = read_xlsx("Data_Nei/licensedata.xlsx")
ld2 <- data.frame(ld[,-4:-9])
ld2 <- ld2 %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=6)))
ld = read_xlsx("Data_Nei/licensedata.xlsx")
ld1 <- data.frame(ld)
ld1 <- ld1 %>% gather("item",value,4:9) %>% 
  bind_cols(data.frame(item_id=rep(1:6,each=6)))
ldedu = read_xlsx("Data_Nei/licensedataedu.xlsx")
ldedu1 <- data.frame(ldedu[,-4:-9])
ldedu1 <- ldedu1 %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=24)))
ldedu = read_xlsx("Data_Nei/licensedataedu.xlsx")
ldedu11 <- data.frame(ldedu[,-10])
ldedu111 <- data.frame(ldedu11[,-3])
ldedu111 <- ldedu111 %>% gather("item",value,3:8) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=18)))
p = read_xlsx("Data_Nei/problem.xlsx")
p <- data.frame(p[,1:11])
p <- p %>% gather("item",value,3:11) %>% bind_cols(data.frame(item_id=rep(1:9,each=24)))
ss = read_xlsx("Data_Nei/salarysex.xlsx")
ss <- data.frame(ss)
ss <- ss %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=6)))
se = read_xlsx("Data_Nei/salaryedu.xlsx")
se <- data.frame(se)
se <- se %>% gather("item",value,3:4) %>% 
  bind_cols(data.frame(item_id=rep(1:2,each=24)))
te = read_xlsx("Data_Nei/timeedu.xlsx")
te1 <- data.frame(te[,1:8])
te1 <- te1 %>% gather("item",value,3:8) %>% 
  bind_cols(data.frame(item_id=rep(1:8,each=18)))
te = read_xlsx("Data_Nei/timeedu.xlsx")
te2 <- data.frame(te[,-3:-8])
te2 <- te2 %>% gather("item",value,3) %>% 
  bind_cols(data.frame(item_id=rep(1,each=24)))
