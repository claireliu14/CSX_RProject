# 姚承佑的分碼：失業問題

# 必要資源載入------------------------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(reshape2)
library(showtext)

library(RCurl)
library(XML)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(jsonlite)

library(base)
library(readr)
library(readxl)
library(splines)

# 基本變數宣告------------------------------------------------------------------------------------------

# showtext function
showtext_auto(enable = TRUE)
PlotThroughShowtext <- function(x)
{
  showtext.begin()
  print(x)
  showtext.end()
}

source('unemployment.R')
source('employment.R')
source('business.R')

#UI Part------------------------------------------------------------------------------------------
ui <- shinyUI (
  navbarPage ("臺灣青年就業環境狀況探討",
    
    # unemployment -----------------------------------
    navbarMenu ( "失業",
                 tabPanel("失業人口歷年統計",
                          titlePanel(h2("青年失業人口總統計")),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("Y_UN_YTOTAL_I", h3("年份選擇"),
                                          min = 82, max = 106, value = 105),
                              h3("詳細資料"),
                              verbatimTextOutput("Y_UN_YTOTAL_O_T")),
                            
                            mainPanel(
                              plotOutput("Y_UN_YTOTAL_O"),
                              plotOutput("Y_UN_YALL_O"))
                          )),
                 
                 tabPanel("失業原因歷年統計",
                          titlePanel (h2("青年失業原因總統計")),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("Y_UN_WHY_I", h3("年份選擇"),
                                          min = 82, max = 106, value = 105),
                              selectInput("Y_UN_WHY_I_S", label = h3("個別原因歷年成長"),
                                          choices = levels(factor(Y_UN_WHY_g[,3])),
                                          selected = levels(factor(Y_UN_WHY_g[,3]))[3])
                              ),
                            mainPanel(
                              plotOutput("Y_UN_WHY_O"),
                              plotOutput("Y_UN_WHY_O_S"))
                          )),
                 
                 tabPanel("失業與教育程度", titlePanel(h2("失業與教育程度統計")),
                          selectInput("Y_UN_EDU_I_S", label = h3("個別學歷歷年分佈"),
                                      choices = levels(factor(Y_UN_EDU_g[,3])),
                                      selected = levels(factor(Y_UN_EDU_g[,3]))[1]),
                          plotOutput("Y_UN_EDU_O"))
                 ),
    
    
    # employment -----------------------------------
    navbarMenu("就業",
               tabPanel("青年與其他年齡就業人口",
                        tags$h3("1978至2017年台灣就業人口調查數據"),
                        plotOutput("C_EMP_POP")),
               
               tabPanel("從月線看青年就業趨勢",
                        tags$h3("臺灣每月青年就業趨勢（1978至2017年每月就業平均數據）"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("C_EMP_MONTH_SLIDER", h6("Select year range"),
                                        sep="",
                                        min=min(xml.df.month$year),
                                        max=max(xml.df.month$year),
                                        value=c(min(xml.df.month$year), max(xml.df.month$year)))
                          ),
                          mainPanel(plotOutput("C_EMP_MONTH"))
                        )),
               
               tabPanel("青年勞工生活及就業狀況調查",
                        tags$h3("臺灣勞工生活及就業狀況調查"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('C_EMP_SATI_AGE', h6('Select age range'),
                                        unique(sati.age$細項), selectize = TRUE),
                            
                            selectInput('C_EMP_SATI_AGE_COMPARE', h6('選擇不同項目'),
                                        unique(sati.age$統計項目別), selectize = TRUE)
                          ),
                          mainPanel(
                            plotOutput("C_EMP_SATI"),
                            plotOutput("C_EMP_SATI_COMPARE"))
                        )),
               
               tabPanel("求職方法趨勢變化",
                        tags$h3("臺灣歷年就業者獲取現職的方法數據（1980-2017年）"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("C_GET_JOB_SLIDER", label = h6("Select year range"),
                                        sep = "",
                                        min = xml.get.job.year.min,
                                        max = xml.get.job.year.max,
                                        value = xml.get.job.year.max)
                          ),
                          mainPanel(plotOutput("C_EMP_GET_JOB"))
                        )),
               
               tabPanel("大專以上畢業生就業狀況分析",
                        tags$h3("不同學類學門畢業起薪（平均月薪）參考"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("C_GRA_SCHOOL_YEAR", label = h6("選擇畢業年度"),
                                         unique(data.price$畢業年度), inline=T), 
                            
                            radioButtons("C_EMP_SALARY_YEAR", label = h6("選擇薪資年"),
                                         unique(data.price$到職年), inline=T),
                            
                            radioButtons("C_GRA_EDU_LV", label = h6("選擇學歷"),
                                         unique(data.price$學歷), inline=T),
                            
                            selectInput("C_GRA_ACADEMY", label = h6("選擇學門"),
                                        unique(data.price$學門名稱), selectize = TRUE)
                          ),
                          mainPanel(
                            plotOutput("C_EMP_START_CLASS"),
                            plotOutput("C_EMP_START_STUDY")
                          )
                        ))
               ),
    
    navbarMenu("創業",
               tabPanel("創業貸款人數",
                        h1("台灣青年創業貸款人數"),
                        p("收集91~106年的資料，男性的人數皆是比女性多，而整體來看，103年是高峰，有最多創業貸款的人數，但近幾年有下滑的趨勢。"),
                        plotOutput("N_plot1")), 
               
               tabPanel("創業貸款人數與青年人數",
                        h1("台灣青創業貸款人數與青年人數"),
                        plotOutput("N_plot3")),        
               
               tabPanel("人數總比較",
                        h1("人數總比較"),
                        plotOutput("N_plot4"),
                        h1("創業人數占就業人數比例"),
                        plotOutput("N_plot41")),
               
               tabPanel("創業貸款金額",
                        h1("創業貸款金額"),
                        p("青年創業貸款的總金額也呈現下降的趨勢。"),
                        plotOutput("N_plot2"),
                        h2("性別與貸款金額"),
                        plotOutput("N_plot21"))
               ),
    
    navbarMenu("青年勞工調查",
               tabPanel("轉換工作情形",
                  h1("青年轉換工作意願"),
                  p("從圖表顯示，歷年來，沒有想換工作的比例居多，而在想換工作的青年當中，原因以待遇差為多，其次為工作發展無前景。然而因為想創業而換工作的比例並未最多，但從這三年來看，因創業而想換工作的比例有逐漸增加。不過，創業貸款的人數是呈現下降的，或許能反映出有越來越多青年有想創業的意願，但實際付出行動的卻不多。"),sidebarLayout(
                  sidebarPanel(
                    radioButtons("N_Choices", label = "Choices", 
                                 choices = list("有無打算轉換工作意願" = 1, "打算轉換工作原因" = 2,"因創業而想換工作" = 3))),
                                 mainPanel(plotOutput("N_plotjob")))),
               
               tabPanel("轉換工作與教育程度",
                        h1("101,103,105年度 青年打算轉換工作與教育程度之關聯"),
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("N_Choices2", label = "Choices", 
                                         choices = list("101年" = 4, "103年" = 5,"105年" = 6))),
                          mainPanel(plotOutput("N_plotjobedu")))),
               
               tabPanel("打算考證照情形",
                        h1("青年打算考證照情形"),
                        p("不論是從教育程度或者證照類別來看，青年打算考證照的意願是逐年降低的。"),
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("N_Choices3", label = "Choices", 
                                         choices = list("有無打算考證照" = 7, "打算考證照類別" = 8,"有無打算考證照和教育程度" = 9))),
                          mainPanel(plotOutput("N_plotlic")))),
               
               tabPanel("想考證照類別與教育程度",
                        h1("青年打算考證照之類別與教育程度"),
                        p("國中(及以下)和高中(職)的青年較多是打算考技術士證照，但隨著教育程度提高，則是想考語文證照的比例增加。"),
                        plotOutput("N_plot63")),
               
               tabPanel("初次尋職困難與教育程度",
                        h1("青年初次尋職所遇到的困難"),
                        p("不論教育程度為何，青年們所遭遇到最大的困難皆是(1)不知道自己適合哪方面的工作、(2)經驗不足。不過，可以發現的是，教育程度為高中(職)或國中的青年，尤其是僅有國中學歷的，他們遇到學歷不足的困難較其他教育程度別的青年多。"),
                        plotOutput("N_plot7")),
               
               tabPanel("薪資狀況",
                        h1("男性和女性青年之薪資狀況"),
                        p("以這三年的數據來看，男性的初次尋職或現職之工作平均每月薪資皆略高於女性，但整體來看，兩者的薪資都逐漸增加。"),
                        plotOutput("N_plot8")),
               
               tabPanel("薪資狀況與教育程度",
                        h1("青年薪資狀況與教育程度之關係"),
                        p("下表呈現出青年的教育程度越高，其初次尋職之工作或現職之工作的平均每月薪資也較高。"),
                        plotOutput("N_plot81")),
               
               tabPanel("初次尋職時間與教育程度關係",
                        h1("青年初次尋職時間與教育程度關係"),
                        plotOutput("N_plot9"),
                        plotOutput("N_plot91")))
    )
)

#Server Part------------------------------------------------------------------------------------------
server <- function(input, output)
{
  
  #失業人口歷年統計------------------------------------------------------------------------------------------
  output$Y_UN_YTOTAL_O <-renderPlot({
    g<-ggplot(Y_UN_YTOTAL, aes(x = YEAR)) +
      geom_area(aes(y = TOTAL, group = 1,fill = "總計",color="總計")) +
      geom_area(aes(y = MALE, group = 2,fill="男",color = "男")) +
      geom_area(aes(y = FEMALE, group = 3,fill="女",color = "女")) +
      scale_x_continuous(breaks=Y_UN_YTOTAL$YEAR) +
      scale_y_continuous(breaks=seq(44,271,by=8)) +
      scale_fill_manual(values=c("RED", "BLUE", "GREEN"))+
      scale_colour_manual(values=c("black", "black", "black")) +
      geom_vline(xintercept = input$Y_UN_YTOTAL_I,
                 color = "black", size=1) +
      geom_hline(yintercept=Y_UN_YTOTAL[input$Y_UN_YTOTAL_I-81,2], linetype="dashed",
                 color = "black", size=0.5)+
      geom_hline(yintercept=Y_UN_YTOTAL[input$Y_UN_YTOTAL_I-81,3], linetype="dashed", 
                 color = "black", size=0.5)+
      geom_hline(yintercept=Y_UN_YTOTAL[input$Y_UN_YTOTAL_I-81,4], linetype="dashed", 
                 color = "black", size=0.5) +
      labs(x="年(民國)",y="人數(千人)",title = "青年失業人口總統計\n\n民國82年至106年(15~29歲)\n\n單位：千人",
           caption="資料來源：中華民國統計資料網\n\n(表18　歷年失業者之年齡)")+
      guides(color=FALSE)
    PlotThroughShowtext(g)
  }
  )
  output$Y_UN_YTOTAL_O_T <-renderText(
    {
      paste(
        "青年(15~29歲)失業人口資料\n",
        "民國:",input$Y_UN_YTOTAL_I,"年\n",
        "青年失業總人口:",Y_UN_YTOTAL[input$Y_UN_YTOTAL_I -81,2],"(千人)\n",
        "青年男性失業人口:",Y_UN_YTOTAL[input$Y_UN_YTOTAL_I -81,3],"(千人)\n",
        "青年女性失業人口:",Y_UN_YTOTAL[input$Y_UN_YTOTAL_I -81,4],"(千人)\n",
        "全國失業總人口:",Y_UN_YALL[input$Y_UN_YTOTAL_I -81,2],"(千人)\n",
        "青年失業率佔全國：",round(((Y_UN_YTOTAL[input$Y_UN_YTOTAL_I -81,2]/Y_UN_YALL[input$Y_UN_YTOTAL_I -81,2])*100),digits=3),"%"
      )
    }
  )
  output$Y_UN_YALL_O <-renderPlot({
    g<-ggplot(data=subset(Y_UN_YALL_g[],Y_UN_YALL_g[,1]==input$Y_UN_YTOTAL_I),aes(x=YEAR,y=age_un,fill=age)) +
      geom_bar(stat = "identity") +
      coord_polar("y", start=0)+
      scale_x_continuous(breaks=c(0,0,0))+
      labs(x=paste("民國 ",input$Y_UN_YTOTAL_I,"年"),y="人數(千人)",title = paste("民國",input$Y_UN_YTOTAL_I,"年失業人口各年齡結構\n\n單位：千人"),
           caption="資料來源：中華民國統計資料網\n\n(表18　歷年失業者之年齡)")
    PlotThroughShowtext(g)
  }
  )
  #失業原因歷年統計------------------------------------------------------------------------------------------
  output$Y_UN_WHY_O <- renderPlot(
    {
      g<-ggplot(subset(Y_UN_WHY_g,YEARS==input$Y_UN_WHY_I&!AGE=="TOTAL"), aes(x=why, y=why_un, fill=AGE)) + 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme(legend.position = "top")+
        labs(x="人數(人)",y="原因",title = paste("民國",input$Y_UN_WHY_I,"年青年各原因失業人數\n\n單位：人"),
             caption="資料來源：中華民國統計資料網\n\n(表61　失業者之失業原因－按年齡分齡)")
      PlotThroughShowtext(g)
    }
  )
  output$Y_UN_WHY_O_S <- renderPlot(
    {
      Y_UN_WHY_Data_X = subset(Y_UN_WHY,select = c("YEARS","AGE",input$Y_UN_WHY_I_S))
      Y_UN_WHY_Data_X = dcast(Y_UN_WHY_Data_X, YEARS~AGE,sum)
      colnames(Y_UN_WHY_Data_X) =  c("YEARS","FIF","TWEN","TWENFITH","TOTAL")
      #  Y_UN_WHY_Data_X = Y_UN_WHY_Data_X[-26,-2]
      g<-ggplot(Y_UN_WHY_Data_X,aes(x=YEARS)) +
        geom_line(aes(y = TOTAL,group = 1,color="TOTAL")) +
        geom_line(aes(y = FIF,group = 2,color="15~19")) +
        geom_line(aes(y = TWEN,group = 3,color="20~25")) +
        geom_line(aes(y = TWENFITH,group = 4,color="25~29")) +
        scale_x_continuous(breaks=Y_UN_WHY_Data_X$YEAR)+
        scale_y_continuous(
          breaks=seq
          (
            min(
              Y_UN_WHY_Data_X$FIF,
              Y_UN_WHY_Data_X$TWEN,
              Y_UN_WHY_Data_X$TWENFITH
            ),
            max(
              Y_UN_WHY_Data_X$TOTAL
            ),
            (
              as.integer
              (
                (
                  max
                  (
                    Y_UN_WHY_Data_X$TOTAL
                  )
                  -min
                  (
                    Y_UN_WHY_Data_X$FIF,
                    Y_UN_WHY_Data_X$TWEN,
                    Y_UN_WHY_Data_X$TWENFITH
                  )
                )
                /25
              )
            )
          )
        ) +
        #scale_fill_manual(values=c("RED", "BLUE", "GREEN"))+
        #s
        geom_vline(xintercept = input$Y_UN_WHY_I,
                   color = "black", size=1)+
        geom_hline(yintercept=Y_UN_WHY_Data_X[input$Y_UN_WHY_I-81,5], linetype="dashed",
                   color = "black", size=0.5)+
        geom_hline(yintercept=Y_UN_WHY_Data_X[input$Y_UN_WHY_I-81,4], linetype="dashed", 
                   color = "black", size=0.5)+
        geom_hline(yintercept=Y_UN_WHY_Data_X[input$Y_UN_WHY_I-81,3], linetype="dashed", 
                   color = "black", size=0.5) +
        geom_hline(yintercept=Y_UN_WHY_Data_X[input$Y_UN_WHY_I-81,2], linetype="dashed", 
                   color = "black", size=0.5)+
        labs(x="年(民國)",y="人數(人)",title = paste(input$Y_UN_WHY_I_S," 的歷年青年失業人數統計\n\n單位：人"),
             caption="資料來源：中華民國統計資料網\n\n(表61　失業者之失業原因－按年齡分齡)")
      PlotThroughShowtext(g)
    }
  )
  #失業與教育程度統計------------------------------------------------------------------------------------------
  output$Y_UN_EDU_O <- renderPlot(
    {
      Y_UN_EDU_Data_X <-subset(Y_UN_EDU,Y_UN_EDU$AGE == "TOTAL")
      Y_UN_EDU_Data_X <-subset(Y_UN_EDU_Data_X[,-2])
      Y_UN_EDU_Data_Y <-subset(Y_UN_EDU_g,!Y_UN_EDU_g$AGE == "TOTAL")
      Y_UN_EDU_Data_Y <-subset(Y_UN_EDU_Data_Y,Y_UN_EDU_Data_Y$edu == input$Y_UN_EDU_I_S)
      Y_UN_EDU_Data_Y <-subset(Y_UN_EDU_Data_Y[,-3])
      g<-ggplot() + 
        #直條圖
        geom_bar(data=Y_UN_EDU_Data_Y,aes(x= YEAR, y= edu_un, fill=AGE,alpha=12),stat="identity",position = position_stack(reverse = TRUE))+
        # 線
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,2],group = 1,color="國小及以下"))+
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,3],group = 2,color="國中")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,4],group = 3,color="高中")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,5],group = 4,color="高職")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,6],group = 5,color="專科")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,7],group = 6,color="大學")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,8],group = 7,color="研究所"))+
        #刻度
        scale_x_continuous(breaks=Y_UN_EDU_Data_X$YEAR)+
        scale_y_continuous(breaks=seq(min(Y_UN_EDU_g$edu_un),max(Y_UN_EDU_g$edu_un),as.integer((max(Y_UN_EDU_g$edu_un)-min(Y_UN_EDU_g$edu_un))/25))) +
        labs(x="年(民國)",y="人數(人)",title = "民國100~106年\n\n歷年失業與教育人數統計\n\n單位：人",
             caption="資料來源：中華民國統計資料網\n\n(表59　失業者之教育程度－按年齡分)")+
        #關閉圖示
        guides(alpha=FALSE)
      PlotThroughShowtext(g)
    }
  )
  
  
  # 就業------------------------------------------------------
  output$C_EMP_POP <- renderPlot({
    g <- data.frame(year = emp.year, young_pop = emp.young, other_pop = emp.other) %>%
      ggplot(aes(x = year, group = 1)) +
      geom_density(aes(y = young_pop), stat = "identity", fill="yellow", alpha=.5) +
      geom_density(aes(y = other_pop), stat = "identity", fill="red", alpha=.3) +
      labs(x = "年", y = "就業人口數（單位：千人）", 
           title = "1978至2017年台灣就業人口調查數據",
           caption="資料來源：政府資料開放平臺 - 人力資源調查就業人數") +
      theme(text=element_text(family="wqy-microhei"))
    PlotThroughShowtext(g)
  })
  
  
  output$C_EMP_MONTH <- renderPlot({
    g <- data.frame(year=xml.df.month$year, month=xml.df.month$month, young=emp.month.young) %>%
      filter(year>=input$C_EMP_MONTH_SLIDER[1] & year <= input$C_EMP_MONTH_SLIDER[2]) %>%
      group_by(month) %>%
      summarise(young = mean(young)) %>%
      ggplot(aes(x=month, y=young, group=1)) + geom_point() + geom_line() +
      labs(x = "月", y = "就業人口數（單位：千人）", 
           title = "臺灣每月青年就業趨勢（1978至2017年每月就業平均數據）",
           caption="資料來源：政府資料開放平臺 - 人力資源調查就業人數") +
      theme(text=element_text(family="wqy-microhei"))
    PlotThroughShowtext(g)
  })
  
  output$C_EMP_SATI <- renderPlot({
    g <- sati.data %>%
      filter(程度!="樣本數（人）" & 細項==input$C_EMP_SATI_AGE) %>%
      ggplot(aes(x=0, y=value, fill=程度)) +
      geom_bar(stat="identity") +
      coord_polar("y", start=0) +
      theme(axis.text.y=element_blank(), 
            axis.title.y = element_blank(), 
            axis.ticks.y=element_blank(),
            text=element_text(family="wqy-microhei")) +
      facet_wrap(~統計項目別)+
      labs(title = "臺灣15-24歲青年勞工生活及就業狀況調查", y=element_blank(),
           caption="資料來源：政府資料開放平臺 - 勞工生活及就業狀況調查")
    PlotThroughShowtext(g)
  })
  
  output$C_EMP_SATI_COMPARE <- renderPlot({
    g <- sati.data %>%
      filter(統計項目別==input$C_EMP_SATI_AGE_COMPARE & 程度!="樣本數（人）" ) %>%
      ggplot(aes(x=細項, y=value, color=程度, group=程度)) +
      geom_point() + geom_line() +
      theme(text=element_text(family="wqy-microhei"))+
      labs(title = "不同統計項目別之滿意度檢視", y="滿意度", x="年齡分佈",
           caption="資料來源：政府資料開放平臺 - 勞工生活及就業狀況調查")
    PlotThroughShowtext(g)
  })
  
  
  output$C_EMP_GET_JOB <- renderPlot({
    g <- xml.get.job.df %>%
      filter(項目別_Iterm<=input$C_GET_JOB_SLIDER) %>%
      ggplot(aes(x=項目別_Iterm, y=value/總__計_Total, color=方法)) +
      geom_line() + geom_point() +
      theme(text=element_text(family="wqy-microhei")) +
      labs(title = "臺灣歷年就業者獲取現職的方法數據（1980-2017年）", y="年", x="比例",
           caption="資料來源：政府資料開放平臺 - 歷年就業者之獲得現職方法")
    PlotThroughShowtext(g)
  })
  
  output$C_EMP_START_CLASS <- renderPlot({
    g <- data.price %>% 
      filter(畢業年度==input$C_GRA_SCHOOL_YEAR & 到職年==input$C_EMP_SALARY_YEAR & 學歷==input$C_GRA_EDU_LV) %>%
      ggplot(aes(x=學門名稱, y=value, color=學門名稱)) +
      geom_boxplot()+
      geom_hline(aes(yintercept=mean(value)),linetype=5,col="red")+
      theme(text=element_text(family="wqy-microhei"),
            legend.position="none") +
      labs(title = "大專以上不同學門畢業生就業平均月薪", y="平均月薪",
           caption="資料來源：政府資料開放平臺 - 大專畢業生就業概況分析") +
      ylim(min, max) + coord_flip()
    PlotThroughShowtext(g)
  })
  
  output$C_EMP_START_STUDY <- renderPlot({
    g <- data.price %>% 
      filter(畢業年度==input$C_GRA_SCHOOL_YEAR & 到職年==input$C_EMP_SALARY_YEAR & 學歷==input$C_GRA_EDU_LV) %>%
      filter(學門名稱==input$C_GRA_ACADEMY) %>%
      ggplot() +
      geom_histogram(aes(x=reorder(學類名稱,value), y=value, fill=value), stat="identity") +
      geom_hline(aes(yintercept=mean(value)),linetype=5,col="red")+
      theme(text=element_text(family="wqy-microhei"),
            axis.text.x=element_text(angle=90, hjust=1),
            legend.position="none") +
      labs(title = "大專以上特定學門不同學類畢業生就業平均月薪", y="平均月薪", x="學類名稱",
           caption="資料來源：政府資料開放平臺 - 大專畢業生就業概況分析") +
      scale_colour_gradient2() + coord_flip()
    PlotThroughShowtext(g)
  })
  
  
  # business ------------------------------------------------------
  # 創業就業資料----  
  output$N_plot1 <- renderPlot({N1 <- ggplot(d222, aes(x = 91:106)) + 
    geom_point(aes(y = d222[,3])) + 
    geom_line(aes(y = d222[,3],  color="女性獲貸人數")) +
    geom_point(aes(y = d222[,2])) + 
    geom_line(aes(y= d222[,2], color="男性獲貸人數")) +
    geom_point(aes(y = d222[,4])) + 
    geom_line(aes(y= d222[,4], color="總獲貸人數")) + xlab("年") + ylab("人數")+
    theme(text=element_text(family="wqy-microhei")) 
  PlotThroughShowtext(N1)})
  
  output$N_plot2 <- renderPlot({N2 <- barplot(dm[,6],width = 1, space = NULL,beside = TRUE, col = c("#4FB0C6","#4F86C6","#C65146","#EC6A5C","#e97f02","#f8ca00","#8FBC94","#548687","#6E7783","#77AAAD","#99CCCC","#FFCC99","#CC9999","#CCCC99","#0099CC","#FF6666","#996699","#666666","#996697"), xlab = "年度", ylab = "貸款金額(千元)", legend=dm$"年度")+
    theme(text=element_text(family="wqy-microhei")) 
  PlotThroughShowtext(N2)})
  
  output$N_plot3 <-renderPlot({N3 <-ggplot(alld2t1, aes(x = 91:106)) + 
    geom_point(aes(y = alld2t1[,1])) + 
    geom_line(aes(y = alld2t1[,1],  color="總青年人數")) +
    geom_point(aes(y = alld2t1[,2])) + 
    geom_line(aes(y= alld2t1[,2], color="男性青年人數")) +
    geom_point(aes(y = alld2t1[,3])) + 
    geom_line(aes(y= alld2t1[,3], color="女性青年人數"))+
    geom_point(aes(y = alld2t1[,5])) + 
    geom_line(aes(y= alld2t1[,5], color="男性青年創業貸款人數"))+
    geom_point(aes(y = alld2t1[,6])) + 
    geom_line(aes(y= alld2t1[,6], color="女性青年創業貸款人數"))+
    geom_point(aes(y = alld2t1[,7])) + 
    geom_line(aes(y= alld2t1[,7], color="總青年創業貸款人數")) + xlab("年") + ylab("人數")+
    theme(text=element_text(family="wqy-microhei")) 
  PlotThroughShowtext(N3)}) 
  
  output$N_plot4 <-renderPlot({N4 <-ggplot(t2, aes(x = 91:106)) + 
    geom_point(aes(y =t2[,4])) + 
    geom_line(aes(y = t2[,4],  color="青年創業貸款總人數")) +
    geom_point(aes(y = t2[,2])) + 
    geom_line(aes(y= t2[,2], color="青年失業總人數")) +
    geom_point(aes(y = t2[,3])) + 
    geom_line(aes(y= t2[,3], color="青年就業總人數"))+
    geom_point(aes(y = t2[,5])) + 
    geom_line(aes(y= t2[,5], color="青年總人數")) + xlab("年") + ylab("千人")+
    theme(text=element_text(family="wqy-microhei")) 
  PlotThroughShowtext(N4)})
  
  output$N_plot41 <-renderPlot({N41 <-barplot(t22, col = "skyblue",xlab = "91年至106年", ylab = "創業貸款人數占就業人數比例")+
    theme(text=element_text(family="wqy-microhei")) 
  PlotThroughShowtext(N41)})
  
  output$N_plot21 <-renderPlot({N21 <-barplot(dm1,width = 3, space = NULL,beside = TRUE, col = c("skyblue", "pink"), xlab = "年度", ylab = "貸款金額(千元)", legend=c("男","女"))+
    theme(text=element_text(family="wqy-microhei")) 
  PlotThroughShowtext(N21)})
  
  output$N_plotjob <-renderPlot({if (input$N_Choices == "1"){Njob <-ggplot(j33,aes(x=年,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
    labs(title="青年有無打算轉換工作意願",y = "百分比值") + 
    scale_x_continuous(breaks=seq(101,105,by=2))+
    theme(text=element_text(family="wqy-microhei")) 
  PlotThroughShowtext(Njob)}
    else if (input$N_Choices == "2"){Njob2 <-ggplot(j3,aes(x=年,value))+
      geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
      labs(title="青年打算轉換工作原因",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))+
      theme(text=element_text(family="wqy-microhei")) 
    PlotThroughShowtext(Njob2)}
    else if (input$N_Choices == "3"){Njob3 <- ggplot(j222,aes(x=年,value))+
      geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
      labs(title="青年因創業而打算轉換工作",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))+
      theme(text=element_text(family="wqy-microhei")) 
    PlotThroughShowtext(Njob3)}})
  
  output$N_plotjobedu <- renderPlot({if (input$N_Choices2 == "4"){Njobedu <- ggplot(j101,aes(x = 教育程度,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
    labs(title="101年青年打算轉換工作情形與教育程度",y = "百分比值")+
    theme(text=element_text(family=("wqy-microhei")))  
  PlotThroughShowtext(Njobedu)}
    else if (input$N_Choices2 == "5"){N103 <- ggplot(j103,aes(x = 教育程度,value))+
      geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
      labs(title="103年青年打算轉換工作情形與教育程度",y = "百分比值", 
           theme(text=element_text(family=("wqy-microhei")))) 
    PlotThroughShowtext(N103)}
    else if(input$N_Choices2 == "6"){N105 <-ggplot(j105,aes(x = 教育程度,value))+
      geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.5)+
      labs(title="105年青年打算轉換工作情形與教育程度",y = "百分比值")+
      theme(text=element_text(family=("wqy-microhei"))) 
    PlotThroughShowtext(N105)}})
  
  output$N_plotlic <- renderPlot({if (input$N_Choices3 == "7"){Nlic <-ggplot(ld2,aes(x = 年,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+
    labs(title="青年有無打算考證照比例",y = "百分比值") +
    scale_x_continuous(breaks=seq(101,105,by=2))+theme(text=element_text(family="wqy-microhei")) 
  PlotThroughShowtext(Nlic)}
    else if (input$N_Choices3 == "8"){Nlic1 <- ggplot(ld1,aes(x = 年,value))+
      geom_bar(aes(fill=item),stat = "identity",position="dodge",width=1.5)+
      labs(title="青年打算考證照類別比例",y = "百分比值") + scale_x_continuous(breaks=seq(101,105,by=2))+
      theme(text=element_text(family=("wqy-microhei"))) 
    PlotThroughShowtext(Nlic1)}
    else if (input$N_Choices3 == "9"){Nlic2 <- ggplot(ldedu1,aes(x = 教育程度,value))+
      geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +
      labs(title="青年有無打算考證照比例",y = "百分比值")+
      theme(text=element_text(family=("wqy-microhei"))) 
    PlotThroughShowtext(Nlic2)}})
  
  output$N_plot63 <- renderPlot({N_licedu <- ggplot(ldedu111,aes(x = 教育程度,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +
    labs(title="教育程度和青年所想考證照類別之關聯",y = "百分比值")+
    theme(text=element_text(family=("wqy-microhei"))) 
  PlotThroughShowtext(N_licedu)})
  
  output$N_plot7<- renderPlot({N_diffic <- ggplot(p,aes(x = 教育程度,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8)+ facet_grid(年~.) +labs(title=" 青年勞工初次尋職困難與教育程度",y = "百分比值")+
    theme(text=element_text(family=("wqy-microhei"))) 
  PlotThroughShowtext(N_diffic)})
  
  output$N_plot8<- renderPlot({Nsex <- ggplot(ss,aes(x = 性別,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+
    labs(title="青年勞工初次尋職與現職工作平均每月薪資比較", y = "平均每月薪資(元)")+
    theme(text=element_text(family=("wqy-microhei"))) 
  PlotThroughShowtext(Nsex)})
  
  output$N_plot81<- renderPlot({Nmoney <-ggplot(se,aes(x = 教育程度,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+
    labs(title="青年勞工初次尋職與現職工作平均每月薪資比較", y = "平均每月薪資(元)", 
         theme(text=element_text(family=("wqy-microhei")))) 
  PlotThroughShowtext(Nmoney)})
  
  output$N_plot9<- renderPlot({NN <- ggplot(te1,aes(x = 教育程度,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+
    labs(title=" 青年勞工初次尋職時間與教育關係",y = "百分比(%)", 
         theme(text=element_text(family=("wqy-microhei"))))
  PlotThroughShowtext(NN)})
  
  output$N_plot91<- renderPlot({NNN <-ggplot(te2,aes(x = 教育程度,value))+
    geom_bar(aes(fill=item),stat = "identity",position="dodge",width=0.8) + facet_grid(年~.)+
    labs(title=" 青年勞工初次尋職時間與教育關係",y = "月", 
         theme(text=element_text(family=("wqy-microhei"))))
  PlotThroughShowtext(NNN)})
}

#執行------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

