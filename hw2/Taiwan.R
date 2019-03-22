library(readr)
library(dplyr)
library(ggplot2)
library(grid)
install.packages("ggpubr")
install.packages("corrplot")
library(corrplot)
library(ggpubr)
X201806_data <- read_csv("201806_data.csv")
X201807_data <- read_csv("201807_data.csv")
X201808_data <- read_csv("201808_data.csv")

page_count=X201806_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
#top10=merge(page_count,X201806_data,by="Page_Name")
#top10=top10[1:10,]

alldata=rbind(X201806_data,X201807_data,X201808_data)
mean(nchar(alldata$Page_Name))
filter(alldata,grepl("立法委員 黃偉哲",alldata$Page_Name)==T)%>%count()
filter(alldata,grepl("高思博 A Po",alldata$Page_Name)==T)%>%count()
filter(alldata,grepl("林義豐MarkLin",alldata$Page_Name)==T)%>%count()
filter(alldata,grepl("陳永和",alldata$Page_Name)==T)%>%count()

alldata$Date=as.POSIXct(alldata$Date,format="%Y/%m/%d %H:%M:%S")
huang=filter(alldata,grepl("立法委員 黃偉哲",alldata$Page_Name)==T)
huang_T=filter(alldata,grepl("立法委員 黃偉哲",alldata$Page_Name)==T&grepl("台南",alldata$Message)==T)%>%count()
huang_TR=filter(alldata,grepl("立法委員 黃偉哲",alldata$Page_Name)==T&grepl("旅遊",alldata$Message)==T)%>%count()

PO=filter(alldata,grepl("高思博 A Po",alldata$Page_Name)==T)
mark=filter(alldata,grepl("林義豐MarkLin",alldata$Page_Name)==T)
plot(huang$Date,huang$Comment_Count,type = "l",col="green")
lines(PO$Date,PO$Comment_Count,type = "l",col="blue")
lines(mark$Date,mark$Comment_Count,type = "l",col="red")

ggplot(huang, aes(x=Type)) + geom_bar(fill="lightblue", colour="black")+ylab("數量")+ggtitle("黃偉哲貼文類型")
#黃偉哲20186-8月的貼文類型

ggplot(huang, aes(x=Type, y=All_Reaction_Count)) + geom_boxplot()+ggtitle("黃偉哲貼文類型與留言數分析")
#黃偉著20186-8月貼文類型與留言數的

################################################################
huang$mes_nchar=nchar(huang$Message)
PO$mes_nchar=nchar(PO$Message)
ggscatter(huang,x="All_Reaction_Count",y="LIKE_COUNT",add="reg.line",conf.int = TRUE,
          cor.coef=TRUE,cor.method="pearson")
ggqqplot(huang$All_Reaction_Count) #常態分布
ggqqplot(huang$LIKE_Count)         #常態分布

#################################################################
#2019.03.18今日上課筆記
#留言長度分析
## r相關係數
#介於-1~1 >0正相關 <0負相關

#p value H0 期望假設VS虛無假設 設定假說(嗚嗚嗚 脫離不了統計了ㄚㄚㄚ)
#可以先分析一下要探討的變數之間有沒有關係，如果是低度相關的話其實做這樣的分析 沒有太大的意義。