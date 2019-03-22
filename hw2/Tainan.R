library(readr)
library(dplyr)
library(ggplot2)
library(grid)
install.packages("ggpubr")
install.packages("corrplot")
library(corrplot)
library(ggpubr)
X201801_data <- read_csv("201801_data.csv")
X201802_data <- read_csv("201802_data.csv")
X201803_data <- read_csv("201803_data.csv")
X201804_data <- read_csv("201804_data.csv")
X201805_data <- read_csv("201805_data.csv")
X201806_data <- read_csv("201806_data.csv")
X201807_data <- read_csv("201807_data.csv")
X201808_data <- read_csv("201808_data.csv")
X201809_data <- read_csv("201809_data.csv")
X201810_data <- read_csv("201810_data.csv")

Q1=rbind(X201801_data,X201802_data,X201803_data,X201804_data,X201805_data)
Q2=rbind(X201806_data,X201807_data,X201808_data,X201809_data,X201810_data)

page_count1=X201801_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
page_count2=X201802_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m02top10=page_count2[1:10,]
page_count3=X201803_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m03top10=page_count3[1:10,]
page_count4=X201804_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m04top10=page_count4[1:10,]
page_count5=X201805_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m05top10=page_count5[1:10,]
page_count6=X201806_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m06top10=page_count6[1:10,]
page_count7=X201807_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m07top10=page_count7[1:10,]
page_count8=X201808_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m08top10=page_count8[1:10,]
page_count9=X201809_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m09top10=page_count9[1:10,]
page_count10=X201810_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
m10top10=page_count10[1:10,]

mean(nchar(Q1$Page_Name))
filter(Q1,grepl("立法委員 黃偉哲",Q1$Page_Name)==T)%>%count()
filter(Q1,grepl("高思博 A Po",Q1$Page_Name)==T)%>%count()
filter(Q1,grepl("林義豐MarkLin",Q1$Page_Name)==T)%>%count()
filter(Q1,grepl("陳永和 台南市長候選人",Q1$Page_Name)==T)%>%count()

Q1$Date=as.POSIXct(Q1$Date,format="%Y/%m/%d %H:%M:%S")
huang=filter(Q1,grepl("立法委員 黃偉哲",Q1$Page_Name)==T)
PO=filter(Q1,grepl("高思博 A Po",Q1$Page_Name)==T)
Mark=filter(Q1,grepl("林義豐MarkLin",Q1$Page_Name)==T)
chen=filter(Q1,grepl("陳永和 台南市長候選人",Q1$Page_Name)==T)

##############黃偉哲貼文數量分析######################
filter(huang,grepl("photo",huang$Type)==T)%>%count()
#huangS=filter(huang,grepl("photo",huang$Type)==T)
filter(huang,grepl("video",huang$Type)==T)%>%count()
filter(huang,grepl("link",huang$Type)==T)%>%count()
filter(huang,grepl("status",huang$Type)==T)%>%count()
ggplot(huang, aes(x=Type)) + geom_bar(fill="lightgreen")+ylab("數量")+ggtitle("黃偉哲2018年1-5月貼文類型")
#ggplot(huang, aes(x=Comment_Count)) + geom_bar(fill="lightgreen")+ylab("數量")+ggtitle("黃偉哲2018年1-5月貼文留言數")
ggplot(huang, aes(x=Type, y=LIKE_COUNT)) + geom_boxplot()+ggtitle("黃偉哲貼文類型與按讚分析")
huang%>%group_by(LIKE_COUNT)%>%filter(LIKE_COUNT>3000)

##############高思博貼文數量分析######################
filter(PO,grepl("photo",PO$Type)==T)%>%count()
filter(PO,grepl("video",PO$Type)==T)%>%count()
filter(PO,grepl("link",PO$Type)==T)%>%count()
filter(PO,grepl("status",PO$Type)==T)%>%count()
filter(PO,grepl("event",PO$Type)==T)%>%count()
ggplot(PO,aes(x=Type)) + geom_bar(fill="skyblue")+ylab("數量")+ggtitle("高思博2018年1-5月貼文類型")
#ggplot(PO,aes(x=Comment_Count)) + geom_bar(fill="skyblue")+ylab("數量")+ggtitle("高思博2018年1-5月貼文留言數")
ggplot(PO,aes(x=Type, y=LIKE_COUNT)) + geom_boxplot()+ggtitle("高思博貼文類型與按讚分析")
PO%>%group_by(LIKE_COUNT)%>%filter(LIKE_COUNT>3000)

##############林義豐貼文數量分析######################
filter(Mark,grepl("photo",Mark$Type)==T)%>%count()
filter(Mark,grepl("video",Mark$Type)==T)%>%count()
filter(Mark,grepl("link",Mark$Type)==T)%>%count()
filter(Mark,grepl("status",Mark$Type)==T)%>%count()
filter(Mark,grepl("event",Mark$Type)==T)%>%count()
ggplot(Mark,aes(x=Type)) + geom_bar(fill="pink")+ylab("數量")+ggtitle("林義豐2018年1-5月貼文類型")
#ggplot(Mark,aes(x=Type)) + geom_bar(fill="pink")+ylab("數量")+ggtitle("林義豐2018年1-5月貼文留言數")
ggplot(Mark,aes(x=Type, y=LIKE_COUNT)) + geom_boxplot()+ggtitle("林義豐貼文類型與按讚分析")
Mark%>%group_by(LIKE_COUNT)%>%filter(LIKE_COUNT>3000)


##################################################################
huang$mes_nchar=nchar(huang$Message)
PO$mes_nchar=nchar(PO$Message)
ggscatter(huang,x="All_Reaction_Count",y="LIKE_COUNT",add="reg.line",conf.int = TRUE,
          cor.coef=TRUE,cor.method="pearson")
ggscatter(PO,x="All_Reaction_Count",y="LIKE_COUNT",add="reg.line",conf.int = TRUE,
          cor.coef=TRUE,cor.method="pearson")
ggqqplot(huang$All_Reaction_Count) #常態分布
ggqqplot(huang$LIKE_COUNT)  


plot(huang$Date,huang$Comment_Count,type = "l",col="green")
lines(PO$Date,PO$Comment_Count,type = "l",col="blue")
lines(Mark$Date,mark$Comment_Count,type = "l",col="red")
lines(chen$Date,chen$Comment_Count,type = "l",col="red")
#########################Q1############################

mean(nchar(Q2$Page_Name))
filter(Q2,grepl("立法委員 黃偉哲",Q2$Page_Name)==T)%>%count()
filter(Q2,grepl("高思博 A Po",Q2$Page_Name)==T)%>%count()
filter(Q2,grepl("林義豐MarkLin",Q2$Page_Name)==T)%>%count()
filter(Q2,grepl("陳永和 台南市長候選人",Q2$Page_Name)==T)%>%count()

###################################################################
#黃偉哲20181月到5月貼文中有提到台南的與沒有提到台南 按讚數的分析
huang_T=filter(X201805_data,grepl("立法委員 黃偉哲",X201805_data$Page_Name)==T&grepl("台南",X201805_data$Message)==T)
sum(huang_T$LIKE_COUNT)
huang_F=filter(Q1,grepl("立法委員 黃偉哲",Q1$Page_Name)==T&grepl("台南",Q1$Message)==F)
sum(huang_F$LIKE_COUNT)
huang_T=na.omit(huang_T) #可忽略遺漏值(前面是變數名字，括號加入要忽略的資料。)