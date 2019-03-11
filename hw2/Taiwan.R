library(readr)
library(dplyr)
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

alldata$Date=as.POSIXct(alldata$Date,format="%Y/%m/%d %H:%M:%S")
huang=filter(alldata,grepl("立法委員 黃偉哲",alldata$Page_Name)==T)
huang_T=filter(alldata,grepl("立法委員 黃偉哲",alldata$Page_Name)==T&grepl("台南",alldata$Message)==T)%>%count()
PO=filter(alldata,grepl("高思博 A Po",alldata$Page_Name)==T)
plot(huang$Date,huang$Comment_Count,type = "l",col="green")
lines(PO$Date,PO$Comment_Count,type = "l",col="blue")

