library(readr)
library(dplyr)
X201901_data <- read_csv("201901_data.csv")
View(X201901_data)

n = nchar(X201901_data$Message)
k = grep("柯文哲", X201901_data$Page_Name)
h = grep("韓國瑜", X201901_data$Message)
tours = gregexpr("高雄", X201901_data$Message)
han = data.frame(X201901_data$Message == "TRUE")
n1 = grepl("udn.com 聯合新聞網", X201901_data$Page_Name)
n2 = grep("蘋果日報 台灣", X201901_data$Page_Name)
n3 = grep("自由時報", X201901_data$Page_Name)
#n4 = grep("中時", X201901_data$Page_Name)&|

test=filter(X201901_data,grepl("udn.com 聯合新聞網", X201901_data$Page_Name))
udn_page_count=count(test)
page_count=X201901_data%>%group_by(Page_Name)%>%count()%>%arrange(desc(n))%>%filter(n>100)
top3 = merge(page_count,X201901_data,by="Page_Name")
page_count[1:3,]
