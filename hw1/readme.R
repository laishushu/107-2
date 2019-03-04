library(readr)
X201801_data <- read_csv("201901_data.csv")
View(X201901_data)

n = nchar(X201901_data$Message)
k = grep("柯文哲", X201901_data$Page_Name)
