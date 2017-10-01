library("ggplot2")
library(plyr)
library(data.table)
library(lubridate)

ransomware <- read.csv("https://ransomwaretracker.abuse.ch/feeds/csv/",header = TRUE,sep = ",",skip = 8)
ransomware <- ransomware[!(is.na(ransomware$Country) | ransomware$Country==""), ]
ransomware$Country <- substring(ransomware$Country, 0, 2)

# plot no of incidents per month
bymonth <- data.table(ransomware)[,.N,by=format(as.Date(ransomware$X..Firstseen..UTC.), "%Y-%m")]
ggplot(bymonth, aes(x=bymonth$format,y=bymonth$N)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="No. of incidents per month", x = "Month",y="No. of incidents")
summary(bymonth)

#plot occurences of range no. of incidents
byN2 <- data.frame(Range=character(),Occurences=integer())
group <- 76
interval <- max(bymonth$N)/group
for(i in 0:(group-1)){
  newrow <- data.frame(Range=paste((i*interval+1),(i+1)*interval,sep="-"),Occurences=length(which(bymonth$N >(i*interval) & bymonth$N <= ((i+1)*interval))))
  byN2 <- rbind(byN2,newrow)
  }
summary(byN2)
ggplot(byN2, aes(x=byN2$Range,y=byN2$Occurences)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Occurences of no. of incidents per month", x = "No. of incidents",y="No. of occurences")
