library("ggplot2")
library(plyr)
library(data.table)
library(lubridate)

ransomware <- read.csv("https://ransomwaretracker.abuse.ch/feeds/csv/",header = TRUE,sep = ",",skip = 8)
ransomware <- ransomware[!(is.na(ransomware$Country) | ransomware$Country==""), ]
ransomware$Country <- substring(ransomware$Country, 0, 2)

ransomwareDS <- ransomware[ransomware$Threat =="Distribution Site",]
ransomwarePS <- ransomware[ransomware$Threat =="Payment Site",]
ransomwareC2 <- ransomware[ransomware$Threat =="C2",]
ransomwareCerber <- ransomware[ransomware$Malware == "Cerber",]
ransomwareX <- ransomware[ransomware$Country == "US" & ransomware$Threat =="Distribution Site" & ransomware$Malware == "Cerber",]

# plot no of incidents per month in 2016
bymonth <- data.table(ransomwareX)[,.N,by=list(format(as.Date(ransomwareX$X..Firstseen..UTC.), "%Y-%m"))]
ggplot(bymonth, aes(x=bymonth$format,y=bymonth$N)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="No. of new hosts (US, Cerber, Dist. Site)", x = "Month",y="No. of new hosts")
summary(bymonth)

# plot no of incidents per year
byyear <- data.table(ransomware)[,.N,by=format(as.Date(ransomware$X..Firstseen..UTC.), "%Y")]
ggplot(byyear, aes(x=byyear$format,y=byyear$N)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="No. of incidents per year", x = "Year",y="No. of incidents")
summary(byyear)

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
