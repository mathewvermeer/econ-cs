library("ggplot2")
library(plyr)

ransomware <- read.csv("https://ransomwaretracker.abuse.ch/feeds/csv/",header = TRUE,sep = ",",skip = 8)
ransomware <- ransomware[!(is.na(ransomware$Country) | ransomware$Country==""), ]
ransomware$Country <- substring(ransomware$Country, 0, 2)
total <- count(ransomware, "Country")
online <- ransomware[ransomware$Status == "online", ]
onlineCount <- count(online, "Country")

merged <- merge(total, onlineCount, by="Country")
merged$f <- merged$freq.y / merged$freq.x
thresholded <- merged[merged$freq.x > 10, ]
ordered <- merged[order(-merged$f), ]
orderedThresholded <- thresholded[order(-thresholded$f), ]
top10 <- ordered[1:10, ]
top10Thresholded <- orderedThresholded[1:10, ]

barplot(top10$f, names=top10$Country, xlab = "Country", ylab = "Fraction online", main = "Fraction of threats still online")
barplot(top10Thresholded$f, names=top10Thresholded$Country, xlab = "Country", ylab = "Fraction online", main = "Thresholded fraction of threats still online")
