library(WDI)
library(ggplot2)
library(plyr)
library(corrplot)
library("Hmisc")

# IT.NET.SECR
# IT.NET.USER.ZS
# SP.POP.TOTL
# SP.POP.SCIE.RD.P6
# IP.JRN.ARTC.SC
# country=c('BE', 'GB', 'FI', 'IL', 'ID', 'IR', 'CL', 'SK', 'CY', 'KR', 'TH')

wdi <- WDI(indicator=c('IT.NET.USER.ZS', 'SP.POP.TOTL', 'IT.NET.SECR'), country=c('BE', 'GB', 'FI', 'IL', 'ID', 'IR', 'CL', 'SK', 'CY', 'KR', 'TH'), start=2016, end=2016)

ransomware <- read.csv("https://ransomwaretracker.abuse.ch/feeds/csv/",header = TRUE,sep = ",",skip = 8)
ransomware <- ransomware[!(is.na(ransomware$Country) | ransomware$Country==""), ]
ransomware$Country <- substring(ransomware$Country, 0, 2)
total <- count(ransomware, "Country")
online <- ransomware[ransomware$Status == "online", ]
onlineCount <- count(online, "Country")

merged <- merge(total, onlineCount, by="Country")
merged$f <- merged$freq.y / merged$freq.x


merged <- rename(merged, c("Country"="iso2c"))
merged <- merge(merged, wdi, by="iso2c")

drops <- c("iso2c","Country", "country", "year")
merged <- merged[ , !(names(merged) %in% drops)]

res <- cor(merged)
res2 <- rcorr(merged)
res2

corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(res2$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


thresholded <- merged[merged$freq.x > 10, ]
ordered <- merged[order(-merged$f), ]
orderedThresholded <- thresholded[order(-thresholded$f), ]
top10 <- ordered[1:10, ]
top10Thresholded <- orderedThresholded[1:10, ]

barplot(top10$f, names=top10$Country, xlab = "Country", ylab = "Fraction online", main = "Fraction of threats still online")
barplot(top10Thresholded$f, names=top10Thresholded$Country, xlab = "Country", ylab = "Fraction online", main = "Thresholded fraction of threats still online")

reg <-lm(merged$f ~ merged$IT.NET.USER.ZS + merged$SP.POP.TOTL + merged$IT.NET.SECR)
summary(reg)