library("ggplot2")
library(plyr)

wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

ransomware <- read.csv("https://ransomwaretracker.abuse.ch/feeds/csv/",header = TRUE,sep = ",",skip = 8)
ransomware <- ransomware[!(is.na(ransomware$Registrar) | ransomware$Registrar==""), ]
total <- count(ransomware, "Registrar")
offline <- ransomware[ransomware$Status == "offline", ]
offlineCount <- count(offline, "Registrar")

merged <- merge(total, offlineCount, by="Registrar")
merged$f <- merged$freq.y / merged$freq.x
thresholded <- merged[merged$freq.x > 10, ]
ordered <- merged[order(merged$f), ]
orderedThresholded <- thresholded[order(thresholded$f), ]
top10 <- ordered[1:10, ]
top10Thresholded <- orderedThresholded[1:10, ]

barplot(top10$f, names=wrap.labels(top10$Registrar, 15), xlab = "Registrar", ylab = "Effectiveness", main = "Threat-removal effectiveness")
barplot(top10Thresholded$f, names=wrap.labels(top10Thresholded$Registrar, 15), xlab = "Registrar", ylab = "Effectiveness", main = "Thresholded threat-removal effectiveness")
