#libraries needed
# install.packages("ggplot2")
library("ggplot2")
library(plyr)

#Download data
ransomware <- read.csv("https://ransomwaretracker.abuse.ch/feeds/csv/",header = TRUE,sep = ",",skip = 8)

#Clean data:
#Todo: translate URL to TLD, 
ransomware[, "Country"] = substring(ransomware[, "Country"], 0, 2)
ransomware = ransomware[!(is.na(ransomware$Country) | ransomware$Country==""), ]
countryCounts = count(ransomware, "Country")
topCountries = countryCounts[countryCounts$freq > 200, ]
sortedCountries = topCountries[order(-topCountries$freq), ]

#Investigate data:
#Investigate coocurance malware and threat
table(ransomware$Malware, ransomware$Threat)

#Investigate coocurance malware and country
table(ransomware$Malware, ransomware$Country)

#Investigate coocurance malware and registration company
table(ransomware$Malware, ransomware$Registrar)


#Plot data:
#plot frequency of mallware
barplot(table(ransomware['Malware']), ylab = "Frequency",las = 2)

#Plot frequency of countries
#Todo: plot only most occuring, color coded mallware
# barplot(sort(table(ransomware['Country'])), ylab = "Frequency",las = 2, horiz = TRUE)
barplot(sortedCountries$freq, names=sortedCountries$Country, xlab = "Country", ylab = "Frequency", main = "Infections per country")

#Plot frequency per registration company
#Todo: Only plot most occuring, indicate different mallware with different color
barplot(sort(table(ransomware['Registrar'])), ylab = "Frequency",las = 2, horiz = TRUE)

#Plot frequency per URL
#Todo: URL --> TLD, only most occuring, color coded mallware
barplot(table(ransomware['URL']), ylab = "Frequency",las = 2, horiz = TRUE)
