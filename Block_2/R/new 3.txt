#libraries needed
#install.packages("ggplot2")
library("ggplot2")

#Download data
ransomware <- read.csv("https://ransomwaretracker.abuse.ch/feeds/csv/",header = TRUE,sep = ",",skip = 8)

#Clean data:
#Todo: Clean countries, translate URL to TLD, 

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
barplot(sort(table(ransomware['Country'])), ylab = "Frequency",las = 2, horiz = TRUE)

#Plot frequency per registration company
#Todo: Only plot most occuring, indicate different mallware with different color
barplot(sort(table(ransomware['Registrar'])), ylab = "Frequency",las = 2, horiz = TRUE)

#Plot frequency per URL
#Todo: URL --> TLD, only most occuring, color coded mallware
barplot(table(ransomware['URL']), ylab = "Frequency",las = 2, horiz = TRUE)
