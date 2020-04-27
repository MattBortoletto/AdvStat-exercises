# EXERCISE 1

name <- c("Loch Ness", "Loch Lomond", "Loch Morar", "Loch Tay", "Loch Awe", "Loch Maree",
          "Loch Ericht", "Loch Lochy", "Loch Rannoch", "Loch Shiel", "Loch Katrine", "Loch Arkaig", "Loch Shin")
volume <- c(7.45, 2.6, 2.3, 1.6, 1.2, 1.09, 1.08, 1.07, 0.97, 0.79, 0.77, 0.75, 0.35)
area <- c(56, 71, 27, 26.4, 39, 28.6, 18.6, 16, 19, 19.5, 12.4, 16, 22.5)
lenght <- c(39, 36, 18.8, 23, 41, 20, 23, 16, 15.7, 28, 12.9, 19.3, 27.8)
MaxDepth <- c(230, 190, 310, 150, 94, 114, 156, 162, 134, 128, 151, 109, 49)
MeanDepth <- c(132, 37, 87, 60.6, 32, 38, 57.6, 70, 51, 40, 43.4, 46.5, 15.5)

scottish.lakes <- data.frame(name, volume, area, lenght, MaxDepth, MeanDepth)

head(scottish.lakes)

scottish.lakes[which.max(scottish.lakes$volume),]

scottish.lakes[which.min(scottish.lakes$volume),]

scottish.lakes[which.max(scottish.lakes$area),]

scottish.lakes[which.min(scottish.lakes$area),]

scottish.lakes[order(area),]

TotArea <- sum(scottish.lakes$area)
TotArea


# EXERCISE 2

library("DAAG")
library("tibble")
library(help="DAAG")
?ais

data <- as_tibble(ais)

head(data)

tab1 <- table(data$sex, data$sport)
tab1

barplot(tab1, legend = rownames(tab1), xlab="Sport", las=2, cex.names=.65)
title (main = "Males and females for each sport")

any(is.na(data))

#par(mfrow=c(2,2))
boxplot(rcc ~ sport, data = ais ,
        ylab = "red blood cell count (in)" , xlab = "Sport",
        col = "darkorchid2", medcol = "white", las=2, cex.axis=.65, cex.lab=0.9)
boxplot(wcc ~ sport, data = ais ,
        ylab = "white blood cell count (in per liter)" , xlab = "Sport",
        col = "aquamarine2", medcol = "white", las=2, cex.axis=.65, cex.lab=0.9)
boxplot(hc ~ sport, data = ais ,
        ylab = "hematocrit (%)" , xlab = "Sport",
        col = "darkolivegreen2", medcol = "white", las=2, cex.axis=.65, cex.lab=0.9)
boxplot(hg ~ sport, data = ais ,
        ylab = "hemaglobin concentration (g/decaliter)" , xlab = "Sport",
        col = "goldenrod2", medcol = "white", las=2, cex.axis=.65, cex.lab=0.9)
title("Main blood variables", outer = TRUE, line = -2)

# non faccio tutti i possibili scatter plot, solo alcuni
p_area <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2, byrow=TRUE)
layout(p_area)
plot(data$rcc, data$wcc, pch = 20, cex=1.25, 
     col = ifelse(data$sex == "f", "navy", "darkorange"), asp = 1,
     xlab = "rcc", ylab = "wcc")
plot(data$rcc, data$hc, pch = 20, cex=1.25, 
     col = ifelse(data$sex == "f", "navy", "darkorange"), asp = 1,
     xlab = "rcc", ylab = "hc")
plot(data$rcc, data$hg, pch = 20, cex=1.25, 
     col = ifelse(data$sex == "f", "navy", "darkorange"), asp = 1,
     xlab = "rcc", ylab = "hg")
plot(data$wcc, data$hc, pch = 20, cex=1.25, 
     col = ifelse(data$sex == "f", "navy", "darkorange"), asp = 1,
     xlab = "wcc", ylab = "hc")
title("Some blood variables scatter plots", outer = TRUE, line = -2)

# EXERCISE 3

needed_packages <- c('lubridate','readxl', 'curl')
already_installed <- needed_packages %in% installed.packages()
for (pack in needed_packages [!already_installed]) {
  message (paste("To be installed :", pack , sep =" "))
  install.packages(pack)
}
library (lubridate)
library (readxl)
library (curl)
url <- "https://www.ecdc.europa.eu/sites/default/files/documents/"
fname <- "COVID-19-geographic-disbtribution-worldwide-"
date <- lubridate::today() - 1
ext = ".xlsx"
target <- paste(url, fname, date, ext, sep ="")
message ("target: ", target)
tmp_file <- tempfile("data", "/tmp", fileext = ext)
tmp <- curl::curl_download(target, destfile = tmp_file)
covid <- readxl::read_xlsx(tmp_file)

covid

lastDay <- subset(covid, dateRep==lubridate::today() - 1)
lastDay 
df <- lastDay[lastDay[, "cases"] > 200 & lastDay[, "deaths"] > 200,]
df

#TopCases <- covid[order(-covid$cases),]
#TopCasesID <- TopCases[!duplicated(TopCases$geoId),]
#df1 <- TopCasesID[1:10,]
#df1
#plot(df1$dateRep, df1$cases , pch=16, xlab="time")

TotCases <- aggregate(covid$cases, by=list(geoId=covid$geoId), FUN=sum)
top10cases <- TotCases[order(-TotCases$x),][1:10,]
top10cases

ord_cov <- covid[order(-covid$cases),]
count.max <- ord_cov[!duplicated(ord_cov$geoId),]
country <- count.max$geoId
cov_date <- covid[order(covid$dateRep),]
date_ord <- cov_date[!duplicated(cov_date$dateRep),]
date  <- date_ord$dateRep

library(ggplot2)
plot <- list()
for (i in 1:10){
t <- cov_date[cov_date$geoId==country[i],]
plot[[i]] <- ggplot(t, aes(x=dateRep, y=cases)) + geom_line()
}

print(plot[[1]])
print(plot[[2]])
print(plot[[3]])
print(plot[[4]])
print(plot[[5]])
print(plot[[6]])
print(plot[[7]])
print(plot[[8]])
print(plot[[9]])
print(plot[[10]])

