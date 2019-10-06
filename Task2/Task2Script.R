rm(list=ls())

library(rvest)
library(tidyverse)
library(lubridate)

html_session("https://w2.brreg.no/kunngjoring/kombisok.jsp?datoFra=01.01.2019&datoTil=07.10.2019&id_region=100&id_fylke=-+-+-&id_niva1=2&id_bransje1=0")

original <- read_html("https://w2.brreg.no/kunngjoring/kombisok.jsp?datoFra=01.01.2019&datoTil=07.10.2019&id_region=100&id_fylke=-+-+-&id_niva1=2&id_bransje1=0")
original <- html_nodes(original, xpath = "//table")
original <- html_nodes(original, 'table')
original <- html_nodes(original, xpath = "//p")
test <- html_text(original)
testlist <- as.list(test)

listnordland <- testlist[sum(grep("Nordland", testlist)+2):sum(grep("Troms", testlist)-2)]
listtroms <- testlist[sum(grep("Troms", testlist)+2):sum(grep("Finnmark", testlist)-2)]
listfinnmark <- testlist[sum(grep("Finnmark", testlist)+2):length(testlist)]

dfnord <- data.frame(matrix(listnordland, ncol=7, byrow=T))
dfnord <- dfnord[,c(1,3)]
names(dfnord) <- c("Name", "Date")
dfnord <- dfnord %>% mutate("Region" = "Nordland")

dftroms <- data.frame(matrix(listtroms, ncol=7, byrow=T))
dftroms <- dftroms[,c(1,3)]
names(dftroms) <- c("Name", "Date")
dftroms <- dftroms %>% mutate("Region" = "Troms")

dffinn <- data.frame(matrix(listfinnmark, ncol=7, byrow=T))
dffinn <- dffinn[,c(1,3)]
names(dffinn) <- c("Name", "Date")
dffinn <- dffinn %>% mutate("Region" = "Finnmark")

dffull <- bind_rows(dfnord, dftroms, dffinn)
dffull$Date <- dmy(dffull$Date)
dffull <- dffull %>% mutate("Month" = month(dffull$Date, label = TRUE, abbr = FALSE))

dfsummary <- dffull %>% group_by(Region, Month) %>% summarize(., "Number" = n())
best <- dfsummary %>% filter(Number == max(dfsummary$Number)) %>% collect()
best$Month <- gsub("janvier", "January", best$Month)

ggplot(dfsummary, aes(x=Month, y=Number, color=Region)) + geom_col(fill="white") + ylab("Newly registered companies") + ggtitle("Newly registered companies in 2019") + scale_color_grey() + theme_classic() + theme(legend.position = "bottom")