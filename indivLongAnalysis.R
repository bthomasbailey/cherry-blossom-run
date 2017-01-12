setwd("C:/MyGitRepos/cherry-blossom-run/Data")

load("cbMen.rda")

cbMenSub <- cbMen[cbMen$runTime > 30 & !is.na(cbMen$age) & cbMen$age > 15, ]

#Create function to clean up spaces in names
trimBlanks <- function(charVector){
    nameClean <- gsub("^[[:blank:]]+", "", charVector)
    nameClean <- gsub("[[:blank:]]+$", "", nameClean)
    nameClean <- gsub("[[:blank:]]+", " ", nameClean)
}

nameClean <- trimBlanks(cbMenSub$name)

length(nameClean)

length(unique(nameClean))

table(table(nameClean))

head(sort(table(nameClean), decreasing = T), 1)

mSmith <- cbMenSub[nameClean == "Michael Smith", ]

head(unique(mSmith$home))

nameClean <- tolower(nameClean)

head(sort(table(nameClean), decreasing = T), 1)

nameClean <- gsub("[,.]", "", nameClean)

tabNameYr <- table(cbMenSub$year, nameClean)

max(tabNameYr)
