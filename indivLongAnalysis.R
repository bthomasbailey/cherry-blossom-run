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

which(tabNameYr == max(tabNameYr))
indMax <- which(tabNameYr == max(tabNameYr), arr.ind = T)

colnames(tabNameYr)[indMax[2]]

###Add cleaned version of name to df
cbMenSub$nameClean <- nameClean

###Calculate year of birth for each runner by subtracting age from race year
cbMenSub$yob <- cbMenSub$year - cbMenSub$age

###Clean home variable and add to cbMenSub df
homeClean <- trimBlanks(cbMenSub$home)
homeClean <- tolower(homeClean)
homeClean <- gsub("[,.`]", "", homeClean)
cbMenSub$homeClean <- homeClean

###Look at values for Michael Brown, whose name appears 5 times in 2012
vars <- c("year", "homeClean", "nameClean", "yob", "runTime")
mb <- which(nameClean == "michael brown")
birthOrder <- order(cbMenSub$yob[mb])
cbMenSub[mb[birthOrder], vars]

###Create identifier for individuals
cbMenSub$ID <- paste(nameClean, cbMenSub$yob, sep="_")

###use tapply to determine how many times each ID appears
#tapply(Summary Var, Group Var, Function)
races <- tapply(cbMenSub$year, cbMenSub$ID, length)
races8 <- names(races)[which(races>=8)]
men8 <- cbMenSub[cbMenSub$ID %in% races8, ] 
orderByRunner <- order(men8$ID, men8$year)
men8 <- men8[orderByRunner, ]

