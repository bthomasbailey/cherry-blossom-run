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

###alternative orginization: store them as a list with an element for each ID in races8
men8L <- split(men8, men8$ID)
names(men8L) <- races8

###find records where performance varies by more than 20 minutes from year to year
gapTime <- tapply(men8$runTime, men8$ID, function(t) any(abs(diff(t))) > 20)
gapTime <- sapply(men8L, function(df) any(abs(diff(df$runTime)) > 20))
#How many runners are there like this?
sum(gapTime)
#show more data on first two of these runners
lapply(men8L[gapTime][1:2], function(df) df[, vars])

###determine state (or province/country) of each runner and add that to ID variable
homeLen <- nchar(cbMenSub$homeClean)
cbMenSub$state <- substr(cbMenSub$homeClean, start = homeLen - 1, stop = homeLen)
cbMenSub$state[cbMenSub$year == 2006] <- NA
cbMenSub$ID <- paste(cbMenSub$nameClean, cbMenSub$yob, cbMenSub$state, sep="_")

###select those IDs that occur at least 8 times
numRaces <- tapply(cbMenSub$year, cbMenSub$ID, length)
races8 <- names(numRaces)[which(numRaces>=8)]
men8 <- cbMenSub[cbMenSub$ID %in% races8, ] 
orderByRunner <- order(men8$ID, men8$year)
men8 <- men8[orderByRunner, ]

men8L <- split(men8, men8$ID)
names(men8L) <- races8
length(races8)


###Divide runners up into 9 groups
groups <- 1 + (1:length(men8L) %% 9)


addRunners <- function(listRunners, colors, numLty){
  numRunners <- length(listRunners)
  colIndx <- 1 + (1:numRunners) %% length(colors)
  ltys <- rep(1:numLty, each = length(colors), length = numRunners)
  
  mapply(function(df, i){
          lines(df$runTime ~ df$age,
                col = colors[colIndx[i]], lwd = 2, lty = ltys[i])    
        }, listRunners, i = 1:numRunners)
}


colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", 
            "#ff7f00", "#a65628")

par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))

invisible(
sapply(1:9, function(grpId){
  plot(x = 40, y = 60, type = "n",
       xlim = c(20, 80), ylim = c(50, 130),
       xlab = "Age(years)", ylab = "Run Time (minutes)")
  
  addRunners(men8L[groups == grpId], colors, numLty = 6)
}))


fitOne <- function(oneRunner, addLine = F, col = "grey"){
  lmOne <- lm(runTime ~ age, data = oneRunner)
  if(addLine){
    lines(x = oneRunner$age, y = predict(lmOne),
          col = col, lwd = 2, lty = 2)
  }  
  
  ind <- floor((nrow(oneRunner) + 1) / 2)
  res <- c(coefficients(lmOne)[2], oneRunner$age[ind], 
           predict(lmOne)[ind])
  names(res) <- c("ageCoeff", "medAge", "predRunTime")  
  return(res)
}

#look at just the 9th group to test out fitOne function
plot(x = 40, y = 60, type = "n",
     xlim = c(20, 80), ylim = c(50, 130),
     xlab = "Age(years)", ylab = "Run Time (minutes)")

addRunners(men8L[groups == 9], colors, numLty = 6)
lapply(men8L[groups == 9], fitOne, addLine = T, col = "black")

#fit lines for all 306 runners
men8LongFit <- lapply(men8L, fitOne)
coeffs <- sapply(men8LongFit, "[", "ageCoeff")
ages <- sapply(men8LongFit, "[", "medAge")

#Now regress coeffs on age. Is there positive relationship?
longCoeffs <- lm(coeffs ~ ages)
summary(longCoeffs)



