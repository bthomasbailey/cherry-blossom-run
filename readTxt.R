
setwd("C:/MyGitRepos/cherry-blossom-run/Data")

# els <- readLines("MenTxt/2012.txt")
# eqIndex <- grep("^===", els)
# spacerRow <- els[eqIndex]
# headerRow <- els[eqIndex - 1]
# body <- els[-(1:eqIndex)]
# 
# headerRow <- tolower(headerRow)

# ageStart <- regexpr("ag", headerRow)
# age <- substr(body, start = ageStart, stop = ageStart + 1)

# blankLocs <- gregexpr(" ", spacerRow)
# searchLocs <- c(0, blankLocs[[1]])

# Values <- mapply(substr, list(body), start = searchLocs[-length(searchLocs) + 1], stop = searchLocs[-1] - 1)


findColLocs <- function(spacerRow) {
    
    spaceLocs <- gregexpr(" ", spacerRow)[[1]]
    rowLength <- nchar(spacerRow)
    
    if (substring(spacerRow, rowLength, rowLength) != " ") {
        return (c(0, spaceLocs, rowLength + 1))
    } else {
        return (c(0, spaceLocs))
    }
}


selectCols <- function(colNames, headerRow, searchLocs) {
    sapply(colNames,
           function(name, headerRow, searchLocs) {
               startPos <- regexpr(name, headerRow)[[1]]
               if (startPos == -1) {
                   return(c(NA, NA))
               }
               
               index <- sum(startPos >= searchLocs)
               c(searchLocs[index] + 1, searchLocs[index + 1])
           },
           headerRow = headerRow, searchLocs = searchLocs)
    
}

# searchLocs <- findColLocs(spacerRow)
# ageLoc <- selectCols("ag", headerRow, searchLocs)
# ages <- mapply(substr, list(body), start = ageLoc[1,], stop = ageLoc[2,])
# 
# shortColNames <- c("name", "home", "ag", "gun", "net", "time")
# 
# locCols <- selectCols(shortColNames, headerRow, searchLocs)
# Values <- mapply(substr, list(body), start = locCols[1,], stop = locCols[2,])
# class(Values)
# colnames(Values) <- shortColNames


extractVariables <- function(file, varNames = c("name", "home", "ag", "gun", "net", "time"),
                             sex, year) {
    
    #Find the index of the footer row
    footIndex <- grep("^[[:blank:]]*[#|*]", file)
    
    #Find the index of rows that are completely blank
    blankIndex <- grep("^[[:blank:]]*$", file)
    
    if(sex == "W" & year == 2001){
        #women's file for 2001 does not contain spacer or header rows
        body <- file[-c(footIndex, blankIndex)]
        locCols<-matrix(c(13, 34, 38, 56, 35, 37, 65, 72, 57, 64, NA, NA), nrow = 2)
        colnames(locCols) <- varNames
        
    } else {
        #Find the index of the row with equal signs
        eqIndex <- grep("^===", file)    
        
        #Extract the two key rows and the data (fix men 2006 spacer row)
        spacerRow <- file[eqIndex]
        headerRow <- tolower(file[eqIndex - 1])
        
        if (year == 2006){
            locNetTime <- regexpr("net", headerRow)
            spacerRow <- paste(substr(spacerRow, 1, locNetTime - 2), 
                               substr(spacerRow, locNetTime, nchar(spacerRow)), "")
        }
        
        
        body <- file[-c(1:eqIndex, footIndex, blankIndex)]
        
        #Obtain the starting and ending positions of variables
        searchLocs <- findColLocs(spacerRow)
        locCols <- selectCols(varNames, headerRow, searchLocs)
    }
    
    Values <- mapply(substr, list(body), start = locCols[1,], stop = locCols[2,])
    colnames(Values) <- varNames
    
    invisible(Values)
}


mfilenames <- paste("MenTxt/", 1999:2012, ".txt", sep="")
menFiles <- lapply(mfilenames, readLines)
names(menFiles) <- 1999:2012


wfilenames <- paste("WomenTxt/", 1999:2012, ".txt", sep="")
womenFiles <- lapply(wfilenames, readLines)
names(womenFiles) <- 1999:2012


#menResMat <- lapply(menFiles, extractVariables)
#womenResMat <- lapply(womenFiles, extractVariables)
menResMat <- mapply(extractVariables, menFiles, sex = "M", year = 1999:2012)
womenResMat <- mapply(extractVariables, womenFiles, sex = "W", year = 1999:2012)

sapply(menResMat, nrow)
sapply(womenResMat, nrow)


##Check ages
#Men
age <- as.numeric(menResMat$`2012`[, "ag"])
age <- sapply(menResMat, function(x) as.numeric(x[ , "ag"]))
boxplot(age, ylab = "Age", xlab = "Year")

sapply(age, function(x) sum(is.na(x)))
age2001 <- age[["2001"]]
grep("^===", menFiles[["2001"]])

badAgeIndex <- which(is.na(age2001)) + 5
menFiles[["2001"]][badAgeIndex]

blanks <- grep("^[[:blank:]]*$", menFiles[["2001"]])

which(age2001 < 5)
menFiles[["2001"]][which(age2001 < 5) + 5]

charTime <- menResMat[["2012"]][, "time"]
timePieces <- strsplit(charTime, ":")
timePieces[[1]]
tail(timePieces, 1)

timePieces <- sapply(timePieces, as.numeric)
runTime <- sapply(timePieces,
                  function(x){
                      if (length(x) == 2) x[1] + x[2]/60
                      else 60*x[1] + x[2] + x[3]/60
                  })

summary(runTime)

#Women
age <- as.numeric(womenResMat$`2012`[, "ag"])
age <- sapply(womenResMat, function(x) as.numeric(x[ , "ag"]))
boxplot(age, ylab = "Age", xlab = "Year")

sapply(age, function(x) sum(is.na(x)))

age1999 <- age$`1999`

badAgeIndex <- which(is.na(age1999)) + grep("^===", womenFiles[["1999"]])
womenFiles$`1999`[badAgeIndex]

age2002 <- age$`2002`
badAgeIndex <- which(is.na(age2002)) + grep("^===", womenFiles[["2002"]])
womenFiles$`2002`[badAgeIndex]

age2005 <- age$`2005`
badAgeIndex <- which(is.na(age2005)) + grep("^===", womenFiles[["2005"]])
womenFiles$`2005`[badAgeIndex]

age2001 <- age$`2001`
min(age2001)
zeroAgeIndex <- which(age2001 == 0)
womenFiles$`2001`[zeroAgeIndex + 3]

age2009 <- age$`2009`
min(age2009, na.rm = T)
ageSevenIndex <- which(age2009 == 7) + grep("^===", womenFiles[["2009"]])
womenFiles$`2009`[ageSevenIndex]


convertTime <- function(charTime){
    #takes time in h:mm:ss format and converts it to minutes
    #if time is invalid, it forces it to NA
    
    timePieces <- strsplit(charTime, ":")
    timePieces <- sapply(timePieces, as.numeric)
    
    #Fix to account for times that are of incorrect format, e.g. "1:30:" 
    nbrColons <- lapply(charTime, 
                       function(x) {
                         length(gregexpr(":", x)[[1]])
                       })
    
    runTime <- mapply(function(x, y, z){
                  nbrTimePieces <- length(x)
                  if (nbrTimePieces <= y) {
                      return(NA)}
                  else if (nbrTimePieces == 2) {
                      return(x[1] + x[2]/60)}
                  else {
                      return(60*x[1] + x[2] + x[3]/60)}
               }, 
               timePieces, 
               nbrColons,
               charTime)
    
}

createDF <- function(Res, year, sex){
    #Determine which time to use
    useTime <- if(!is.na(Res[1, "net"])) {
        Res[, "net"]
    } else if(!is.na(Res[1, "gun"])) {
        Res[, "gun"]
    } else {
        Res[, "time"]}
    
    #Remove # and * and blanks from time
    useTime <- gsub("[#\\*[:blank:]]", "", useTime)
    
    #Drop rows with no time
    Res <- Res[useTime != "", ]
    
    runTime <- convertTime(useTime[useTime != ""])
    
    #convertTime returns NA for invalid run times; drop these records and print
    #message about record(s) dropped
    if(sum(is.na(runTime)) > 0){
      print(paste("Dropping the following records in year", year, "for", 
                  ifelse(sex == "M", "Men", "Women"), 
                  "due to invalid times", sep = " "))
      
      print(Res[is.na(runTime), ])     
    }

    
    Results <- data.frame(year = rep(year, nrow(Res)),
                          sex = rep(sex, nrow(Res)),
                          name = Res[ , "name"],
                          home = Res[ , "home"],
                          age = as.numeric(Res[ , "ag"]), 
                          runTime = runTime,
                          stringsAsFactors = F)
    
    invisible(Results)
                        
}


menDF <- mapply(createDF, menResMat, year = 1999:2012, sex = "M", SIMPLIFY = F)
womenDF <- mapply(createDF, womenResMat, year = 1999:2012, sex = "W", SIMPLIFY = F)


#check NA values for runTime
sapply(menDF, function(x) sum(is.na(x$runTime)))
sapply(womenDF, function(x) sum(is.na(x$runTime)))

#Check for why there are so many NA's in women's 2006 file for runtime
#It's because hometown spacer row is not separated from net time, just like in men's 2006 file
#We can fix this in the extractVariables function
fileWomen2006 <- womenFiles$`2006`
head(fileWomen2006, 30)

cbMen <- do.call(rbind, menDF)
save(cbMen, file = "cbMen.rda")

cbWomen <- do.call(rbind, womenDF)
save(cbWomen, file = "cbWomen.rda")


