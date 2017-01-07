
setwd("C:/MyGitRepos/cherry-blossom-run/Data")

els <- readLines("MenTxt/2012.txt")
eqIndex <- grep("^===", els)
spacerRow <- els[eqIndex]
headerRow <- els[eqIndex - 1]
body <- els[-(1:eqIndex)]

headerRow <- tolower(headerRow)

# ageStart <- regexpr("ag", headerRow)
# age <- substr(body, start = ageStart, stop = ageStart + 1)

blankLocs <- gregexpr(" ", spacerRow)
searchLocs <- c(0, blankLocs[[1]])

Values <- mapply(substr, list(body), start = searchLocs[-length(searchLocs) + 1], stop = searchLocs[-1] - 1)


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

searchLocs <- findColLocs(spacerRow)
ageLoc <- selectCols("ag", headerRow, searchLocs)
ages <- mapply(substr, list(body), start = ageLoc[1,], stop = ageLoc[2,])

shortColNames <- c("name", "home", "ag", "gun", "net", "time")

locCols <- selectCols(shortColNames, headerRow, searchLocs)
Values <- mapply(substr, list(body), start = locCols[1,], stop = locCols[2,])
class(Values)
colnames(Values) <- shortColNames


extractVariables <- function(file, varNames = c("name", "home", "ag", "gun", "net", "time"),
                             sex, year) {
    #Find the index of the row with equal signs
    eqIndex <- grep("^===", file)
    
    #Find the index of the footer row
    footIndex <- grep("^[[:blank:]]*[#|*]", file)
    
    #Find the index of rows that are completely blank
    blankIndex <- grep("^[[:blank:]]*$", file)
    
    #Extract the two key rows and the data (fix men 2006 spacer row)
    spacerRow <- file[eqIndex]
    
    if (sex == "M" & year == 2006){
        locNetTime <- regexpr("net", headerRow)
        spacerRow <- paste(substr(spacerRow, 1, locNetTime - 2), 
                              substr(spacerRow, locNetTime, nchar(spacerRow)), " ")
    }
    
    
    headerRow <- tolower(file[eqIndex - 1])
    body <- file[-c(1:eqIndex, footIndex, blankIndex)]
    
    #Obtain the starting and ending positions of variables
    searchLocs <- findColLocs(spacerRow)
    locCols <- selectCols(varNames, headerRow, searchLocs)
    
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


##Check ages
age <- as.numeric(menResMat$`2012`[, "ag"])
age <- sapply(menResMat, function(x) as.numeric(x[ , "ag"]))
boxplot(age, ylab = "Age", xlab = "Year")

sapply(age, function(x) sum(is.na(x)))
age2001 <- age[["2001"]]
grep("^===", menFiles[["2001"]])

badAgeIndex = which(is.na(age2001)) + 5
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


convertTime <- function(charTime){
    #takes time in h:mm:ss format and converts it to minutes
    
    timePieces <- strsplit(charTime, ":")
    timePieces <- sapply(timePieces, as.numeric)
    
    runTime <- sapply(timePieces,
                      function(x){
                          if (length(x) == 2) {x[1] + x[2]/60}
                          else {60*x[1] + x[2] + x[3]/60}
                      })
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

#check NA values for runTime
sapply(menDF, function(x) sum(is.na(x$runTime)))

cbMen <- do.call(rbind, menDF)
save(cbMen, file = "cbMen.rda")



