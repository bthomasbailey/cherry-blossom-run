

setwd("C:/Users/Tom Bailey/Documents/Training/Data_Science_in_R/Ch2_CherryBlossomRace/Data")

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
               c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
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
