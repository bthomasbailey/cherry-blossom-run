###Exercise #1: Redo reading in txt files, but do so with read.fwf

setwd("C:/MyGitRepos/cherry-blossom-run/Data")


trimBlanks <- function(charVector){
  nameClean <- gsub("^[[:blank:]]+", "", charVector)
  nameClean <- gsub("[[:blank:]]+$", "", nameClean)
  nameClean <- gsub("[[:blank:]]+", " ", nameClean)
}

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

read_txt_fwf <- function(M = T, yr, widths, 
                         colNames = c("name", "age", "home", "gun", "net"), 
                         skipRows){
  
  dirName <- ifelse(M, "MenTxt", "WomenTxt")
  file <- file.path(dirName, paste0(yr, ".txt")) 
  
  df <- read.fwf(file, widths = widths, col.names = colNames, 
                 skip = skipRows, comment.char = "")
  
  #add column to keep track of year
  df$year <- yr
  
  #keep only one time column - first look for net, then gun, then time
  useTime <- if("net" %in% colNames) {
    "net"
  } else if("gun" %in% colNames) {
    "gun"
  } else {
    "time"}
  
  df <- df[, c("year", "name", "age", "home", useTime)]
  names(df)[names(df) == "net"] <- "time"
  
  df$name <- tolower(gsub("[,.]", "", trimBlanks(as.character(df$name))))
  df$age <- as.integer(df$age)
  df$home <- tolower(gsub("[,.`]", "", trimBlanks(as.character(df$home))))

  #Remove # and * and blanks from time, drop records w/o time, and then convert to minutes
  df$time <- gsub("[#\\*[:blank:]]", "", as.character(df$time))
  df <- df[df$time != "", ]
  df$time <- convertTime(df$time)
  
  return(df)
  
}


menWidths <- list(c(-16, 22, 3, 19, 8),
                  c(-22, 22, 3, 19, 9, 8),
                  c(-12, 22, 3, 19, 8, 8),
                  c(-12, 22, 3, 19, 8, 8),               
                  c(-22, 30, 3, 20, 9, 8),
                  c(-22, 30, 3, 20, 8, 8),
                  c(-16, 23, 3, 19, 8, 8), 
                  c(-22, 23, 3, 24, 9, 6), 
                  c(-25, 23, 3, 19, 9), 
                  c(-25, 23, 3, 19, -28, 8),
                  c(-25, 23, 3, 21, 9, 9), 
                  c(-25, 23, 3, 21, -8, 8, 9), 
                  c(-25, 23, 3, 21, -8, 8, 8), 
                  c(-25, 23, 3, 21, -8, 8)
                )

menColNames <- list(c("name", "age", "home", "time"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "time"),
                    c("name", "age", "home", "time"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "time")
)


menSkipRows <- list(3, 2, 5, 3, 3, 8, 8, 9, 7, 8, 8, 8, 8, 8)

names(menWidths) <- 1999:2012
names(menColNames) <- 1999:2012
names(menSkipRows) <- 1999:2012


womenWidths <- list(c(-16, 22, 3, 19, 8),
                    c(-22, 22, 3, 19, 9, 8),
                    c(-12, 22, 3, 19, 8, 8),
                    c(-12, 22, 3, 19, 8, 8),               
                    c(-22, 30, 3, 20, 9, 8),
                    c(-22, 30, 3, 20, 8, 8),
                    c(-16, 23, 3, 19, 8, 8), 
                    c(-22, 23, 3, 24, 9, 6), 
                    c(-25, 23, 3, 19, 9), 
                    c(-25, 23, 3, 19, -28, 8),
                    c(-25, 23, 3, 21, 8, 9), 
                    c(-25, 23, 3, 21, -8, 8, 9), 
                    c(-25, 23, 3, 21, -8, 8, 8), 
                    c(-25, 23, 3, 21, -8, 8)
)


womenColNames <- list(c("name", "age", "home", "time"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "net", "gun"),
                    c("name", "age", "home", "time"),
                    c("name", "age", "home", "time"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "gun", "net"),
                    c("name", "age", "home", "time")
)


womenSkipRows <- list(3, 2, 5, 3, 3, 8, 8, 9, 7, 8, 8, 8, 8, 8)

names(womenWidths) <- 1999:2012
names(womenColNames) <- 1999:2012
names(womenSkipRows) <- 1999:2012



#must use SIMPLIFY = F to return list of dataframes; 
#if not, it will try to reduce the result to a matrix
menDF_fwf <- mapply(read_txt_fwf, M = T, yr = 1999:2012, widths = menWidths,
                colNames = menColNames, skipRows = menSkipRows,
                SIMPLIFY = F)

womenDF_fwf <- mapply(read_txt_fwf, M = F, yr = 1999:2012, widths = womenWidths,
                      colNames = womenColNames, skipRows = womenSkipRows,
                      SIMPLIFY = F)


#combine each DF in list into one big DF
cbMen_fwf <- do.call(rbind, menDF_fwf)
cbWomen_fwf <- do.call(rbind, womenDF_fwf)

#save DFs
save(cbMen_fwf, file = "cbMen_fwf.rda")
save(cbWomen_fwf, file = "cbWomen_fwf.rda")





