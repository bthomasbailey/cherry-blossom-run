

setwd("C:/MyGitRepos/cherry-blossom-run/Data")
require(stringr)

extractSingleItem <- function(strLine, pattern){
    itemVal <- str_match(strLine, pattern)[1, 1]
    len <- nchar(itemVal)
    
    if (!is.na(itemVal)) {
        loc <- regexpr(itemVal, strLine)
        newStrLine <- substr(strLine, start = loc + len, stop = nchar(strLine))   
    } else {
        newStrLine <- strLine
    }
    
    return (c(itemVal, newStrLine))
}

parseLine <- function(strLine){
    
    placeOut <- extractSingleItem(strLine, "\\d+")
    place <- str_trim(placeOut[1])
    newStrLine <- placeOut[2]
    
    divTotOut <- extractSingleItem(newStrLine, "\\d+/\\d+")
    divTot <- str_trim(divTotOut[1])
    newStrLine <- divTotOut[2]
    
    numOut <- extractSingleItem(newStrLine, "\\d+")
    num <- str_trim(numOut[1])
    newStrLine <- numOut[2]
    
    nameOut <- extractSingleItem(newStrLine, "[A-Za-z\\-\\'\\.\\s]+")
    name <- str_trim(nameOut[1])
    newStrLine <- nameOut[2]
    
    agOut <- extractSingleItem(newStrLine, "\\d+")
    ag <- str_trim(agOut[1])
    newStrLine <- agOut[2]
    
    homeTownOut <- extractSingleItem(newStrLine, "([A-Za-z]+[\\s]*)+")
    homeTown <- str_trim(homeTownOut[1])
    newStrLine <- homeTownOut[2]
    
    times <- str_match_all(newStrLine, "\\d+[\\:]{1}\\d+([\\:]{1}\\d+)?[#|*]*")[[1]]
    gunTime <- str_trim(times[1, 1])
    netTime <- str_trim(times[2, 1])
    pace <- str_trim(times[3, 1])
    
    return (c(place, divTot, num, name, ag, homeTown, gunTime, netTime, pace))
    
}



extractResTable <- function(url, year = 1999, men = T, file = NULL) {
    #Retrieve data from web site, find preformatted text, return as chr vector
    
    require(XML)
    require(gdata)
    
    doc <- htmlParse(url)
    
    if (year == 2000) {
        #Get text from 4th font element
        #File is ill-formed so <pre> search doesn't work.
        ff <- getNodeSet(doc, "//font")
        txt <- xmlValue(ff[[4]])
        els <- strsplit(txt, "\r\n")[[1]]
    } else if (year == 2009 & men == T) {
        #The html for this is nooooo fun
        dp <- getNodeSet(doc, "//div//pre")
        nodeVals <- lapply(dp, xmlValue)
        
        eqIndex <- grep("^===", nodeVals)
        
        spacerRow <- nodeVals[eqIndex][[1]]
        vecSpacerRow <- str_split(str_trim(spacerRow), "[\\s|Â]+")[[1]]
        
        #Need to adjust Gun Time and Net Time columns b/c they are too short for some of their values
        vecSpacerRow[7] <- paste(vecSpacerRow[7],"=", sep = "")
        vecSpacerRow[8] <- paste(vecSpacerRow[8],"=", sep = "")
        
        #manually type in headers; I know this isn't ideal, but code is only useful for Men's 2009 results anyway
        vecHeaderRow <- c("Place", "Div/Tot", "Num", "Name", "Ag", "Hometown", "Gun Tim", "Net Tim", "Pace")
        
        bodyNodeVals <- nodeVals[(eqIndex + 1):(length(nodeVals) - 2)]
        
        colLens <- sapply(vecSpacerRow, nchar, USE.NAMES = F)
        bodyMat <- t(sapply(bodyNodeVals, parseLine))
        
        preHeadNodeVals <- nodeVals[(1):(eqIndex - 2)]
        vecPreHeadText <- sapply(preHeadNodeVals, gsub, pattern = "Â", replacement = " ")
        vecPreHeadText <- sapply(vecPreHeadText, str_trim)
        
        footerNodeVals <- nodeVals[(length(nodeVals) - 1):(length(nodeVals))]
        vecFooterText <- sapply(footerNodeVals, gsub, pattern = "Â", replacement = " ")
        #footerMat <- t(sapply(vecFooterText, function(x) c(x, rep(" ", 8)), USE.NAMES = F))
        
        dataMat <- rbind(vecHeaderRow, vecSpacerRow, bodyMat, deparse.level = 0)
        
        #write to Txt file
        lapply(vecPreHeadText, write, "temp2009.txt", append = T)
        write.fwf(dataMat, file = "temp2009.txt", width = colLens, colnames = F, append = T)
        lapply(vecFooterText, write, "temp2009.txt", append = T)
        
        #convert contents of txt file to chr vector
        els <- readLines("temp2009.txt")
        file.remove("temp2009.txt")
    }
    else {
        preNode <- getNodeSet(doc, "//pre")
        txt <- xmlValue(preNode[[1]])
        els <- strsplit(txt, "\r\n")[[1]]
    }
    
    if (men == T) {
        subDir <- "MenTxt"
    } else {
        subDir <- "WomenTxt"
    }
    
    if (!(is.null(file))) {
        if(!(dir.exists(subDir))) {dir.create(subDir)} 
        writeLines(els, file.path(subDir, file))
    }
    
    return(els)
}

ubase <- "http://www.cherryblossom.org/"
years <- 1999:2012

#urls <- paste(ubase, "results/", 1999:2012, "/", 1999:2012, "cucb10m-m.htm", sep = "")
menURLs <- c("cb99m.htm", "cb003m.htm", "results/2001/oof_m.html", 
             "results/2002/oofm.htm", "results/2003/CB03-M.HTM",
             "results/2004/men.htm", "results/2005/CB05-M.htm",
             "results/2006/men.htm", "results/2007/men.htm", 
             "results/2008/men.htm", "results/2009/09cucb-M.htm",
             "results/2010/2010cucb10m-m.htm", 
             "results/2011/2011cucb10m-m.htm",
             "results/2012/2012cucb10m-m.htm")

urls <- paste(ubase, menURLs, sep = "")

#menTables <- lapply(urls, extractResTable)
menTables <- mapply(extractResTable, url = urls, year = years, men = T, 
                    file = paste(years, ".txt", sep = ""))
names(menTables) <- years


womURLS <- c("cb99f.htm", "cb003f.htm", "results/2001/oof_f.html", 
             "results/2002/ooff.htm", "results/2003/CB03-F.HTM",
             "results/2004/women.htm", "results/2005/CB05-F.htm",
             "results/2006/women.htm", "results/2007/women.htm", 
             "results/2008/women.htm", "results/2009/09cucb-F.htm",
             "results/2010/2010cucb10m-f.htm", 
             "results/2011/2011cucb10m-f.htm",
             "results/2012/2012cucb10m-f.htm")

urls <- paste(ubase, womURLS, sep = "")
womTables <- mapply(extractResTable, url = urls, year = years, men = F,
                    file = paste(years, ".txt", sep = ""))
names(womTables) <- years


sapply(menTables, length)
sapply(womTables, length)


save(menTables, file = "CBMenTextTables.Rda")
save(womTables, file = "CBWomenTextTables.Rda")

