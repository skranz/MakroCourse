
read.sheet = function (wb, sheetIndex, sheetName = NULL, rowIndex = NULL, 
    startRow = NULL, endRow = NULL, colIndex = NULL, as.data.frame = TRUE, 
    header = TRUE, colClasses = NA, keepFormulas = FALSE, encoding = "unknown", 
    ...) 
{

    sheets <- getSheets(wb)
    sheet <- if (is.null(sheetName)) {
        sheets[[sheetIndex]]
    }
    else {
        sheets[[sheetName]]
    }
    if (is.null(sheet)) 
        stop("Cannot find the sheet you requested in the file!")
    rowIndex <- if (is.null(rowIndex)) {
        if (is.null(startRow)) 
            startRow <- .jcall(sheet, "I", "getFirstRowNum") + 
                1
        if (is.null(endRow)) 
            endRow <- .jcall(sheet, "I", "getLastRowNum") + 1
        startRow:endRow
    }
    else rowIndex
    rows <- getRows(sheet, rowIndex)
    if (length(rows) == 0) 
        return(NULL)
    cells <- getCells(rows, colIndex)
    res <- lapply(cells, getCellValue, keepFormulas = keepFormulas, 
        encoding = encoding)
    if (as.data.frame) {
        ind <- lapply(strsplit(names(res), "\\."), as.numeric)
        namesIndM <- do.call(rbind, ind)
        row.names <- sort(as.numeric(unique(namesIndM[, 1])))
        col.names <- paste("V", sort(unique(namesIndM[, 2])), 
            sep = "")
        col.names <- sort(unique(namesIndM[, 2]))
        cols <- length(col.names)
        VV <- matrix(list(NA), nrow = length(row.names), ncol = cols, 
            dimnames = list(row.names, col.names))
        indM <- apply(namesIndM, 2, function(x) {
            as.numeric(as.factor(x))
        })
        VV[indM] <- res
        if (header) {
            colnames(VV) <- VV[1, ]
            VV <- VV[-1, , drop = FALSE]
        }
        res <- vector("list", length = cols)
        names(res) <- colnames(VV)
        for (ic in seq_len(cols)) {
            aux <- unlist(VV[, ic], use.names = FALSE)
            nonNA <- which(!is.na(aux))
            if (length(nonNA) > 0) {
                ind <- min(nonNA)
                if (class(aux[ind]) == "numeric") {
                  dateUtil <- .jnew("org/apache/poi/ss/usermodel/DateUtil")
                  cell <- cells[[paste(row.names[ind + header], 
                    ".", col.names[ic], sep = "")]]
                  isDatetime <- dateUtil$isCellDateFormatted(cell)
                  if (isDatetime) {
                    if (identical(aux, round(aux))) {
                      aux <- as.Date(aux - 25569, origin = "1970-01-01")
                    }
                    else {
                      aux <- as.POSIXct((aux - 25569) * 86400, 
                        tz = "GMT", origin = "1970-01-01")
                    }
                  }
                }
            }
            if (!is.na(colClasses[ic])) 
                suppressWarnings(class(aux) <- colClasses[ic])
            res[[ic]] <- aux
        }
        res <- data.frame(res, ...)
    }
    res
}

import.wgi = function() {
  library(xlsx)
  setwd("D:/lehre/makro")
  
  file = "wgidataset.xlsx"
  wb <- loadWorkbook(file)
  sheets = getSheets(wb)
  
  sheetInd = 2
  
  li = lapply(3:7, function(sheetInd) {
    sheetName = names(sheets)[sheetInd]
  
    res <- read.sheet(wb,sheetInd, startRow=15)
    d = res
    library(reshape2)
    library(stringtools)
    library(dplyr)
    colnames(d)[1] = "country"
    dm = melt(d, id.vars=1:2)
    dm$variable = as.character(dm$variable)
    ind = as.numeric(str.right.of(as.character(dm$variable),"."))
    ind[is.na(ind)] = 0
    
    unique(dm$variable)
    unique(ind)
    dm$year = 1996+ind
    dm$stat = str.left.of(dm$variable,".")
    dm$var = sheetName
    
    dat = select(dm, country,WBCode, year,  var, stat, value)
    dat = arrange(dat,country, year, stat,var)
    write.csv(dat, paste0("wgi ",sheetName,".csv"), row.names=FALSE)
    dat    
  })

  li = lapply(3:7, function(sheetInd) {
    sheetName = names(sheets)[sheetInd]
    read.csv(paste0("wgi ",sheetName,".csv"))
  })
  library(data.table)
  
  dt = rbindlist(li)
  dt$var = gsub(" ","",dt$var, fixed=TRUE)
  write.csv(dat,"wgiall.csv", row.names=FALSE)
  
  d = filter(dt, stat=="Estimate") %>%
      select(-stat)
  library(tidyr)
  dw = spread(d, key="var", value="value")

  write.csv(dw,"wgi.csv", row.names=FALSE)
