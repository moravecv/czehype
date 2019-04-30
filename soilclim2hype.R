#' Converts data from Soilclim datasets into HYPE structure  
#'
#' @param basins vector of basin names
#' @param variables desired variables 
#' @param path_soilclim path of the folder where .rds of Soilclim is stored
#' @param write if data.table should be written into file
#' @param xobs if variable belongs to xobs class
#' @param varhype name of the xobs variable
#' @param path_out the path and name of the output file
#'
#' @return data.table with desired variables
#' @export text_file 
#'
#' soilclim2hype(basins = c("BER_3030.rds", "HVL_3030.rds"), variables = "Rain",
#' path_soilclim = "E:/SOILCLIM/", write = T, xobs = T, varhype = "rswe",
#' path_out = "E:/SOLCLIM_OUT/Xobs.txt")
#'

soilclim2hype <- function(basins, variables, path_soilclim, write = FALSE,
                          xobs = FALSE, varhype, path_out){
  # desc: Converts data from Soilclim datasets into HYPE structure
  # arg basins: vector of basin names
  # arg variables: desired variables
  # arg path_soilclim: path of the folder where .rds of Soilclim is stored
  # arg write: if data.table should be written into file
  # arg xobs if variable belongs to xobs class
  # arg varhype: name of the xobs variable
  # arg path_out: the path and name of the output file
  # return: data.table with desired variables
  
  library(data.table)
  library(HYPEtools)
  library(stringr)
  if (write == TRUE & xobs == FALSE){ 
    c <- list() # empty list
    n <- length(basins)
    pb <- txtProgressBar(min = 0, max = n, style = 3)
    for (i in 1:n){
      a <- data.table(readRDS(file = paste0(path_soilclim, basins[i]))) # read rds
      b <- a[, c("UPOV_ID","DTM", variables), with = FALSE] # extract desired columns
      b[,DTM:= as.Date(DTM)]
      c[[i]] <- b # add to the list
      setTxtProgressBar(pb, i)
    }
    close(pb)
    c1 <- do.call(rbind, c)
    message("Executing dcast ...")
    d <- dcast(data = c1, formula = DTM ~ UPOV_ID, value.var = variables) # dcast it into hype structure
    WritePTQobs(x = d,filename = path_out, obsid = colnames(d)[2:ncol(d)]) # write PTQobs file
    
  } else if (write == TRUE & xobs == TRUE) {
    c <- list() # empty list
    n <- length(basins)
    pb <- txtProgressBar(min = 0, max = n, style = 3)
    for (i in 1:n){
      a <- data.table(readRDS(file = paste0(path_soilclim, basins[i]))) # read rds
      b <- a[, c("UPOV_ID","DTM", variables), with = FALSE] # extract desired columns
      b[,DTM:= as.Date(DTM)]
      c[[i]] <- b # add to the list
      setTxtProgressBar(pb, i)
    }
    close(pb)
    c1 <- do.call(rbind, c)
    message("Executing dcast ...")
    d <- dcast(data = c1, formula = DTM ~ UPOV_ID, value.var = variables) # dcast it into hype structure
    if (length(variables) > 1){
      colname <- colnames(d)[2:length(colnames(d))]
      for (i in variables){
        colname <- str_remove(string = colname, pattern = paste0(i, "_"))
      }
      WriteXobs(x = d, filename = path_out, comment = paste(variables, collapse=", "),
                variable = rep(varhype, each = length(colnames(d)[2:ncol(d)]) / length(varhype)),
                subid = colname)
      } else {
        WriteXobs(x = d, filename = path_out, comment = variables,
                  variable = rep(varhype, length(colnames(d)[2:ncol(d)])),
                  subid = colnames(d)[2:length(d)])
    }
    } else {
    c <- list() # empty list
    n <- length(basins)
    pb <- txtProgressBar(min = 0, max = n, style = 3)
    for (i in 1:n){
      #print(basins[i]) # debug line
      a <- data.table(readRDS(file = paste0(path_soilclim, basins[i]))) # read rds
      b <- a[, c("UPOV_ID","DTM", variables), with = FALSE] # extract desired columns
      b[,DTM:= as.Date(DTM)]
      c[[i]] <- b # add to the list
      setTxtProgressBar(pb, i)
    }
    close(pb)
    c1 <- do.call(rbind, c)
    message("Executing dcast ...")
    d <- dcast(data = c1, formula = DTM ~ UPOV_ID, value.var = variables) # dcast it into hype structure
    return(d) #return data.table
  }
}
