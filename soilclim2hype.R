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
#' soilclim2hype(basins = c("BER_3030", "HVL_3030"), variables = "Rain",
#' path_soilclim = "E:/SOILCLIM/", write = T, xobs = T, varhype = "rswe",
#' path_out = "E:/SOLCLIM_OUT/")
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
  if (write == TRUE & xobs == FALSE){ 
    c <- data.table() # empty data table
    for (i in basins){
      a <- data.table(readRDS(paste0(path, i))) # read rds
      b <- a[, c("UPOV_ID","DTM", variables), with = FALSE] # extract desired columns
      c <- rbind(c,b) # join them together
    }
    d <- dcast(data = c, formula = DTM ~ UPOV_ID, value.var = variables) # dcast it into hype structure
    WritePTQobs(x = d,filename = path_out, obsid = colnames(d)[2:ncol(d)]) # write PTQobs file
    
  } else if (write == TRUE & xobs == TRUE) {
    c <- data.table() # empty data table
    for (i in basins){
      a <- data.table(readRDS(paste0(path, i))) # read rds
      b <- a[, c("UPOV_ID","DTM", variables), with = FALSE] # extract desired columns
      c <- rbind(c,b) # join them together
    }
    d <- dcast(data = c, formula = DTM ~ UPOV_ID, value.var = variables) # dcast it into hype structure
    WriteXobs(x = d,filename = path_out, comment = variables,
              variable = rep(varhype, length(colnames(d)[2:ncol(d)])),
              subid = colnames(d)[2:length(d)])
    
  } else {
    c <- data.table() # empty data table
    for (i in basins){
      a <- data.table(readRDS(paste0(path, i))) # read rds
      b <- a[, c("UPOV_ID","DTM", variables), with = FALSE] # extract desired columns
      c <- rbind(c,b) # join them together
    }
    d <- dcast(data = c, formula = DTM ~ UPOV_ID, value.var = variables) # dcast it into hype structure
    return(d) #return data.table
  }
}
