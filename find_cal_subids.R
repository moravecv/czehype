#' Finds all dependent upstream SUBID numbers for partial calibration when 
#' list of SUBID supplied
#'
#' @param list list of SUBID numbers 
#' @param gd corresponding GeoData file
#' @param write if TRUE writes vector of all dependent SUBID numbers to the file
#' @param path_out path where to save the text file with SUBID numbers
#'
#' @return vector of all dependent upstream SUBID numbers for partial calibration
#' @export text_file with all  dependent upstream SUBID numbers
#'
#' @examples find_cal_subids(list = list$V1, gd = gd, write = T, path_out = "D:/partial.txt")

find_cal_subids <- function(list, gd, write, path_out){
  # desc: #' Finds all dependent upstream SUBID numbers for partial calibration when 
  # list of SUBID supplied
  # arg list: list of SUBID numbers 
  # arg gd: corresponding GeoData file
  # arg write: if TRUE writes vector of all dependent SUBID numbers to the file
  # arg path_out: path where to save the text file with SUBID numbers
  # return: vector of all dependent upstream SUBID numbers for partial calibration
  
  library(HYPEtools)
  all_subids <- c()
  for (i in list){
    subids <- AllUpstreamSubids(subid = i, gd = gd, sort = T)
    all_subids <- append(all_subids, subids)
  }
  all_subids <- unique(all_subids)
  if (write == TRUE){
    write.table(x = t(all_subids), file = path_out, quote = F, sep = " ",
                row.names = FALSE, col.names = FALSE)
  } else {
    return(all_subids)
  }
}



