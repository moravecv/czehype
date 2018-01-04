#' Replaces length of main river (RIVLEN) and local river (LOC_RIVLEN) in GeoData
#'
#' @param geodata path to the GeoData.txt file
#' @param rivlen path to three column (RDS) file of river length
#' @details !!! 1st col: merging ID, 2nd col: main river, 3rd col: loc river !!!
#' @param write write GeoData.txt if TRUE
#' @param path_out path where to save the GeoData.txt file
#'
#' @return data.frame of fixed GeoData
#' @export text_file of GeoData
#'
#' @examples 
#' rivers2geodata(geodata = "E:/Data/GeoData_V01.txt", rivlen = E:/Data/OLAKE_AREA.rds",
#' write T, path_out = "E:/Data/Geodata_fixed.txt")
#' 

rivers2geodata = function(geodata, rivlen, write, path_out){
  # desc: Replaces length of main river (RIVLEN) and local river (LOC_RIVLEN) in GeoData
  # arg riverlen: path to three column (RDS) file of river length
  # details: !!! 1st col: merging ID, 2nd col: main river, 3rd col: loc river !!!
  # arg write: write GeoData.txt if TRUE
  # arg path_out: path where to save the GeoData.txt file
  # return: data.frame of fixed GeoData
  # export: text_file of GeoData
  
  library(HYPEtools)
  gd <- ReadGeoData(filename = geodata)
  rivlen <- readRDS(rivlen)
  col_names <- colnames(rivlen)
  gd2 <- merge(as.data.frame(gd), as.data.frame(rivlen), 
               by = c("UPOV_ID", "UPOV_ID"), all = T) # join rivlen table to geodata
  gd2$RIVLEN <- NULL # delete old column of rivlen from geodata
  colnames(gd2)[which(colnames(gd2) == col_names[2])] <- "RIVLEN" # rename merged col
  colnames(gd2)[which(colnames(gd2) == col_names[3])] <- "LOC_RIVLEN" # rename merged col
  gd3 <- SortGeoData(gd2) # sort new geodata
  if (write == TRUE){
    message("Writing GeoData to .txt ...")
    WriteGeoData(x = gd3, filename = path_out)
  } else {
    return(gd3)
  }
}
