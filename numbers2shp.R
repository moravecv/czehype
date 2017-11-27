#' Creates HYPE numbering for shapefile recognizable by WHIST
#'
#' @param shp path to input shapefile to be numbered according to HYPE structure
#' @param layer name of the layer in shapefile
#' @param match_col column name for matching the numbering
#' @param number_table path to table with upstream downstream connections
#' @param write write shapefile if TRUE
#' @param path_out path where to save the shapefile
#'
#' @return shp with HYPE numbering
#' @export shp with HYPE numbering
#'
#' @examples
#' 
#' 
#' 

numbers2shp <- function(shp, layer, match_col, number_table, write = FALSE, path_out){
  # desc: Creates HYPE numbering for shapefile recognizable by WHIST
  # arg shp: path to input shapefile to be numbered according to HYPE structure
  # arg layer: name of the layer in shapefile
  # arg match_col: column name for matching the numbering
  # arg number_table: path to table with upstream downstream connections
  # arg write: write shapefile if TRUE
  # arg path_out: path where to save the shapefile
  # return: shp with HYPE numbering
  
  library(data.table)
  library(rgdal)
  if (write == TRUE){
    table <- data.table(readRDS(number_table))
    shp <- readOGR(dsn = shp, layer = layer)
    shp@data <- data.frame(shp@data, table[match(shp@data[[match_col]], table[[match_col]]),]) # merge table of shp with number table
    k <- 1
    shp@data$SUBID <- NA_integer_ # create NA column
    shp@data$DOWN <- NA_integer_ # create NA column
    for (i in shp@data$UPOV_ID){ 
      shp@data$SUBID[which(shp@data$UPOV_ID == i)] <- k # UPOV = i >> SUBID = k
      shp@data$DOWN[which(shp@data$TO == i)] <- k # TO = i >> DOWN = k
      k <- k + 1
    }
    #shp@data[is.na(shp@data)] <- -9999
    shp@data$DOWN[which(shp@data$TO == -9999)] <- -9999 # replace NA's with -9999
    writeOGR(shp, path_out, layer, driver="ESRI Shapefile") # write shapefile
  } else {
    table <- data.table(readRDS(number_table))
    shp <- readOGR(dsn = shp, layer = layer)
    shp@data <- data.frame(shp@data, table[match(shp@data[[match_col]], table[[match_col]]), ]) # merge table of shp with number table
    k <- 1
    shp@data$SUBID <- NA_integer_ # create NA column
    shp@data$DOWN <- NA_integer_ # create NA column
    for (i in shp@data$UPOV_ID){
      shp@data$SUBID[which(shp@data$UPOV_ID == i)] <- k # UPOV = i >> SUBID = k
      shp@data$DOWN[which(shp@data$TO == i)] <- k # TO = i >> DOWN = k
      k <- k + 1
    }
    #shp@data[is.na(shp@data)] <- -9999
    shp@data$DOWN[which(shp@data$TO == -9999)] <- -9999 # replace NA's with -9999
    return(shp)
  }
  
}
