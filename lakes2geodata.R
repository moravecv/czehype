#' Replaces fractions of ilake and olake areas and compensates sizes of 
#' other fractions while protecting urban areas
#'
#' @param geodata path to the GeoData.txt file
#' @param olake_area path to two column file of olake area
#' @param ilake_area path to two column file of ilake area
#' @param write write GeoData.txt if TRUE
#' @param path_out path where to save the GeoData.txt file
#'
#' @return data.frame of fixed GeoData
#' @export text_file of GeoData
#'
#' @examples 
#' lakes2geodata(geodata = "E:/Data/GeoData_V01.txt", olake_area = "E:/Data/OLAKE_AREA.rds",
#' ilake_area = "E:/Data/ILAKE_AREA.rds", write = T, path_out = "E:/Data/Geodata_fixed.txt")
#' 

lakes2geodata <- function(geodata, olake_area, ilake_area, write, path_out){
  # desc: Replaces fractions of ilake and olake areas and compensates sizes of 
  # other fractions while protecting urban areas
  # arg geodata: path to the GeoData.txt file
  # arg olake_area: path to two column file of olake area
  # arg ilake_area: path to two column file of ilake area
  # arg write: write GeoData.txt if TRUE
  # arg path_out: path where to save the GeoData.txt file
  # return: data.frame of fixed GeoData
  # export: text_file of GeoData
  
  library(HYPEtools)
  message("Reading input data ...")
  gd <- ReadGeoData(filename = geodata)
  olake_area <- readRDS(olake_area)
  colnames(olake_area) <- c("UPOV_ID", "OLAKE_AREA")
  ilake_area <- readRDS(ilake_area)
  colnames(ilake_area) <- c("UPOV_ID", "ILAKE_AREA")
  gd$SLC_2050 <- 0 # SLC OLAKE set to zero
  gd$SLC_2051 <- 0 # SLC ILAKE set to zero
  gd2 <- merge(as.data.frame(gd), as.data.frame(olake_area),
               by = c("UPOV_ID", "UPOV_ID"), all = T) # join olake table to geodata
  gd3 <- merge(as.data.frame(gd2), as.data.frame(ilake_area), 
               by = c("UPOV_ID", "UPOV_ID"), all = T) # join ilake table to geodata
  col_pos <- grep("SLC_", colnames(gd3)) # SLC column indexes
  col_names <- colnames(gd3)[grep("SLC_", colnames(gd3))] # SLC column names
  wbodies <- grep("SLC_21", colnames(gd3)) # waterbodies SLC indexes
  wbodies_names <- colnames(gd3)[grep("SLC_21", colnames(gd3))] # waterbodies colnames
  gd3[,wbodies] <- 0 # set zero to waterbodies fractions 
  gd3$OLAKE_AREA[is.na(gd3$OLAKE_AREA)] <- 0 # set zero where is NA
  gd3$ILAKE_AREA[is.na(gd3$ILAKE_AREA)] <- 0 # set zero where is NA
  gd3$SLC_2050 <- gd3$OLAKE_AREA / gd3$AREA # compute olake fraction
  gd3$SLC_2051 <- gd3$ILAKE_AREA / gd3$AREA # compute ilake fraction
  gd3$DIFF <- apply(gd3[,col_pos], 1, function(x) {sum(x) - 1}) # compute diff from 1
  pattern <- c("SLC_10", "SLC_21", "SLC_2050", "SLC_2051") # flag urban and water areas
  urb_pos <- grep(pattern = paste(pattern, collapse = "|"), x = colnames(gd3),invert = T)
  urb_names <- grep("SLC_", colnames(gd3)[urb_pos], value = T) # colnames except urban&water
  urb_pos2 <- which(colnames(gd3) %in% urb_names) # SLC column indexes except urban&water
  n <- nrow(gd3)
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (i in 1:nrow(gd3)){
    #gd3[i,col_pos] debug line
    add.diff <- which(colnames(gd3) == "DIFF") # column index of the diff value
    diff <- gd3[i, add.diff] # diff value
    p_vals <- which(gd3[i,urb_pos2] != 0) # indexes of values to be fixed
    n <- length(p_vals) # number of values to be fixed
    diff_spread <- diff / n # fraction of difference to be added to each value
    if (length(which(gd3[i,urb_pos2][p_vals] < diff_spread)) == 0){ 
      # if there is no value lower than the fraction itself then subtract the diff fraction 
      gd3[i,urb_pos2][p_vals] <- gd3[i,urb_pos2][p_vals] - diff_spread
    } else {
      # else sum all the values lower than the fraction and subtract it from the total diff
      small <- sum(gd3[i,urb_pos2][p_vals][which(gd3[i,urb_pos2][p_vals] < diff_spread)])
      diff_fix <- diff - small # new diff value
      # set all the values lover than the fraction of diff to zero
      gd3[i,urb_pos2][p_vals][which(gd3[i,urb_pos2][p_vals] < diff_spread)] <- 0
      p_vals_fix <- which(gd3[i,urb_pos2] != 0) # new indexes of values to be fixed
      n_fix <- length(p_vals_fix) # new number of values to be fixed
      diff_spread_fix <- diff_fix / n_fix # new fraction of difference 
      gd3[i,urb_pos2][p_vals_fix] <- gd3[i,urb_pos2][p_vals_fix] - diff_spread_fix
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  if (write == TRUE){
    message("Writing GeoData to .txt ...")
    WriteGeoData(x = gd3, filename = path_out)
  } else {
    return(gd3)
  }
}