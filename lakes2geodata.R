#' Replaces fractions of ilake and olake areas and compensates sizes of 
#' other fractions while protecting urban areas
#'
#' @param geodata path to the GeoData.txt file
#' @param olake_area path to two column (RDS) file of olake area
#' @param ilake_area path to two column (RDS) file of ilake area
#' @param match_col name of matching column (character)
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

lakes2geodata <- function(geodata, olake_area, ilake_area, match_col, write, path_out){
  # desc: Replaces fractions of ilake and olake areas and compensates sizes of 
  # other fractions while protecting urban areas
  # arg geodata: path to the GeoData.txt file
  # arg olake_area: path to two column file (RDS) of olake area
  # arg ilake_area: path to two column file (RDS) of ilake area
  # arg match_col: name of matching column (character)
  # arg write: write GeoData.txt if TRUE
  # arg path_out: path where to save the GeoData.txt file
  # return: data.frame of fixed GeoData
  # export: text_file of GeoData
  
  library(HYPEtools)
  message("Reading input data ...")
  gd <- ReadGeoData(filename = geodata)
  olake_area <- readRDS(olake_area)
  colnames(olake_area) <- c(match_col, "OLAKE_AREA")
  ilake_area <- readRDS(ilake_area)
  colnames(ilake_area) <- c(match_col, "ILAKE_AREA")
  gd$SLC_5050 <- 0 # SLC OLAKE set to zero
  gd$SLC_5151 <- 0 # SLC ILAKE set to zero
  gd2 <- merge(as.data.frame(gd), as.data.frame(olake_area),
               by = c(match_col, match_col), all = T) # join olake table to geodata
  gd3 <- merge(as.data.frame(gd2), as.data.frame(ilake_area), 
               by = c(match_col, match_col), all = T) # join ilake table to geodata
  col_pos <- grep("SLC_", colnames(gd3)) # SLC column indexes
  col_names <- colnames(gd3)[grep("SLC_", colnames(gd3))] # SLC column names
  wbodies <- grep("SLC_21", colnames(gd3)) # waterbodies SLC indexes
  wbodies_names <- colnames(gd3)[grep("SLC_21", colnames(gd3))] # waterbodies colnames
  gd3[,wbodies] <- 0 # set zero to waterbodies fractions 
  gd3$OLAKE_AREA[is.na(gd3$OLAKE_AREA)] <- 0 # set zero where is NA
  gd3$ILAKE_AREA[is.na(gd3$ILAKE_AREA)] <- 0 # set zero where is NA
  gd3$SLC_5050 <- gd3$OLAKE_AREA / gd3$AREA # compute olake fraction
  gd3$SLC_5151 <- gd3$ILAKE_AREA / gd3$AREA # compute ilake fraction
  gd3$DIFF <- apply(gd3[,col_pos], 1, function(x) {sum(x) - 1}) # compute diff from 1
  pattern <- c("SLC_10", "SLC_21", "SLC_5050", "SLC_5151") # flag urban and water areas
  urb_pos <- grep(pattern = paste(pattern, collapse = "|"), x = colnames(gd3),invert = T)
  urb_names <- grep("SLC_", colnames(gd3)[urb_pos], value = T) # colnames except urban&water
  urb_pos2 <- which(colnames(gd3) %in% urb_names) # SLC column indexes except urban&water
  message("Recalculating SLC fractions ...")
  n <- nrow(gd3)
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (i in 1:nrow(gd3)){
    add.diff <- which(colnames(gd3) == "DIFF") # column index of the diff value
    diff <- gd3[i, add.diff] # diff value
    if (diff == 0){
      next
    } else {
      p_vals <- which(gd3[i,urb_pos2] != 0) # indexes of values to be fixed
      if (length(p_vals) == 0){
        next
      } else {
        vec <- gd3[i,urb_pos2][p_vals] # create vector of values to be fixed
        inverse <- 1 - vec # vector of add-on values to 1
        scale <- inverse / sum(inverse) # scale values to get percentage for diff spread
        vec2 <- vec - (scale * diff) # vector of fixed values
        if (any(vec2 < 0)){
          # if any value in fixed vector is lower than 0 proceed with while cycle
          while (any(vec2 < 0)) {
            # while any value in fixed vector is lower than 0 repeat cycle
            deduct <- sum(vec[which(vec2 < 0)]) 
            # sum all the values in vector "vec" which end up being negative in vector "vec2"
            diff <- diff - deduct # deduct the sum of potential negative value from the diff 
            vec2[vec2 < 0] <- 0 # set to zero the values which are negative
            p_vals2 <- which(vec2 != 0) # indexes of values to be fixed
            inverse2 <- 1 - vec2[p_vals2] # vector of add-on values to 1
            scale2 <- inverse2 / sum(inverse2) # scale values to get percentage for diff spread
            vec2[p_vals2] <- vec2[p_vals2] - (scale2 * diff) # vector of fixed values
          }
          gd3[i,urb_pos2][p_vals] <- vec2 # write vector of fixed values in the gd dataframe
          check <- sum(gd3[i,col_pos]) # check if the sum of slcs is equal to 1
          if (check < 1){
            # if check value is lower than 1 then add back to the latest fixed values  
            # to get sum slcs == 1
            fix <- 1 - check # calculate fix value 
            vec2[p_vals2] <- vec2[p_vals2] + (scale2 * fix) # spread it among the latest values
            gd3[i,urb_pos2][p_vals] <- vec2 # write vector of fixed values in the gd dataframe
          } else {
            next
          }
        } else {
          gd3[i,urb_pos2][p_vals] <- vec2 # write vector of fixed values in the gd dataframe
        }
        setTxtProgressBar(pb, i)
      }
      }
      
  }
  close(pb)
  gd3 <- SortGeoData(gd3) # sort new geodata
  if (write == TRUE){
    message("Writing GeoData to .txt ...")
    WriteGeoData(x = gd3, filename = path_out)
  } else {
    return(gd3)
  }
}
