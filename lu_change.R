#' Moves fraction of specified landuse to another landuse class 
#' (for simulating landuse change)
#' @param from - which landuse will be fraction taken 
#' @param to - which landuse will be fraction moved
#' @param fraction of landuse to be moved (0 - 1)
#' @param subid vector of subids where landuse will be moved
#' @param gd data.frame of GeoData
#' @param gc data.frame of GeoClass
#'
#' @return data.frame GeoData with moved fractions 
#'
#' @examples 
#' gdnew = lu_change(from = 2, to = 5, fraction = 0.3, subid = c(1,9,15), gd = gd, gc = gc)

lu_change <- function(from, to, fraction, subid, gd, gc){
  # desc: Moves fraction of specified landuse to another landuse class 
  # (for simulating landuse change)
  # arg from: - which landuse will be fraction taken 
  # arg to: - which landuse will be fraction moved
  # arg fraction: of landuse to be moved (0 - 1)
  # arg subid: vector of subids where landuse will be moved
  # arg gd: data.frame of GeoData
  # arg gc: data.frame of GeoClass
  # return: data.frame GeoData with moved fractions 
  
  library(HYPEtools)
  nofrac <- c() # empty ector for subids with no fraction to move
  n <- length(subid)
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  for (j in 1:n){
    #print(j) # debug line
    gdcols.slc <- which(toupper(substr(names(gd), 1, 3)) == "SLC") # indexes of SLC columns
    slc <- gd[which(gd$SUBID == subid[j]), gdcols.slc]
    #slc <- gd[which(gd$SUBID == j), gdcols.slc] # SCL columns for specified subid
    original <- slc # save original of SLC row for specified subid
    
    if(fraction == 1) {
      # if fraction == 1 all values will be moved
      fixing <- slc[,gc[which(gc[,2] == from),1]] # SLC's of landuse to be moved
      pointers <- which(fixing != 0) # indexes of values to be modified
      fixing[,pointers] <- 0 # zero all positive values
      slc[,gc[which(gc[,2] == from),1]] <- fixing 
      # write vetor of fixed values into SLC row of specified subid
    } else {
      val_change <- fraction * sum(slc[,gc[which(gc[,2] == from),1]])
      # total fraction to be moved from separate SLC's
      if (val_change == 0) {
        nofrac <- append(nofrac,j) # add subid which has no fraction to move in vector
      } else {
        fixing <- slc[,gc[which(gc[,2] == from),1]] # SLC's of landuse to be moved
        pointers <- which(fixing != 0) # indexes of values to be modified
        inverse <- 1 - fixing[,pointers] # addition to 1 of values to be fixed
        if (length(inverse) == 1 && inverse == 0){
          vec <- fixing[,pointers] - val_change
          slc[,gc[which(gc[,2] == from),1]][pointers] <- vec
        } else {
          scale <- inverse / sum(inverse) # scale of values to be fixed
          vec <- fixing[,pointers] - (scale * val_change) # vector of values after deduction
          if (any(vec < 0)){
            # if any value in fixed vector is lower than 0 proceed with while cycle
            while (any(vec < 0)){
              # while any value in fixed vector is lower than 0 repeat cycle
              minus <- vec[which(vec < 0)] # value(s) lower than 0
              deduct <- sum(minus) # sum of value(s) lower than 0
              plus <- vec[,vec > 0] # value(s) higher than 0
              vec[which(vec < 0)] <- 0 # set to zero the values which are negative
              inverse2 <- 1 - plus # vector of add-on values to 1
              scale2 <- inverse2 / sum(inverse2) # scale of values to be fixed
              vec[,vec > 0] <- vec[,vec > 0] - (scale2 * abs(deduct)) # vector of fixed values
            }
            slc[,gc[which(gc[,2] == from),1]][pointers] <- vec
            # write vetor of fixed values into SLC row of specified subid
          } else { 
            slc[,gc[which(gc[,2] == from),1]][pointers] <- vec }
        }}}
    
    indexes <- which(!original %in% slc) # indexes of modified values
    for (i in indexes){
      orig <- original[i] # original value
      new <- slc[i] # new value
      diff <- orig - new # difference between those two values
      texture <- gc[i,][,3] # texture of given SLC
      depth <- gc[i,][,13] # depth of given SLC
      text.fit <- gc[which(gc[,2] == to),1][which(gc[gc[which(gc[,2] == to),1],][,3] == texture)]
      # indexes of SLC's with the same texture as the one which was moved
      depth.fit <- gc[which(gc[,2] == to),1][which(gc[gc[which(gc[,2] == to),1],][,13] == depth)]
      # indexes of SLC's with the same depth as the one which was moved
      depth.fit2 <- depth.fit[depth.fit %in% text.fit]
      # intersect of the texture and depth
      if (length(depth.fit2) == 0){
        # if there was no suitable combination found, spread the fraction among other SLC's with 
        # the same structure i.e. among various depth
        slc[depth.fit] <- slc[depth.fit] + (unlist(diff) / length(slc[depth.fit]))
        # recalculate values that are increased by fraction deducted from SLC 
      } else {
        slc[depth.fit2] <- slc[depth.fit2] + unlist(diff)
        # recalculate value that is increased by fraction deducted from SLC 
      }
    }
    #gd[which(gd$SUBID == j), gdcols.slc] <- slc # write vector of fixed values in the gd dataframe
    gd[which(gd$SUBID == subid[j]), gdcols.slc] <- slc
    setTxtProgressBar(pb, j)
  }
  close(pb)
  print(nofrac) # print out vector of subids with no fraction to move
  return(gd)
}
