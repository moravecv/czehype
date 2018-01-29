#' Fuction converts sentinel coding of polygons to lpis coding
#'
#' @param x vector of sentinel coding characters 
#'
#' @return vector of lpis coding characters
#'
#' @examples sent_2015$LPIS_KOD = sntnl2lpis(sent_2015$NKOD_DPB)

sntnl2lpis <- function(x){
  # desc: Fuction converts sentinel coding of polygons to lpis coding
  # arg x: vector of sentinel coding characters
  # return: vector of lpis coding characters
  
  ifelse(test = nchar(as.character(x)) == 9,
         yes = paste0(substr(x, 1, 2), "0", "-", substr(x, 4, 6), "0", "/", 
                      substr(x, 3, 3), substr(x, 7, 9)), 
         no = ifelse(test = nchar(as.character(x)) == 11,
                     yes = paste0(substr(x, 1, 2), "0", "-", substr(x, 4, 6),
                                  "0", "/", substr(x, 3, 3), substr(x, 7, 11)),
                     no = ifelse(test = nchar(as.character(x)) == 12,
                                 yes = paste0(substr(x, 1, 2), "0", "-", substr(x, 4, 6),
                                              "0", "/", substr(x, 3, 3), substr(x, 7, 12)),
                                 no = paste0(substr(x, 1, 2), "0", "-", substr(x, 4, 6),
                                             "0", "/", substr(x, 3, 3), substr(x, 7, 13)))))}
