#' Creates geoclass file from geodata file
#'
#' @param in_file input geodata file
#' @param vg_typ1 vector of numbers indicating vegetation type 1
#' @param vg_typ2 vector of numbers indicating vegetation type 2
#' @param vg_typ3 vector of numbers indicating vegetation type 3
#' @param sc_river number idicating river class
#' @param sc_wbody nuber indicating water body class
#'
#' @return data.table of geoclass file
#' @export indicators
#'
#' 
#' 

create_geoclass <- function(in_file, vg_typ1, vg_typ2,vg_typ3, sc_river, sc_wbody){
  # desc: Creates geoclass file from geodata file
  # arg in_file: input geodata file
  # arg vg_typ1: vector of numbers indicating vegetation type 1
  # arg vg_typ2: vector of numbers indicating vegetation type 2
  # arg vg_typ3: vector of numbers indicating vegetation type 3
  # arg sc_river: number idicating river class
  # arg sc_wbody: nuber indicating water body class
  # return: data.table of geoclass file
  
  library(HYPEtools)
  library(data.table)
  gd <- ReadGeoData(filename = input_file)
  tt <- data.frame(t(gd))
  GC <- data.table(rownames(tt))
  GC <- GC[V1 %like% "SLC",]
  ######### COMBINATION #########
  GC$V1 <- gsub(pattern = "SLC_", replacement = "", x = GC$V1)
  colnames(GC)[1] <- "COMBINATION"
  ######### LANDUSE #########
  GC$LANDUSE <- substr(x = GC$COMBINATION, start = 1, stop = 2)
  GC$LANDUSE <- as.integer(GC$LANDUSE)
  GC$LANDUSE <- GC$LANDUSE - 9
  ######### SOIL #########
  GC$SOILTYPE <- substr(x = GC$COMBINATION, start = 4, stop = 4)
  GC$SOILTYPE <- as.integer(GC$SOILTYPE)
  ######### CROPS #########
  GC$cropid_main <- 0
  GC$cropid_2nd <- 0
  GC$rotation <- 0
  ######### VEGETATIONS TYP #########
  GC[LANDUSE %in% (vg_typ1 - 9), vegetationstyp:= 1]
  GC[LANDUSE %in% (vg_typ2 - 9), vegetationstyp:= 2]
  GC[LANDUSE %in% (vg_typ3 - 9), vegetationstyp:= 3]
  ######### SPECIAL CLASS #########
  GC[, special_class:= 0]
  GC[LANDUSE %in% (sc_river - 9), special_class:= 12]
  GC[LANDUSE %in% (sc_wbody - 9), special_class:= 2]
  ######### REST #########
  GC$tile_depth <- 0
  GC$drain_depth <- 0
  GC$soil_layers <- 3
  ######### SOIL DEPTH #########
  GC$DEPTH <- substr(x = GC$COMBINATION, start = 3, stop = 3)
  GC$depth_s1 <- 0.25
  GC[DEPTH == "1",depth_s2:= 0.25]
  GC[DEPTH == 2,depth_s2:= 0.5]
  GC[DEPTH == 3,depth_s2:= 0.8]
  GC[DEPTH == 4,depth_s2:= 1]
  GC$depth_s3 <- GC$depth_s2 + 1.5
  ######### NEW DRAIN DEPTH #########
  GC$drain_depth <- GC$depth_s3
  ######### OLAKES ILAKES #########
  GC[COMBINATION == "2050", special_class:= 1]
  GC[COMBINATION == "2051", special_class:= 2]
  
  return(GC)
}