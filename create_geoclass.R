#' Creates Geoclass file from Geodata file
#'
#' @param in_file input Geodata file
#' @param vg_typ1 vector of numbers indicating vegetation type 1
#' @param vg_typ2 vector of numbers indicating vegetation type 2
#' @param vg_typ3 vector of numbers indicating vegetation type 3
#' @param sc_river number idicating river class
#' @param sc_wbody nuber indicating water body class
#' @param write write GeoClass in file if TRUE
#' @param path_out path where to save the GeoData.txt file
#'
#' @return data.table of Geoclass file
#' @export text_file of Geoclass data
#' 
#' @examples 
#' create_geoclass(in_file = "E:/GeoData.txt", vg_typ1 = c(10,11,12,13,14,18),
#' vg_typ2 = c(15,16,17), vg_typ3 = c(19,20,21), sc_river = 20, sc_wbody = 21,
#' write = T, path_out = "E:/GeoClass.txt")
#' 

create_geoclass <- function(in_file, vg_typ1, vg_typ2,vg_typ3, sc_river, sc_wbody,
                            write, path_out){
  # desc: Creates geoclass file from geodata file
  # arg in_file: input geodata file
  # arg vg_typ1: vector of numbers indicating vegetation type 1
  # arg vg_typ2: vector of numbers indicating vegetation type 2
  # arg vg_typ3: vector of numbers indicating vegetation type 3
  # arg sc_river: number idicating river class
  # arg sc_wbody: nuber indicating water body class
  # arg write: write GeoClass in file if TRUE
  # arg path_out: path where to save the GeoData.txt file
  # return: data.table of geoclass file
  
  library(HYPEtools)
  library(data.table)
  gd <- ReadGeoData(filename = in_file)
  tt <- data.frame(t(gd))
  GC <- data.table(rownames(tt))
  GC <- GC[V1 %like% "SLC",]
  ######### Combination #########
  GC$V1 <- gsub(pattern = "SLC_", replacement = "", x = GC$V1)
  colnames(GC)[1] <- "COMBINATION"
  ######### Landuse #########
  GC$LANDUSE <- substr(x = GC$COMBINATION, start = 1, stop = 2)
  GC$LANDUSE <- as.integer(GC$LANDUSE)
  GC$LANDUSE <- GC$LANDUSE - 9
  ######### Soil #########
  GC$SOILTYPE <- substr(x = GC$COMBINATION, start = 4, stop = 4)
  GC$SOILTYPE <- as.integer(GC$SOILTYPE)
  ######### Crops #########
  GC$cropid_main <- 0
  GC$cropid_2nd <- 0
  GC$rotation <- 0
  ######### Vegetations typ #########
  GC[LANDUSE %in% (vg_typ1 - 9), vegetationstyp:= 1]
  GC[LANDUSE %in% (vg_typ2 - 9), vegetationstyp:= 2]
  GC[LANDUSE %in% (vg_typ3 - 9), vegetationstyp:= 3]
  ######### Special class #########
  GC[, special_class:= 0]
  GC[LANDUSE %in% (sc_river - 9), special_class:= 12]
  GC[LANDUSE %in% (sc_wbody - 9), special_class:= 2]
  ######### Rest #########
  GC$tile_depth <- 0
  GC$drain_depth <- 0
  GC$soil_layers <- 3
  ######### Soil depths #########
  GC$DEPTH <- as.integer(substr(x = GC$COMBINATION, start = 3, stop = 3))
  GC$depth_s1 <- 0.25
  GC[DEPTH == 1,depth_s2:= 0.25]
  GC[DEPTH == 2,depth_s2:= 0.5]
  GC[DEPTH == 3,depth_s2:= 0.8]
  GC[DEPTH == 4,depth_s2:= 1]
  GC$depth_s3 <- GC$depth_s2 + 1.5
  ######### New drain depth #########
  GC$drain_depth <- GC$depth_s3
  ######### Olake Ilakes #########
  GC[COMBINATION == "5050", special_class:= 1]
  GC[COMBINATION == "5151", special_class:= 2]
  GC[COMBINATION == "5050", vegetationstyp:= 3]
  GC[COMBINATION == "5151", vegetationstyp:= 3]
  ###### Water related values #######
  GC[special_class %in% c(1,2,12), drain_depth:=1]
  GC[special_class %in% c(1,2,12), soil_layers:=1]
  GC[special_class %in% c(1,2,12), depth_s1:=1]
  GC[special_class %in% c(1,2,12), depth_s2:=1]
  GC[special_class %in% c(1,2,12), depth_s3:=1]
  GC[special_class %in% c(1,2,12), SOILTYPE:=99]
  ###### Clean#######
  GC$DEPTH <- NULL
  GC$COMBI_CHECK <- GC$COMBINATION
  GC$COMBINATION <- 1:nrow(GC)
  ###### Write to .txt ######
  if (write == TRUE){
    write.table(GC, file = path_out, quote = FALSE, sep = "\t", col.names = FALSE,
                row.names = FALSE, append = TRUE, na = "")
  } else {
    return(GC)
  }
}
