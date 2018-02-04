#' Cleans small SLC fractions in GeoData using funcion CleanSLCClasses, 
#' first by method 1 for landuse and soil respectively and then by method 2. 
#' Optionally it cleans out columns of SLCs populated by just zeros and fixes 
#' the GeoClass that it corespods with the GeoData.
#'  
#' @param geodata path to the GeoData.txt file
#' @param geoclass path to the GeoClass.txt file
#' @param headrow number indicating position of headrow in GeoClass file
#' @param m1.file_land see HYPEtools::CleanSLCClasses() help
#' @param m1.file_soil see HYPEtools::CleanSLCClasses() help
#' @param m2.frac see HYPEtools::CleanSLCClasses() help
#' @param m2.abs see HYPEtools::CleanSLCClasses() help
#' @param land_1st if TRUE, performs cleaning by landuse first then soil and vice versa
#' @param clean if TRUE cleans out SLC columns in GeoData populated by zeros and
#' fixes the GeoClass that it corespods with the GeoData.
#'
#' @return if param clean TRUE - list with fixed GeoData and GeoClass
#' @return if param clean FALSE - data frame of fixed GeoData
#'
#' @examples
#' clean_geodata(geodata = "E:/GeoData.txt", geoclass = "E:/GeoClass.txt", headrow = 1,
#' m1.file_land = "E:/transfer_land.txt", m1.file_soil = "E:/transfer_soil.txt",
#' m2.frac = 0.02, m2.abs = 10000, clean = T)

clean_geodata <- function(geodata, geoclass, headrow, m1.file_land, m1.file_soil,
                          m2.frac, m2.abs, land_1st, clean){
  # desc: Cleans small SLC fractions in GeoData using funcion CleanSLCClasses, 
  # first by method 1 for landuse and soil respectively and then by method 2. 
  # Optionally it cleans out columns of SLCs populated by just zeros and fixes 
  # the GeoClass that it corespods with the GeoData.
  # arg geodata: path to the GeoData.txt file
  # arg geoclass: path to the GeoClass.txt file
  # arg headrow: number indicating position of headrow in GeoClass file
  # arg m1.file_land: see HYPEtools::CleanSLCClasses() help
  # arg m1.file_soil: see HYPEtools::CleanSLCClasses() help
  # arg m2.frac: see HYPEtools::CleanSLCClasses() help
  # arg m2.abs: see HYPEtools::CleanSLCClasses() help
  # arg land_1st: if TRUE, performs cleaning by landuse first then soil and vice versa
  # arg clean: if TRUE cleans out SLC columns in Geodata populated by zeros and
  # fixes the GeoClass that it corespods with the GeoData.
  
  library(HYPEtools)
  gd <- ReadGeoData(filename = geodata)
  gc <- ReadGeoClass(filename = geoclass, headrow = headrow)
  if (land_1st == TRUE){
    gd1 <- CleanSLCClasses(gd = gd, gcl = gc, m1.file = m1.file_land, m1.class = "landuse",
                           m1.clean = c(T,T), m1.precedence = c(T,T), progbar = T)
    gd2 <- CleanSLCClasses(gd = gd1, gcl = gc, m1.file = m1.file_soil, m1.class = "soil",
                           m1.clean = c(T,T), m1.precedence = c(T,T), progbar = T) 
  } else {
    gd1 <- CleanSLCClasses(gd = gd, gcl = gc, m1.file = m1.file_soil, m1.class = "soil",
                           m1.clean = c(T,T), m1.precedence = c(T,T), progbar = T)
    gd2 <- CleanSLCClasses(gd = gd1, gcl = gc, m1.file = m1.file_land, m1.class = "landuse",
                           m1.clean = c(T,T), m1.precedence = c(T,T), progbar = T)
  }
  
  gd3 <- CleanSLCClasses(gd = gd2, gcl = gc, m2.frac = m2.frac, m2.abs = m2.abs, progbar = T)
  
  if (clean == TRUE){
    col_pos <- grep("SLC_", colnames(gd3)) # indexes of colums with SLC
    col_sum <- colSums(gd3[grep("SLC_", colnames(gd3))]) # colsum of SLC columns
    zeros <- which(col_sum == 0) # indexes of columns having zero sum
    gd3[,col_pos[zeros]] <- NULL # delete columns having zero sum
    gc <- gc[-zeros,] # delete rows from GeoClass dataframe
    gc[,1] <- 1:nrow(gc) # new numbering of GeoClass dataframe
    col_pos2 <- grep("SLC_", colnames(gd3)) # new indexes of colums with SLC
    colnames(gd3)[col_pos2] <- paste0("SLC_",gc[,1]) # new numbering of GeoData dataframe
    gd4 <- SortGeoData(gd3)
    return(list(gd = gd4, gc = gc))
  } else {
    gd4 <- SortGeoData(gd3)
    return(gd4)
  }
  
}
