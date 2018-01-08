############################## get_subid_shp ##############################

#' Function get_subid_shp returns polygon for specified SUBID
#'
#' @param subid number of the SUBID
#' @param shape shp of all catchments
#'
#' @return SpatialPolygonsDataFrame of selected SUBID
#'
#' @examples 
#' get_subid_shp(subid = 30, shape = readOGR("D:/Catchment.shp",layer = "Catchment"))
#'

get_subid_shp <- function(subid,shape){
  # desc: Function get_subid_shp returns polygon for specified SUBID
  # arg subid: number of the SUBID
  # arg shape: shp of all catchments
  # return: SpatialPolygonsDataFrame of selected SUBID
  
  out <- shape[match(subid, shape$SUBID), ]
  out$SUBID <- subid
  return(out)
}

############################## get_usage ##############################

#' Function get_usage returns dataframe of water usage for specified SUBID 
#'
#' @param catchment SpatialPolygonsDataFrame of selected SUBID
#'
#' @return dataframe of water usage for specified SUBID
#'
#' @examples 
#' get_usage(catchment = get_subid_shp())
#'

get_usage <- function(catchment){
  # desc: Function get_usage returns dataframe of water usage for specified SUBID 
  # arg catchment: SpatialPolygonsDataFrame of selected SUBID
  # return: dataframe of water usage for specified SUBID
  
  bb <- bbox(catchment)
  u <- uzivani[!is.na(X) & !is.na(Y)][X >= bb['x', 'min'] & X <= bb['x', 'max'] 
                                      & Y >= bb['y', 'min'] & Y <= bb['y', 'max']]
  
  if (dim(u)[1] != 0){
    
    u[, xy:=paste(X, Y)]
    u[, uq:=!duplicated(xy), by = ICOC]
    xy <- u[uq == TRUE, SpatialPointsDataFrame(coords = list(X, Y),
                                               data = data.frame('ICOC' = u[uq == TRUE, ICOC]))]
    
    wh <- gIntersects(xy, catchment, byid = TRUE)
    if (any(wh == TRUE)){
      xxy <- xy[which(wh == TRUE), ]
      a <- dcast(u[ICOC %in% xxy$ICOC, .(value = sum(value)), by = .(DTM, JEV)], DTM ~ JEV)
      b <- dcast(u[ICOC %in% xxy$ICOC, .(PERMIT = sum(PMM3_MES, na.rm = TRUE)),
                   by = .(DTM, JEV)], DTM ~ JEV, value.var = 'PERMIT')
      setnames(b, names(b)[names(b)!='DTM'], paste0('perm_', names(b)[names(b)!='DTM']))
      merge(a, b, by = c('DTM'))
    }
    else {
      data.frame(DTM = seq(as.Date("1979-01-15"), as.Date("2015-12-15"), "month"),
                 POD = NA, POV = NA, VYP = NA, perm_POD = NA, perm_POV = NA, perm_VYP = NA)
    }
  }
  else {
    data.frame(DTM = seq(as.Date("1979-01-15"), as.Date("2015-12-15"), "month"),
               POD = NA, POV = NA, VYP = NA, perm_POD = NA, perm_POV = NA, perm_VYP = NA)
  }
}

############################## get_usage_all ##############################

#' Function get_usage_all returns dataframe of water usage for specified SUBIDs in list 
#'
#' @param list list of SUBID numbers
#' @param shape shp of all catchments
#'
#' @return dataframe of water usage for specified SUBIDs in list
#'
#' @examples 
#' get_usage_all(list = c(2,4,6), shape = readOGR("D:/Catchment.shp",layer = "Catchment"))
#'

get_usage_all <- function(list, shape){
  # desc: Function get_usage_all dataframe of water usage for specified SUBIDs in list
  # arg list: list of SUBID numbers
  # arg shape: shp of all catchments
  # return: dataframe of water usage for specified SUBIDs in list
  
  library(plyr)
  library(data.table)
  uziv = data.frame(matrix(nrow = 0,ncol = 8))
  colnames(uziv) = c("DTM","POD","POV","VYP","perm_POD","perm_POV","perm_VYP","SUBID")
  uziv$DTM = as.Date(uziv$DTM)
  uziv$POD = as.integer(uziv$POD)
  uziv$POV = as.integer(uziv$POV)
  uziv$VYP = as.integer(uziv$VYP)
  uziv$perm_POD = as.integer(uziv$perm_POD)
  uziv$perm_POV = as.integer(uziv$perm_POV)
  uziv$perm_VYP = as.integer(uziv$perm_VYP)
  uziv$SUBID = as.integer(uziv$SUBID)
  data('uzivani')
  for (i in list){
    print(i)
    subid <- get_subid_shp(i, shape)
    c <- get_usage(subid)
    c$SUBID <- i
    uziv <- plyr::rbind.fill(uziv, c)
  }
  uziv <- uziv[,c(1,2,3,4,8)]
  uziv$NAS <- apply(X = uziv, MARGIN = 1, FUN = function(x) sum(is.na(x)))
  uziv <- data.table(uziv)
  uziv_fin <- uziv[NAS != 3,]
  uziv_fin$NAS <- NULL
  return(uziv_fin)
}

############################## eom ##############################

#' Function eom returns last day of the month 
#'
#' @param date any given day in Date format
#'
#' @return last day of the month in Date format 
#'
#' @examples eom("1961-03-07 GMT")
#' 

eom <- function(date) {
  # desc: Function eom returns last day of the month
  # arg date: any given day in Date format
  # return: last day of the month in Date format 
  
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0, tz=attr(date,"tz"))
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

############################## som ##############################

#' Function som returns first day of the month 
#'
#' @param date any given day in Date format
#'
#' @return first day of the month in Date format 
#'
#' @examples som("1961-03-07 GMT")
#' 

som <- function(x) {
  # desc: Function som returns first day of the month
  # arg date: any given day in Date format
  # return: first day of the month in Date format 
  
  as.Date(format(x, "%Y-%m-01"))
}

############################## usage.VUV2HYPE ##############################

#' Function usage.VUV2HYPE returns HYPE friendly dataframe of water usage 
#'
#' @param subid number of the SUBID
#' @param data the dataframe of water usage
#'
#' @return HYPE friendly dataframe of water usage
#'
#' @examples usage.VUV2HYPE(subid = 2, data = get_usage_all())
#' 

usage.VUV2HYPE <- function(subid, data){
  # desc: Function usage.VUV2HYPE returns HYPE friendly dataframe of water usage 
  # arg subid: number of the SUBID
  # arg data: the dataframe of water usage
  # return: HYPE friendly dataframe of water usage
  
  print(subid)
  message("Zuzuji vyber ...")
  sekce <- data[SUBID == subid,]
  sekce$POD[is.na(sekce$POD)] <- 0
  sekce$POV[is.na(sekce$POV)] <- 0
  sekce$VYP[is.na(sekce$VYP)] <- 0
  message("Vytvarim casovou osu ...")
  zac <- som(data[SUBID == subid,1][1])
  poc <- nrow(data[SUBID == subid,])
  start <- seq(as.POSIXct(zac),length.out = poc,by = "months")
  cas <- data.frame(fromdate = start, todate = eom(start))
  message("Pocitam bilanci ...")
  bil <- (1000*(sekce$VYP-(sekce$POD+sekce$POV)))/(ceiling(as.numeric(as.POSIXct(cas$todate)-as.POSIXct(cas$fromdate)))+1)
  message("Vytvarim PointSourceData ...")
  PointSourceData <- data.frame(subid = sekce$SUBID, ps_vol = bil, fromdate = round.POSIXt(x = cas$fromdate, units = "days"), todate = cas$todate)
  return(PointSourceData)
}

############################## usage.VUV2HYPE_all ##############################

#' Function usage.VUV2HYPE_all returns HYPE friendly dataframe of water usage
#' for all SUBIDs in the list 
#'
#' @param list list of SUBID numbers
#' @param write if TRUE writes PointSourceData.txt file
#' @param path_out path where to save the PointSourceData.txt file
#'
#' @return HYPE friendly dataframe of water usage
#'
#' @examples 
#' usage.VUV2HYPE_all(list = c(2,3,4), write = TRUE, path_out = "D:/PointSourceData.txt")
#'

usage.VUV2HYPE_all <- function(list, write, path_out){
  # desc: Function usage.VUV2HYPE_all returns HYPE friendly dataframe of water usage
  # for all SUBIDs in the list 
  # arg list: list of SUBID numbers
  # arg write: if TRUE writes PointSourceData.txt file
  # arg path_out: path where to save the PointSourceData.txt file
  
  library(HYPEtools)
  PointSourceData = list()
  for (i in list){
    usage <- usage.VUV2HYPE(i)
    PointSourceData <- rbind(PointSourceData, usage)
  }
  if (write == TRUE){
    WritePointSourceData(x = PointSourceData, filename = path_out)
  } else {
    return(PointSourceData)
  }
}

############################## swgw.VUV2HYPE ##############################

#' Function swgw.VUV2HYPE calculates the ratio between ground water and surface water withdrawal  
#'
#' @param subid number of the SUBID
#' @param data the dataframe of water usage
#'
#' @return data frame with ratio between ground water and surface water withdrawal
#'
#' @examples 
#' swgw.VUV2HYPE(subid = 2, data = get_usage_all())
#'

swgw.VUV2HYPE <- function(subid, data){
  # desc: Function swgw.VUV2HYPE calculates the ratio between ground water and surface water withdrawal 
  # arg subid: number of the SUBID
  # arg data: the dataframe of water usage
  # return: data frame with ratio between ground water and surface water withdrawal
  
  print(subid)
  message("Zuzuji vyber ...")
  sekce <- data[SUBID == subid,]
  sekce$POD[is.na(sekce$POD)] <- 0
  sekce$POV[is.na(sekce$POV)] <- 0
  sekce$VYP[is.na(sekce$VYP)] <- 0
  message("Pocitam bilanci ...")
  pom <- ((100/(sum(sekce$POD)+sum(sekce$POV)))*sum(sekce$POD))/100
  message("Vytvarim MgmtData ...")
  MgmtData <- data.frame(subid = i, gw_part = pom)
  return(MgmtData)
  
}

############################## swgw.VUV2HYPE_all ##############################

#' Function swgw.VUV2HYPE calculates the ratio between ground water 
#' and surface water withdrawal for all SUBIDs in the list 
#'
#' @param list list of SUBID numbers
#' @param write if TRUE writes MgmtData.txt file
#' @param path_out path where to save the MgmtData.txt file
#'
#' @return data frame with ratio between ground water and surface water withdrawal
#'
#' @examples 
#' swgw.VUV2HYPE_all(list = c(2,3,4), write = TRUE, path_out = "D:/MgmtData.txt")
#'

swgw.VUV2HYPE_all <- function(list, write, path_out){
  # desc: Function swgw.VUV2HYPE calculates the ratio between ground water 
  # and surface water withdrawal for all SUBIDs in the list 
  # arg list: list of SUBID numbers
  # arg write: if TRUE writes MgmtData.txt file
  # arg path_out: path where to save the MgmtData.txt file
  
  library(HYPEtools)
  library(data.table)
  MgmtData <- list()
  for (i in list){
    ratio <- swgw.VUV2HYPE(i)
    MgmtData <- rbind(MgmtData, ratio)
  }
  MgmtData <- data.table(MgmtData)
  MgmtData[is.na(gw_part),gw_part:= 0]
  if (write == TRUE){
    WriteMgmtData(MgmtData, filename = path_out)
  } else {
    return(MgmtData)
  }
} 
