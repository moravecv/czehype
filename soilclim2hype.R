soilclim2hype = function(basins, variables, path_soilclim, write = FALSE, xobs = FALSE, varhype, path_out){
  require(data.table)
  require(HYPEtools)
  if (write == TRUE & xobs == FALSE){
    c = data.frame()
    for (i in basins){
      a = data.table(readRDS(paste0(path, i)))
      b = a[, c("UPOV_ID","DTM", variables), with = FALSE]
      c = rbind(c,b)
    }
    d = dcast(data = c, formula = DTM ~ UPOV_ID, value.var = variables)
    WritePTQobs(x = d,filename = path_out, obsid = colnames(d)[2:ncol(d)])
  }
  else if (write == TRUE & xobs == TRUE) {
    c = data.frame()
    for (i in basins){
      a = data.table(readRDS(paste0(path, i)))
      b = a[, c("UPOV_ID","DTM", variables), with = FALSE]
      c = rbind(c,b)
    }
    d = dcast(data = c, formula = DTM ~ UPOV_ID, value.var = variables)
    WriteXobs(x = d,filename = path_out, comment = variables, variable = rep(varhype, length(colnames(d)[2:ncol(d)])), subid = colnames(d)[2:length(d)])
  }
  else {
    c = data.frame()
    for (i in basins){
      a = data.table(readRDS(paste0(path, i)))
      b = a[, c("UPOV_ID","DTM", variables), with = FALSE]
      c = rbind(c,b)
    }
    d = dcast(data = c, formula = DTM ~ UPOV_ID, value.var = variables)
    return(d)
  }
}