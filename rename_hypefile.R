#' Renames columns of input file according to a table of names
#' in such manner that HYPE recognizes
#' 
#' @param in_file input file to be renamed
#' @param name_table table of names
#' @param col_old string name of column in table of names with old name
#' @param col_new string name of column in table of names with replacement
#' @param multiple TRUE if multiple variables included 
#' @param num_vals number of variables included
#'
#' @return data.table - renamed input file
#' @export text_file of renamed input file
#'
#' @examples
#' renamehypefile(in_file = in_file, name_table = name_table,
#' col_old = "FROM", col_new = "FR_ORD")
#' 

renamehypefile <- function(in_file, name_table, col_old, col_new, multiple, num_vals){
  # desc: Renames columns of input file according to a table of names
  # in such manner that HYPE recognizes
  # arg in_file: input file to be renamed
  # arg name_table: table of names
  # arg col_old: string name of column in table of names with old name
  # arg col_new: string name of column in table of names with replacement
  # arg multiple: TRUE if multiple variables included 
  # arg num_vals: number of variables included
  # return: data.table - renamed input file
  
  library(data.table)
  in_file <- data.table(in_file) 
  name_table <- data.table(name_table)
  if (multiple == FALSE){
    n <- ncol(in_file) - 1
    pb <- txtProgressBar(min = 0, max = n, style = 3)
    c <- 1 # counter for progressbar
    for (i in colnames(in_file)[2:ncol(in_file)]){ # for each column name
      new_name <- name_table[get(col_old) == i, get(col_new)] # find new name in name table 
      position <- which(colnames(in_file) == i) # find a position of old column name in input file
      colnames(in_file)[position] <- new_name # replace the old column name with a new one
      setTxtProgressBar(pb, c)
      c <- c + 1 # counter for progressbar
    }
    close(pb)
  } else {
    n <- (ncol(in_file) - 1) / num_vals
    pb <- txtProgressBar(min = 0, max = n, style = 3)
    c <- 1 # counter for progressbar
    for (i in name_table[,get(col_old)]){ # for each old name
      new_name <- name_table[get(col_old) == i, get(col_new)] # find new name in name table 
      position <- which(grepl(x = colnames(in_file), pattern = i)) # find a position of old columns name in input file
      colnames(in_file)[position] <- new_name # replace the old column names with a new ones
      setTxtProgressBar(pb, c)
      c <- c + 1 # counter for progressbar
    }
    close(pb)
  }
  
  return(in_file) # return renamed input file
}



