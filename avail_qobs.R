#' Prints out the number of available observations in Qobs file over the time 
#'
#' @param Qobs the path to the Qobs.txt file
#'
#' @return ggplot of the number of available observations in Qobs file over the time
#'
#' @examples avail_qobs("D:/Data/Qobs.txt")
#'

avail_qobs <- function(Qobs){
  # desc: Prints out the number of available observations in Qobs file over the time
  # arg: Qobs the path to the Qobs.txt file
  
  library(HYPEtools)
  library(ggplot2)
  library(scales)
  
  Qobs <- ReadPTQobs(Qobs)
  sums <- apply(X = Qobs[,2:ncol(Qobs)], MARGIN = 1, FUN = function(x) sum(!is.na(x)))
  df <- data.frame(DTM = Qobs$DATE, COUNT = sums)
  df$DTM <- as.Date(df$DTM)
  
  ggplot(df) +
    geom_ribbon(aes(x = DTM, ymin = 0, ymax = COUNT), fill = "#15A9C6", alpha = 0.7) +
    scale_x_date(breaks = date_breaks("3 years"), labels = date_format("%Y")) +
    theme(
      plot.title = element_text(size = 8, lineheight=.8, face="plain"),
      axis.text = element_text(size = 8),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.ticks = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 6, face = "italic"),
      legend.key = element_rect(fill = "white"),
      legend.position = "bottom",
      panel.background = element_rect(colour = "white", fill = "white"),
      panel.grid.major = element_line(colour = "gray", linetype = "dashed", size = 0.2),
      panel.grid.minor = element_line(colour = "gray", linetype = "dashed", size = 0.2)
    )
}
