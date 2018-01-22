#' Plots out likelihood measure for all parameters together or for each parameter
#' separately
#' 
#' @author https://github.com/rcapell/
#'
#' @param allsim the path to the allsim.txt file 
#' @param ngen number of generations in calibration 
#' @param npop number of populations in calibration
#' @param params if TRUE plots out each parameter separately
#'
#' @return
#'
#' @examples 
#' demc_spaghetti_violin(allsim = "D:/ownCloud/uziv/calibration/allsim.txt", 
#'                                 ngen = 250, npop = 20, params = F)

demc_spaghetti_violin <- function(allsim, ngen, npop, params){
  # desc: Plots out likelihood measure for all parameters together or for each 
  # parameter separately
  # author: https://github.com/rcapell/
  # arg allsim: the path to the allsim.txt file 
  # arg ngen: number of generations in calibration
  # arg npop: number of populations in calibration
  # arg params: if TRUE plots out each parameter separately
  # return: ggplot 
  
  library(HYPEtools)
  library(ggplot2)
  library(reshape2)
  library(data.table)
  allsim <- ReadAllsim(filename = allsim)
  binsim <- function(binwidth = 5, npop, ngen) {
    nbin <- ngen  / binwidth # number of bins
    rep(seq(from = binwidth - (binwidth / 2), to = ngen - (binwidth / 2), 
            by = binwidth), each = binwidth * npop)
  }
  # create population and generation vectors
  ngen <- ngen
  npop <- npop
  allgen <- rep(1:ngen, each = npop)
  allpop <- rep(1:npop, times = ngen)
  allbin <- binsim(npop = npop, ngen = ngen, binwidth = 10)
  
  if (params == FALSE){
    # create plot dataframe for criterium
    ggc <- data.frame(allgen, allpop, allbin, allsim)
    # replace non-accepted iterations with parent iterations (q&d version with for loops)
    ggc.te <- split(x = ggc, f = ggc$allpop)
    ggc <- ggc[-c(1:nrow(ggc)), ]
    for (i in 1:length(ggc.te)) {
      te <- ggc.te[[i]][, -c(1:3)]
      allgpb <- ggc.te[[i]][, c(1:3)]
      for (j in 2:nrow(te)) {
        if (te$iacc[j] == 0) {
          te[j, ] <- te[j - 1, ]
        }
      }
      ggc <- rbind(ggc, cbind(allgpb, te))
    }
    rm(i, j, te, ggc.te)
    ggplot(data = ggc, aes(x = allgen, y = CRIT)) +
      geom_line(aes(colour = allpop, group = allpop), alpha = 0.5)  +
      geom_violin(aes(group = allbin), position = position_identity(), fill = NA, colour = "black") +
      ylab("Likelihood measure") +
      xlab("DEMC generation") +
      geom_vline(xintercept = 150, linetype = "dotted", col = 2, lwd = 1) +
      coord_cartesian(ylim = range(ggc$CRIT)) +
      theme(legend.position="none")
    
  } else {
    ######################## PARAMETERS ########################
    
    ggp <- data.frame(allgen, allpop, allbin, allsim[, c(21:(ncol(allsim) - 3), ncol(allsim))])
    # replace non-accepted iterations with parent iterations (q&d version with for loops)
    ggp.te <- split(x = ggp, f = ggp$allpop)
    ggp <- ggp[-c(1:nrow(ggp)), ]
    for (i in 1:length(ggp.te)) {
      te <- ggp.te[[i]][, -c(1:3)]
      allgpb <- ggp.te[[i]][, c(1:3)]
      for (j in 2:nrow(te)) {
        if (te$iacc[j] == 0) {
          te[j, ] <- te[j - 1, ]
        }
      }
      ggp <- rbind(ggp, cbind(allgpb, te))
    }
    rm(i, j, te, ggp.te, allgpb)
    # melt into form so that all parameters are concatenated in one "variable" vector
    ggp <- melt(ggp, id.vars = 1:3, measure.vars = 4:(ncol(ggp) - 1))
    ggp <- data.table(ggp)
    ggp <- ggp[!variable %in% c("numrc", "nummc")]
    ggplot(data = ggp, aes(x = allgen, y = value)) +
      scale_colour_gradient(name = "DEMC\npopulation") +
      scale_fill_gradient(name = "DEMC\npopulation") +
      geom_line(aes(colour = allpop, group = allpop), alpha = 0.5)  +
      geom_violin(aes(group = allbin), position = position_identity(), fill = NA, colour = "black") +
      facet_wrap(~ variable, nrow = 9, ncol = 2, scales = "free_y") +
      xlab("DEMC generation") + ylab("Parameter value")
  }
}
