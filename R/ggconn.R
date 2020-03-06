#' ggconn
#'
#' @param atlas Brain atlas to use. Inbuilt options include "aal91" "schafer300_n7"
#' @param background
#' @param view
#' @param conmat
#' @param interactive
#' @param node.size
#' @param node.color
#' @param all.nodes
#' @param edge.color
#' @param edge.alpha
#' @param edge.width
#' @param labels
#' @param show.legend
#' @param thr
#' @param uthr
#' @param scale.edge.width
#' @param label.size
#'
#'
#' @return
#' @export
#'
#' @examples
#' ggConnectome(atlas ="schafer300_n7", conmat=x, node.color = "network")
#' \dontrun{
#' ggConnectome(atlas ="schafer300_n7", conmat=x, node.color = "network")
#' }

ggconn <- function(atlas,
                         background='ICBM152',
                         view ="top",
                         conmat=NULL,
                         interactive = F,
                         node.size=6,
                         node.color="network",
                         all.nodes=FALSE,
                         edge.color="black",
                         edge.alpha=0.8,
                         edge.width=1,
                         labels=FALSE,
                         show.legend=TRUE,
                         thr=NULL,
                         uthr=NULL,
                         scale.edge.width = NULL,
                         label.size=1.5) {

  require(grid)
  require(png)

  list.atlas <- sub('\\.csv$', '', list.files(path="atlases/", pattern = "*.csv"))
  if(any(grepl(atlas, list.atlas, fixed=TRUE))) {data <- read.csv(paste0('atlases/', atlas, '.csv'))
  } else{stop(paste(paste('please select a valid atlas: '), paste(list.atlas, " ", collapse="")))
  }

  #set background
  list.backgroud <- sub('\\.png$', '', list.files(path="background/", pattern = "*.png"))

  if(any(grepl(background, list.backgroud, fixed=TRUE))) {background <-
    rasterGrob(readPNG(paste0("background/", background,"_", view,".png")))
  } else {stop(paste('please select a valid background: ', as.character(list.backgroud)))
  }

  #if no conmat is provided, build nparc x  nparc empty one
  nparc <- dim(data)[1]
  if (!exists("conmat")){conmat <- matrix(0L, nrow=nparc, ncol=nparc)
  }

  #Remove nodes with no edges
  ifelse(isSymmetric.matrix(conmat)==TRUE,
         directed <- FALSE,
         directed <- TRUE)


  if(all.nodes == FALSE && directed == FALSE) {
    include.vec <- vector(length=dim(data)[1])
    for (i in 1:dim(conmat)[1]){
      ifelse(any(conmat[i, ] > 0), include.vec[i] <- 1, include.vec[i] <- 0)
    }
    data <- data[as.logical(include.vec), ,drop=F]
    conmat <- conmat[which(rowSums(conmat) > 0), which(colSums(conmat) > 0), drop = F]
  }



  #split data into two (row data and col data)
  if(all.nodes==FALSE && directed == TRUE) {
    include.vec <- vector(length=dim(data)[1])
    for (i in 1:dim(conmat)[1]){
      ifelse(any(conmat[i, ] > 0) | any(conmat[, i] > 0), include.vec[i] <- 1, include.vec[i] <- 0)
    }
  }

  if(all.nodes==TRUE) {
    include.vec <- vector(length=dim(data)[1])
    include.vec <- rep(1, length=dim(data)[1])
  }


  #if interactive call build_plot_int, else call build con
  source("functions/build_plot.R")
  p <-  if(interactive==FALSE){build_plot(conmat, data, data.row, data.col, background, node.size=node.size, view,
                                          node.color=node.color, thr=NULL, uthr=NULL,
                                          edge.color=edge.color,edge.alpha=edge.alpha,
                                          edge.width=edge.width,  scale.edge.width=scale.edge.width,
                                          show.legend=show.legend, labels=labels, label.size=label.size,
                                          include.vec=include.vec)}

  source("functions/build_plot_int.R")
  p <-  if(interactive==TRUE){build_plot_int(conmat, data, data.row, data.col, background, node.size=node.size, view,
                                             node.color=node.color, thr=NULL, uthr=NULL,
                                             edge.color=edge.color,edge.alpha=edge.alpha,
                                             edge.width=edge.width,  scale.edge.width=scale.edge.width,
                                             show.legend=show.legend, labels=labels, label.size=label.size,
                                             include.vec=include.vec, view=view)}
  print(p)
}
