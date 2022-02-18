#' Plots brains
#'
#' \code{brainconn} plots and returns a ggraph object of plotted brain connectivity matrix..
#' @author Sidhant Chopra
#'
#' @param atlas Either a string of one of the included atlases \code{brainconn::list_atlases{} or a .data.frame that meets specifications, see \code{vignette("brainconn")}
#' @param background 'ICBM152', currently the only background option
#' @param background.alpha Number between 0-1 to  set the transparency of the background.
#' @param view A sting to choose the view. Can be any of these: c("ortho", "top", "bottom", "left", "right")
#' @param conmat A adjacency matrix. Can be binary, weights, directed or undirected. see example_* data.
#' @param node.size A integer that determines the diameter of the nodes. Can also be a vector of integers with a length equal to the number of ROIs in the atlas
#' @param node.color A string that sets the node color. e.g. "blue". If set to "network", then nodes will be colored according to the network column of the atlas
#' @param edge.color.weighted A boolean that applies when the conmat is weighted. if \code{TRUE}, then edges will be colored according to the weight \code{FALSE}, the edges will be sized according to weight.
#' @param all.nodes if \code{TRUE}, then all nodes will be shown be hemisphere without ticks. If \code{FALSE}, then only nodes with connecting edges will be shown.
#' @param edge.color A string that sets the edge color. e.g. "blue".
#' @param edge.alpha Number between 0-1 to  set the transparency of the edges.
#' @param edge.width Number to set the width of the edges.
#' @param labels if \code{TRUE}, ROI labels for all visible nodes will be shown. If \code{FALSE}, then no labes will be shown.
#' @param show.legend if \code{TRUE}, legend will be shown. If \code{FALSE}, then no legend will be shown.
#' @param thr a optional value to set a threshold on the conmat (e.g. edges with a weighted value lower than the one set here will not be shown)
#' @param uthr a optional value to set a upper threshold on the conmat (e.g. edges with a weighted value higher than the one set here will not be shown)
#' @param scale.edge.width If \code{edge.color.weighted=FALSE}, you can use this rescale the edge.width according to weight. e.g. \code{scale.edge.width = c(1,3)}
#' @param label.size If labels=TRUE then, \code{label.size} can can be set as in integer to control the size of the labels.
#' @param label.edge.weight if \code{TRUE}, then the edge weight will be labels along the edge.
#'
#' @return a ggraph object
#'
#' @import ggraph
#' @import cowplot
#' @import grid
#' @importFrom grDevices rgb
#' @examples
#' library(brainconn)
#' x <- example_unweighted_undirected
#' brainconn(atlas ="schaefer300_n7", conmat=x, node.size = 3, view="ortho")
#' @export
brainconn <- function(atlas,
                      background='ICBM152',
                      view ="top",
                      conmat=NULL,
                      #     interactive = F,
                      node.size=4,
                      node.color="network",
                      all.nodes=FALSE,
                      edge.color="black",
                      edge.alpha=0.8,
                      edge.width=1,
                      edge.color.weighted=FALSE,
                      labels=FALSE,
                      show.legend=TRUE,
                      thr=NULL,
                      uthr=NULL,
                      scale.edge.width = NULL,
                      label.size=1.5,
                      label.edge.weight = FALSE,
                      background.alpha = 1) {



  ifelse(is.character(atlas), data <- get(atlas), data <- atlas)

  #set background

  #loop three times for the three vies that make ortho view
  if (view == "ortho") {
    ortho_list <- list()
    ortho_views  <- c("top", "left", "front")
    for (v in 1:3) {
      view <- ortho_views[v]
      bg <- paste0("ICBM152_", view)
      m <- get(bg)
      #if(any(grepl(background, list.backgroud, fixed=TRUE))) {
      #  m <- png::readPNG(paste0("data/background/", background,"_", view,".png"))
      #
      w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * background.alpha), nrow=dim(m)[1])
      background <- rasterGrob(w)
      #} else {stop(paste('please select a valid background: ', as.character(list.backgroud)))
      #}

      #if no conmat is provided, build nparc x  nparc empty one
      nparc <- dim(data)[1]
      if (!exists("conmat")){conmat <- matrix(0L, nrow=nparc, ncol=nparc)
      }

      #convert conmat to matrix
      conmat <- as.matrix(conmat)


      #Remove nodes with no edges
      rownames(conmat) <- colnames(conmat) #this needs to be same same if is.sym to work
      ifelse(isSymmetric.matrix(conmat)==TRUE,
             directed <- FALSE,
             directed <- TRUE)



      if(all.nodes == FALSE && directed == FALSE) {
        include.vec <- vector(length=dim(data)[1])
        for (i in 1:dim(conmat)[1]){
          ifelse(any(conmat[i, ] != 0), include.vec[i] <- 1, include.vec[i] <- 0)
        }
        data <- data[as.logical(include.vec), ,drop=F]
        conmat <- conmat[which(rowSums(conmat, na.rm = T) != 0), which(colSums(conmat, na.rm = T) != 0), drop = F]
      }


      if(all.nodes==FALSE && directed == TRUE) {
        include.vec <- vector(length=dim(data)[1])
        for (i in 1:dim(conmat)[1]){
          ifelse(any(conmat[i, ] != 0) | any(conmat[, i] != 0), include.vec[i] <- 1, include.vec[i] <- 0)
        }
      }

      if(all.nodes==TRUE) {
        include.vec <- vector(length=dim(data)[1])
        include.vec <- rep(1, length=dim(data)[1])
      }


      #if interactive call build_plot_int, else call build con
      #  source("functions/build_plot.R")

      #in ortho view, only show legend for top view to avoid redundancy
      ifelse(v == 1, show.legend <- T, show.legend <- F)

      ortho_list[[v]] <- build_plot(conmat=conmat, data=data, background=background, node.size=node.size, view=view,
                                    node.color=node.color, thr=NULL, uthr=NULL,
                                    edge.color=edge.color,edge.alpha=edge.alpha,
                                    edge.width=edge.width,  scale.edge.width=scale.edge.width,
                                    show.legend=show.legend, labels=labels, label.size=label.size,
                                    include.vec=include.vec, edge.color.weighted=edge.color.weighted, label.edge.weight=label.edge.weight)

      #  source("functions/build_plot_int.R")
      #if(interactive==TRUE){p <- build_plot_int(conmat, data,  background, node.size=node.size, view,
      #                                             node.color=node.color, thr=NULL, uthr=NULL,
      #                                             edge.color=edge.color,edge.alpha=edge.alpha,
      #                                             edge.width=edge.width,  scale.edge.width=scale.edge.width,
      #                                             show.legend=show.legend, labels=labels, label.size=label.size,
      #                                             include.vec=include.vec, view=view, edge.color.weighted=edge.color.weighted)}

      if(is.environment(edge.color) & edge.color.weighted == T) {
        ortho_list[[v]] <- ortho_list[[v]] + edge.color
      }
    }

    right_col <- plot_grid(ortho_list[[2]],
                           ortho_list[[3]],
                           nrow=2,
                           rel_heights = c(1, 1.45))
    p <- plot_grid(ortho_list[[1]], right_col, rel_widths = c(1.8,1.2))
    return(p)


  }

  # If not ortho, then do the below:

  bg <- paste0("ICBM152_", view)
  m <- get(bg)
  #if(any(grepl(background, list.backgroud, fixed=TRUE))) {
  #  m <- png::readPNG(paste0("data/background/", background,"_", view,".png"))
  #
  w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * background.alpha), nrow=dim(m)[1])
  background <- rasterGrob(w)
  #} else {stop(paste('please select a valid background: ', as.character(list.backgroud)))
  #}

  #if no conmat is provided, build nparc x  nparc empty one
  nparc <- dim(data)[1]
  if (!exists("conmat")){conmat <- matrix(0L, nrow=nparc, ncol=nparc)
  }

  #convert conmat to matrix
  conmat <- as.matrix(conmat)


  #Remove nodes with no edges
  rownames(conmat) <- colnames(conmat) #this needs to be same same if is.sym to work
  ifelse(isSymmetric.matrix(conmat)==TRUE,
         directed <- FALSE,
         directed <- TRUE)



  if(all.nodes == FALSE && directed == FALSE) {
    include.vec <- vector(length=dim(data)[1])
    for (i in 1:dim(conmat)[1]){
      ifelse(any(conmat[i, ] != 0), include.vec[i] <- 1, include.vec[i] <- 0)
    }
    data <- data[as.logical(include.vec), ,drop=F]
    conmat <- conmat[which(rowSums(conmat, na.rm = T) != 0), which(colSums(conmat, na.rm = T) != 0), drop = F]
  }


  if(all.nodes==FALSE && directed == TRUE) {
    include.vec <- vector(length=dim(data)[1])
    for (i in 1:dim(conmat)[1]){
      ifelse(any(conmat[i, ] != 0) | any(conmat[, i] != 0), include.vec[i] <- 1, include.vec[i] <- 0)
    }
  }

  if(all.nodes==TRUE) {
    include.vec <- vector(length=dim(data)[1])
    include.vec <- rep(1, length=dim(data)[1])
  }


  #if interactive call build_plot_int, else call build con
  #  source("functions/build_plot.R")
  p <- build_plot(conmat=conmat, data=data, background=background, node.size=node.size, view=view,
                  node.color=node.color, thr=NULL, uthr=NULL,
                  edge.color=edge.color,edge.alpha=edge.alpha,
                  edge.width=edge.width,  scale.edge.width=scale.edge.width,
                  show.legend=show.legend, labels=labels, label.size=label.size,
                  include.vec=include.vec, edge.color.weighted=edge.color.weighted, label.edge.weight=label.edge.weight)

  if(is.environment(edge.color) & edge.color.weighted == T) {
    p <- p + edge.color
  }

  #  source("functions/build_plot_int.R")
  #if(interactive==TRUE){p <- build_plot_int(conmat, data, background, node.size=node.size, view,
  #                                             node.color=node.color, thr=NULL, uthr=NULL,
  #                                             edge.color=edge.color,edge.alpha=edge.alpha,
  #                                             edge.width=edge.width,  scale.edge.width=scale.edge.width,
  #                                             show.legend=show.legend, labels=labels, label.size=label.size,
  #                                             include.vec=include.vec, view=view, edge.color.weighted=edge.color.weighted)}
  return(p)




}



































