
#' Title
#'
#' @param atlas
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
#' @param ...
#'
#' @return
#' @export
#' @impot ggraph
#' @examples
brainconn <- function(atlas,
                         background='ICBM152',
                         view ="top",
                         conmat=NULL,
                         interactive = F,
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
                        background.alpha = 1,
                         ...) {

  load_object <- function(file) {
    tmp <- new.env()
    load(file = file, envir = tmp)
    tmp[[ls(tmp)[1]]]
  }

  list.atlas <- sub('\\.rda$', '', list.files(path="data/", pattern = "*.rda"))
  if(any(grepl(atlas, list.atlas, fixed=TRUE))) {data <- load_object(paste0('data/', atlas, '.rda'))} else
    {stop(paste(paste('please select a valid atlas: '), paste(list.atlas, " ", collapse="")))
  }

  #set background
  list.backgroud <- sub('\\.png$', '', list.files(path="data/background/", pattern = "*.png"))

 # if(any(grepl(background, list.backgroud, fixed=TRUE))) {background <-
 #   grid::rasterGrob(png::readPNG(paste0("data/background/", background,"_", view,".png")))
 # } else {stop(paste('please select a valid background: ', as.character(list.backgroud)))
 # }

  if(any(grepl(background, list.backgroud, fixed=TRUE))) {
    m <- png::readPNG(paste0("data/background/", background,"_", view,".png"))
    w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * background.alpha), nrow=dim(m)[1])
    background <- grid::rasterGrob(w)
  } else {stop(paste('please select a valid background: ', as.character(list.backgroud)))
  }

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
p <- build_plot(conmat, data, data.row, data.col, background, node.size=node.size, view,
                                          node.color=node.color, thr=NULL, uthr=NULL,
                                          edge.color=edge.color,edge.alpha=edge.alpha,
                                          edge.width=edge.width,  scale.edge.width=scale.edge.width,
                                          show.legend=show.legend, labels=labels, label.size=label.size,
                                          include.vec=include.vec, edge.color.weighted=edge.color.weighted)

#  source("functions/build_plot_int.R")
#if(interactive==TRUE){p <- build_plot_int(conmat, data, data.row, data.col, background, node.size=node.size, view,
#                                             node.color=node.color, thr=NULL, uthr=NULL,
#                                             edge.color=edge.color,edge.alpha=edge.alpha,
#                                             edge.width=edge.width,  scale.edge.width=scale.edge.width,
#                                             show.legend=show.legend, labels=labels, label.size=label.size,
#                                             include.vec=include.vec, view=view, edge.color.weighted=edge.color.weighted)}
return(p)
}
