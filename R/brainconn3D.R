#' Plots brains in 3D
#'
#' \code{brainconn} plots and returns a interactive plotly object of plotted brain connectivity matrix..
#' @author Sidhant Chopra
#'
#'
#' @param atlas Either a string of one of the included atlases \code{brainconn::list_atlases() or a .data.frame that meets specifications, see \code{vignette("brainconn")}
#' @param conmat A adjacency matrix. Has ti be binary and undirected (i.e. symetric). see example_* data.
#' @param all.nodes f \code{TRUE}, then all nodes will be shown be hemisphere without ticks. If \code{FALSE}, then only nodes with connecting edges will be shown.
#' @param node.color A string that sets the node color. e.g. "blue". If set to "network", then nodes will be colored according to the network column of the atlas
#' @param node.size A integer that determines the diameter of the nodes. Can also be a vector of integers with a length equal to the number of ROIs in the atlas
#' @param edge.width Number to set the width of the edges.
#' @param edge.color A string that sets the edge color. e.g. "blue".
#' @param opacity Number between 0-1 to  set the transparency of the mesh brain
#' @param d.factor An int to change the distance between the nodes and brain surface.
#' @param show.legend if \code{TRUE}, legend will be shown. If \code{FALSE}, then no legend will be shown
#' @param thr a optional value to set a threshold on the conmat (e.g. edges with a weighted value lower than the one set here will not be shown)
#' @param uthr a optional value to set a upper threshold on the conmat (e.g. edges with a weighted value higher than the one set here will not be shown)
#' @return a plotly object
#' @import scales
#' @rawNamespace import(plotly, except = last_plot)
#' @rawNamespace import(igraph, except = groups)
#' @examples
#' \dontrun{
#' library(brainconn)
#' brainconn3D(atlas ="schaefer300_n7", conmat=example_unweighted_undirected, show.legend = F)
#' }
#' @export

brainconn3D <- function(atlas=NULL,
                           conmat=NULL,
                           all.nodes=FALSE,
                           node.color="network",
                           node.size=10,
                           edge.width=3,
                           edge.color="black",
                           opacity=0.28,
                           d.factor=1.05,
                           show.legend=F,
                           thr=NULL,
                           uthr=NULL) {

  #list.atlas <- sub('\\.rda$', '', list.files(pattern = "*.rda"))
  #if(any(grepl(atlas, list.atlas, fixed=TRUE))) {data <- get(atlas)} else
  #  {stop(paste(paste('please select a valid atlas: '), paste(list.atlas, " ", collapse="")))
  #}
  ifelse(is.character(atlas), data <- get(atlas), data <- atlas)

#convert conmat to matrix
# conmat <- as.matrix(conmat)

  #if no conmat is provided, build nparc x  nparc empty one
  nparc <- dim(data)[1]
  if (!exists("conmat")){conmat <- matrix(0L, nrow=nparc, ncol=nparc)
  }
  #convert conmat to matrix
  conmat <- as.matrix(conmat)


  #make sure the col and row names of the supplied conmat are indexed correctly
  rownames(conmat) <- colnames(conmat) <- 1:dim(conmat)[1]

  #Remove nodes with no edges
  if(all.nodes==FALSE) {
    include.vec <- vector(length=dim(data)[1])
    for (i in 1:dim(conmat)[1]){
      ifelse(any(conmat[i, ] > 0), include.vec[i] <- 1, include.vec[i] <- 0)
    }
    data <- data[as.logical(include.vec), ,drop=F]
    conmat <- conmat[which(rowSums(conmat) > 0), which(colSums(conmat) > 0), drop = F]
  }

  if (!exists("conmat")) stop(print("Please enter a valid connectivity matrix"))
  if (!is.null(thr)) {conmat[conmat < thr] <- 0} #lower threshold graph
  if (!is.null(uthr)) {conmat[conmat > thr] <- 0} #upper threshold graph

#set up mesh
  vb <- get("ICBM152_mesh_vb")
  it <- get("ICBM152_mesh_it")

  x <- vb[1,]
  y <- vb[2,]
  z <- vb[3,]
  m <- matrix(c(x,y,z), ncol=3, dimnames=list(NULL,c("x","y","z")))
  zmean <- apply(t(it), MARGIN=1, function(row){mean(m[row,3])})


  facecolor = colour_ramp(
   brewer_pal(palette="Greys")(1)
  )(rescale(x=zmean))

  p1 <- plot_ly(
    x = x, y = y, z = z,
    i = it[1,]-1, j = it[2,]-1, k = it[3,]-1,
    facecolor = facecolor,
    type = "mesh3d",
    opacity = opacity)

  p1 <- layout(p1, scene = list(xaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE),
                                yaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE),
                                zaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE)))

  ##make edgeplot to overlay
  g <- graph.adjacency(as.matrix(conmat))
  edge.list <- get.edgelist(g)

  size <- rep(0.1,length(data$index))

ifelse(node.color=="network", node.color <- as.factor(data$network), node.color <- node.color)

 if (!is.character(node.color)) {
   p <- plot_ly(data, marker = list(size = node.size),
              x = data$x.mni*d.factor,
              y = data$y.mni*d.factor,
              z = data$z.mni*d.factor,
              color= ~node.color,
              name = data$ROI.Name,
              type = "scatter3d") } else {
                p <- plot_ly(data, x = data$x.mni*d.factor,
                                     y = data$y.mni*d.factor,
                                     z = data$z.mni*d.factor,
                                     color = node.color,
                                     colors = node.color,
                                     marker = list(size = node.size))
                p <- add_markers(p)

              }





    ifelse(show.legend==T, p <- layout(p, showlegend = TRUE), p <- layout(p, showlegend = FALSE))


  elength = length(edge.list)/2
  xx <- yy <- zz <- vector()
  for (e in 1:elength) {
    from <- data$x.mni[which(data$index == edge.list[e,1])]
    to <- data$x.mni[which(data$index == edge.list[e,2])]
    xx[1:length(data$x.mni)] <- NA
    xx[1:2] <- c(from, to)

    from <- data$y.mni[which(data$index == edge.list[e,1])]
    to <- data$y.mni[which(data$index == edge.list[e,2])]
    yy[1:length(data$y.mni)] <- NA
    yy[1:2] <- c(from, to)

    from <- data$z.mni[which(data$index == edge.list[e,1])]
    to <- data$z.mni[which(data$index == edge.list[e,2])]
    zz[1:length(data$z.mni)] <- NA
    zz[1:2] <- c(from, to)


    # p <- add_trace(p, x = xx*d.factor,
    #                y = yy*d.factor,
    #                z = zz*d.factor,
    #                type = "scatter3d",
    #                mode = "lines",
    #                line = list(width = "5", color = "black"),
    #                name = paste0(data$ROI.Name[edge.list[e,1]], "to",data$ROI.Name[edge.list[e,2]], sep=" "),
    #                showlegend = FALSE,
    #                inherit = F)
    p <- add_trace(p, x = xx*d.factor,
                           y = yy*d.factor,
                           z = zz*d.factor,
                           type = "scatter3d",
                           mode = 'lines',
                           line = list(width = edge.width, color= edge.color),
                           inherit = F, showlegend = FALSE)

  }

  #https://plot.ly/r/reference/#scattermapbox-below
  p1p <- subplot(p1, p)

  # remove axis
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  layout(p1p,
         scene = list(xaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE),
                      yaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE),
                      zaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE))
  )
  print(p1p)
}
