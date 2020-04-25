#' Title
#'
#' @param atlas
#' @param conmat
#' @param data
#' @param all.nodes
#' @param node.color
#' @param node.size
#' @param edge.width
#' @param edge.color
#' @param opacity
#' @param d.factor
#' @param show.legend
#' @param thr
#' @param uthr
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
brainconn3D <- function(atlas=NULL,
                           conmat=NULL,
                           data,
                           all.nodes=FALSE,
                           node.color="network",
                           node.size=10,
                           edge.width=3,
                           edge.color="black",
                           opacity=0.28,
                           d.factor=1.05,
                           show.legend=T,
                           thr=NULL,
                           uthr=NULL,
                           ...) {

  list.atlas <- sub('\\.csv$', '', list.files(path="data/atlases/", pattern = "*.csv"))
  if(any(grepl(atlas, list.atlas, fixed=TRUE))) {data <- read.csv(paste0('data/atlases/', atlas, '.csv'))
  } else{stop(paste(paste('please select a valid atlas: '), paste(list.atlas, " ", collapse="")))
  }
#convert conmat to matrix
# conmat <- as.matrix(conmat)


  #if no conmat is provided, build nparc x  nparc empty one
  nparc <- dim(data)[1]
  if (!exists("conmat")){conmat <- matrix(0L, nrow=nparc, ncol=nparc)
  }


  #make sure the col and row names of the supplied conmat are indexed correctly
  rownames(conmat) <- colnames(conmat) <- 1:length(conmat)



  #Remove nodes with no edges
  if(all.nodes==FALSE) {
    include.vec <- vector(length=dim(data)[1])
    for (i in 1:dim(x)[1]){
      ifelse(any(x[i, ] > 0), include.vec[i] <- 1, include.vec[i] <- 0)
    }
    data <- data[as.logical(include.vec), ,drop=F]
    conmat <- conmat[which(rowSums(conmat) > 0), which(colSums(conmat) > 0), drop = F]
  }

  if (!exists("conmat")) stop(print("Please enter a valid connectivity matrix"))
  if (!is.null(thr)) {conmat[conmat < thr] <- 0} #lower threshold graph
  if (!is.null(uthr)) {conmat[conmat > thr] <- 0} #upper threshold graph


  temp <- read.table("data/background/ICBM152_mesh")
  temp.v <- subset(temp, V1 == "v")
  temp.f <- subset(temp, V1 == "f")
  temp.v <- temp.v[,-1]
  temp.f <- temp.f[,-1]
  vb_mat <- t(as.matrix(temp.v))
  vb_mat <- rbind(vb_mat, 1)
  rownames(vb_mat) <- c("xpts", "ypts", "zpts", "")
  it_mat <- t(as.matrix(temp.f))
  rownames(it_mat) <- NULL

  vertices <- c(vb_mat)
  indices <- c(it_mat)

  mesh <- rgl::tmesh3d(vertices = vertices,
                  indices = indices,
                  homogeneous = TRUE,
                  material = NULL,
                  normals = NULL,
                  texcoords = NULL)
  x <- mesh$vb[1,]
  y <- mesh$vb[2,]
  z <- mesh$vb[3,]
  m <- matrix(c(x,y,z), ncol=3, dimnames=list(NULL,c("x","y","z")))
  zmean <- apply(t(mesh$it), MARGIN=1, function(row){mean(m[row,3])})


  facecolor = scales::colour_ramp(
    scales::brewer_pal(palette="Greys")(1)
  )(scales::rescale(x=zmean))

  p1 <- plotly::plot_ly(
    x = x, y = y, z = z,
    i = mesh$it[1,]-1, j = mesh$it[2,]-1, k = mesh$it[3,]-1,
    facecolor = facecolor,
    type = "mesh3d",
    opacity = opacity)

  p1 <- plotly::layout(p1, scene = list(xaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE),
                                yaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE),
                                zaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE)))

  ##make edgeplot to overlay
  g <- igraph::graph.adjacency(as.matrix(conmat))
  edge.list <- igraph::get.edgelist(g)

  size <- rep(0.1,length(data$index))

ifelse(node.color=="network", node.color <- data$network, node.color <- node.color)

 if (node.color==data$network) {
   p <- plotly::plot_ly(data, marker = list(size = node.size),
              x = data$x.mni*d.factor,
              y = data$y.mni*d.factor,
              z = data$z.mni*d.factor,
              color= ~node.color,
              name = data$ROI.Name,
              type = "scatter3d") } else {
                p <- plotly::plot_ly(data, x = data$x.mni*d.factor,
                                     y = data$y.mni*d.factor,
                                     z = data$z.mni*d.factor,
                                     color = node.color,
                                     colors = node.color,
                                     marker = list(size = node.size))
                p <- plotly::add_markers(p)

              }





    ifelse(show.legend==T, p <- plotly::layout(p, showlegend = TRUE), p <- plotly::layout(p, showlegend = FALSE))


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
    p <- plotly::add_trace(p, x = xx*d.factor,
                           y = yy*d.factor,
                           z = zz*d.factor,
                           type = "scatter3d",
                           mode = 'lines',
                           line = list(width = edge.width, color= edge.color),
                           inherit = F, showlegend = FALSE)

  }

  #https://plot.ly/r/reference/#scattermapbox-below
  p1p <- plotly::subplot(p1, p)

  # remove axis
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  plotly::layout(p1p,
         scene = list(xaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE),
                      yaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE),
                      zaxis = list(title = '', autorange = TRUE, showgrid = FALSE, zeroline = FALSE, showline = FALSE, autotick = TRUE, ticks = '', showticklabels = FALSE))
  )
  print(p1p)
}
