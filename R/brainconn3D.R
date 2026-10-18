#' Plots brains in 3D
#'
#' \code{brainconn3D} plots and returns a interactive plotly object of plotted brain connectivity matrix.
#'
#' Edges are drawn as straight lines between nodes. Edge weights and
#' directions are not shown; any non-zero value in \code{conmat} is drawn as
#' an edge.
#' @author Sidhant Chopra
#'
#'
#' @param atlas Either a string of one of the included atlases \code{brainconn::list_atlases()} or a \code{data.frame()} that meets specifications, see \code{vignette("brainconn")}
#' @param conmat A adjacency matrix. Can be binary, weighted, directed or undirected, see example_* data. If \code{NULL}, only the nodes are plotted.
#' @param all.nodes If \code{TRUE}, then all nodes will be shown. If \code{FALSE}, then only nodes with connecting edges will be shown.
#' @param node.color A string that sets the node color. e.g. "blue". If set to "network", then nodes will be colored according to the network column of the atlas
#' @param node.size A number that determines the diameter of the nodes. Can also be a vector with one value per ROI in the atlas, or one value per plotted node.
#' @param edge.width Number to set the width of the edges.
#' @param edge.color A string that sets the edge color. e.g. "blue".
#' @param opacity Number between 0-1 to  set the transparency of the mesh brain
#' @param d.factor A number to change the distance between the nodes and brain surface.
#' @param show.legend if \code{TRUE}, legend will be shown. If \code{FALSE}, then no legend will be shown
#' @param thr a optional value to set a threshold on the conmat (e.g. edges with a weighted value lower than the one set here will not be shown)
#' @param uthr a optional value to set a upper threshold on the conmat (e.g. edges with a weighted value higher than the one set here will not be shown)
#' @return a plotly object
#' @importFrom plotly plot_ly add_trace layout
#' @examples
#' \dontrun{
#' library(brainconn)
#' brainconn3D(atlas ="schaefer300_n7", conmat=example_unweighted_undirected, show.legend = FALSE)
#' }
#' @export

brainconn3D <- function(atlas = NULL,
                        conmat = NULL,
                        all.nodes = FALSE,
                        node.color = "network",
                        node.size = 10,
                        edge.width = 3,
                        edge.color = "black",
                        opacity = 0.28,
                        d.factor = 1.05,
                        show.legend = FALSE,
                        thr = NULL,
                        uthr = NULL) {

  data <- resolve_atlas(atlas, parent.frame())
  conmat <- prepare_conmat(conmat, nrow(data), thr, uthr)

  keep <- nodes_to_keep(conmat, all.nodes)
  node.size <- match_node_size(node.size, keep)
  data <- data[keep, , drop = FALSE]
  conmat <- conmat[keep, keep, drop = FALSE]
  node.color <- resolve_node_color(node.color, data)

  x <- data$x.mni * d.factor
  y <- data$y.mni * d.factor
  z <- data$z.mni * d.factor

  # Brain surface mesh
  vb <- package_data("ICBM152_mesh_vb")
  it <- package_data("ICBM152_mesh_it")
  p <- plot_ly()
  p <- add_trace(p,
                 type = "mesh3d",
                 x = vb[1, ], y = vb[2, ], z = vb[3, ],
                 i = it[1, ] - 1, j = it[2, ] - 1, k = it[3, ] - 1,
                 facecolor = rep("#F0F0F0", ncol(it)),
                 opacity = opacity,
                 hoverinfo = "skip",
                 showlegend = FALSE)

  # Edges: one trace, with NA between edges to break the line
  if (isSymmetric(conmat)) {
    edges <- which(conmat != 0 & upper.tri(conmat), arr.ind = TRUE)
  } else {
    edges <- which(conmat != 0 & row(conmat) != col(conmat), arr.ind = TRUE)
  }
  if (nrow(edges) > 0) {
    path <- function(v) as.vector(rbind(v[edges[, 1]], v[edges[, 2]], NA))
    p <- add_trace(p,
                   type = "scatter3d",
                   mode = "lines",
                   x = path(x), y = path(y), z = path(z),
                   line = list(width = edge.width, color = edge.color),
                   hoverinfo = "skip",
                   showlegend = FALSE)
  }

  # Nodes
  if (identical(node.color, "network")) {
    p <- add_trace(p,
                   type = "scatter3d",
                   mode = "markers",
                   x = x, y = y, z = z,
                   color = as.factor(data$network),
                   marker = list(size = node.size),
                   text = data$ROI.Name,
                   hoverinfo = "text")
  } else {
    p <- add_trace(p,
                   type = "scatter3d",
                   mode = "markers",
                   x = x, y = y, z = z,
                   marker = list(size = node.size, color = node.color),
                   text = data$ROI.Name,
                   hoverinfo = "text",
                   showlegend = FALSE)
  }

  hidden_axis <- list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE,
                      ticks = "", showticklabels = FALSE, showspikes = FALSE)
  layout(p,
         showlegend = show.legend,
         scene = list(xaxis = hidden_axis,
                      yaxis = hidden_axis,
                      zaxis = hidden_axis,
                      aspectmode = "data"))
}
