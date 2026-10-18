#' Plots brains
#'
#' \code{brainconn} plots and returns a ggraph object of plotted brain connectivity matrix.
#'
#' Each view shows the brain as seen from that side. In the \code{"top"} and
#' \code{"back"} views the subject's left hemisphere is on the left of the
#' plot; in the \code{"bottom"} and \code{"front"} views it is on the right.
#' Set \code{hemi.labels = TRUE} (the default) to mark the hemispheres with
#' "L" and "R". In the \code{"left"} and \code{"right"} views, nodes in the
#' hemisphere nearest the viewer are drawn on top.
#'
#' @author Sidhant Chopra
#'
#' @param atlas Either a string of one of the included atlases \code{brainconn::list_atlases()} or a \code{data.frame()} that meets specifications, see \code{vignette("brainconn")}
#' @param background 'ICBM152' (the default), or the path to a custom background image (RGB or RGBA). A custom background cannot be used with \code{view = "ortho"}.
#' @param background.alpha Number between 0-1 to  set the transparency of the background.
#' @param view A string to choose the view. One of \code{c("top", "bottom", "left", "right", "front", "back", "ortho")}. \code{"ortho"} combines the top, left and front views.
#' @param conmat A adjacency matrix. Can be binary, weights, directed or undirected. see example_* data. If \code{NULL}, only the nodes are plotted.
#' @param node.size A number that determines the diameter of the nodes. Can also be a vector with one value per ROI in the atlas, or one value per plotted node.
#' @param node.color A string that sets the node color. e.g. "blue". If set to "network", then nodes will be colored according to the network column of the atlas
#' @param edge.color.weighted A boolean that applies when the conmat is weighted. if \code{TRUE}, then edges will be colored according to the weight \code{FALSE}, the edges will be sized according to weight.
#' @param all.nodes if \code{TRUE}, then all nodes will be shown. If \code{FALSE}, then only nodes with connecting edges will be shown.
#' @param edge.color A string that sets the edge color. e.g. "blue". Can also be a ggraph edge colour scale, e.g. \code{scale_edge_color_gradient(low = "blue", high = "red")}, for use with \code{edge.color.weighted = TRUE}.
#' @param edge.alpha Number between 0-1 to  set the transparency of the edges.
#' @param edge.width Number to set the width of the edges.
#' @param labels if \code{TRUE}, ROI labels for all visible nodes will be shown. If \code{FALSE}, then no labels will be shown.
#' @param show.legend if \code{TRUE}, legend will be shown. If \code{FALSE}, then no legend will be shown.
#' @param thr a optional value to set a threshold on the conmat (e.g. edges with a weighted value lower than the one set here will not be shown)
#' @param uthr a optional value to set a upper threshold on the conmat (e.g. edges with a weighted value higher than the one set here will not be shown)
#' @param scale.edge.width If \code{edge.color.weighted=FALSE}, you can use this rescale the edge.width according to weight. e.g. \code{scale.edge.width = c(1,3)}
#' @param label.size If labels=TRUE then, \code{label.size} can can be set as in integer to control the size of the labels.
#' @param label.edge.weight if \code{TRUE}, then the edge weight will be labels along the edge.
#' @param hemi.labels if \code{TRUE}, "L" and "R" are drawn on the top, bottom, front and back views to mark the left and right hemispheres.
#' @param bg_xmin,bg_xmax,bg_ymin,bg_ymax Offsets (in mm) to nudge the edges of the background image, e.g. to line up a custom background with the nodes.
#'
#' @return a ggraph object
#'
#' @import ggraph
#' @import cowplot
#' @import grid
#' @importFrom OpenImageR readImage
#' @importFrom grDevices rgb
#' @examples
#' library(brainconn)
#' x <- example_unweighted_undirected
#' brainconn(atlas ="schaefer300_n7", conmat=x, node.size = 3, view="ortho")
#' @export
brainconn <- function(atlas,
                      background = "ICBM152",
                      view = "top",
                      conmat = NULL,
                      node.size = 4,
                      node.color = "network",
                      all.nodes = FALSE,
                      edge.color = "black",
                      edge.alpha = 0.8,
                      edge.width = 1,
                      edge.color.weighted = FALSE,
                      labels = FALSE,
                      show.legend = TRUE,
                      thr = NULL,
                      uthr = NULL,
                      scale.edge.width = NULL,
                      label.size = 1.5,
                      label.edge.weight = FALSE,
                      background.alpha = 1,
                      bg_xmax = 0,
                      bg_xmin = 0,
                      bg_ymax = 0,
                      bg_ymin = 0,
                      hemi.labels = TRUE) {

  view <- match.arg(view, c("top", "bottom", "left", "right", "front", "back", "ortho"))
  if (!identical(background, "ICBM152") && view == "ortho") {
    stop("Custom background image detected, view cannot be 'ortho', please select top, ",
         "bottom, left, right, front or back.", call. = FALSE)
  }

  data <- resolve_atlas(atlas, parent.frame())
  conmat <- prepare_conmat(conmat, nrow(data), thr, uthr)

  keep <- nodes_to_keep(conmat, all.nodes)
  node.size <- match_node_size(node.size, keep)
  data <- data[keep, , drop = FALSE]
  conmat <- conmat[keep, keep, drop = FALSE]
  node.color <- resolve_node_color(node.color, data)

  plot_view <- function(v, show.legend) {
    build_plot(conmat = conmat,
               data = data,
               background = background_grob(background, v, background.alpha),
               view = v,
               node.size = node.size,
               node.color = node.color,
               edge.color = edge.color,
               edge.alpha = edge.alpha,
               edge.width = edge.width,
               edge.color.weighted = edge.color.weighted,
               scale.edge.width = scale.edge.width,
               show.legend = show.legend,
               labels = labels,
               label.size = label.size,
               label.edge.weight = label.edge.weight,
               hemi.labels = hemi.labels,
               bg_xmin = bg_xmin,
               bg_xmax = bg_xmax,
               bg_ymin = bg_ymin,
               bg_ymax = bg_ymax)
  }

  if (view != "ortho") {
    return(plot_view(view, show.legend))
  }

  # ortho: top view on the left, left and front views stacked on the right.
  # Only the top view gets a legend, to avoid repeating it.
  top <- plot_view("top", show.legend)
  right_col <- plot_grid(plot_view("left", FALSE),
                         plot_view("front", FALSE),
                         nrow = 2,
                         rel_heights = c(1, 1.45))
  plot_grid(top, right_col, rel_widths = c(1.8, 1.2))
}
