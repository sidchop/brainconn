# Geometry of each 2D view. Each view is a camera looking at the brain from
# that side, so e.g. in the front view the subject's left hemisphere is on the
# viewer's right, and in the top view it is on the viewer's left.
#   x, y, depth: MNI axis plotted on each screen axis (depth points towards
#                the viewer, so nodes nearer the viewer are drawn on top)
#   sign:        multiplier for x, y and depth
#   bg:          extent of the background image (xmin, xmax, ymin, ymax)
#   xlim, ylim:  plot limits
#   left_x:      screen x of the "L" hemisphere label (NA = no label)
#   label_y:     screen y of the hemisphere labels
view_geometry <- list(
  top    = list(x = "x.mni", y = "y.mni", depth = "z.mni", sign = c( 1, 1,  1),
                bg = c(-75, 70, -107, 73), xlim = c(-70, 70), ylim = c(-107, 73),
                left_x = -65, label_y = 65),
  bottom = list(x = "x.mni", y = "y.mni", depth = "z.mni", sign = c(-1, 1, -1),
                bg = c(-70, 70, -107, 73), xlim = c(-70, 70), ylim = c(-107, 73),
                left_x = 65, label_y = 65),
  front  = list(x = "x.mni", y = "z.mni", depth = "y.mni", sign = c(-1, 1,  1),
                bg = c(-70, 70, -48, 80), xlim = c(-70, 70), ylim = c(-107, 73),
                left_x = 65, label_y = 75),
  back   = list(x = "x.mni", y = "z.mni", depth = "y.mni", sign = c( 1, 1, -1),
                bg = c(-70, 70, -48, 80), xlim = c(-70, 70), ylim = c(-107, 73),
                left_x = -65, label_y = 75),
  left   = list(x = "y.mni", y = "z.mni", depth = "x.mni", sign = c(-1, 1, -1),
                bg = c(-72, 103, -50, 77), xlim = c(-64, 98), ylim = c(-44, 76),
                left_x = NA, label_y = NA),
  right  = list(x = "y.mni", y = "z.mni", depth = "x.mni", sign = c( 1, 1,  1),
                bg = c(-103, 72, -50, 77), xlim = c(-98, 64), ylim = c(-44, 76),
                left_x = NA, label_y = NA)
)

# Screen coordinates (x, y, depth) of each atlas region in a given view.
view_coords <- function(data, view) {
  g <- view_geometry[[view]]
  data.frame(x     = data[[g$x]]     * g$sign[1],
             y     = data[[g$y]]     * g$sign[2],
             depth = data[[g$depth]] * g$sign[3])
}

#' workhorse function for \code{brainconn()}
#'
#' Returns a ggraph object of a plotted brain connectivity matrix for a
#' single view. \code{conmat}, \code{data} and \code{node.size} must already
#' be restricted to the nodes being plotted.
#' @author Sidhant Chopra
#' @import ggraph
#' @import ggplot2
#' @import grid
#' @importFrom igraph graph_from_adjacency_matrix set_vertex_attr
#' @keywords internal
build_plot <- function(conmat,
                       data,
                       background,
                       view,
                       node.size,
                       node.color = "network",
                       edge.color = "black",
                       edge.alpha = 0.8,
                       edge.width = 1,
                       edge.color.weighted = FALSE,
                       scale.edge.width = NULL,
                       show.legend = TRUE,
                       labels = FALSE,
                       label.size = 1.5,
                       label.edge.weight = FALSE,
                       hemi.labels = TRUE,
                       bg_xmin = 0,
                       bg_xmax = 0,
                       bg_ymin = 0,
                       bg_ymax = 0) {

  geom <- view_geometry[[view]]
  coords <- view_coords(data, view)

  # Draw nodes furthest from the viewer first, so the near hemisphere is on top
  ord <- order(coords$depth)
  coords <- coords[ord, , drop = FALSE]
  data <- data[ord, , drop = FALSE]
  conmat <- conmat[ord, ord, drop = FALSE]
  if (length(node.size) > 1) node.size <- node.size[ord]

  directed <- !isSymmetric(conmat)
  weighted <- !all(conmat %in% c(0, 1))

  g <- graph_from_adjacency_matrix(conmat,
                                   mode = if (directed) "directed" else "undirected",
                                   weighted = if (weighted) TRUE else NULL,
                                   diag = TRUE)
  g <- set_vertex_attr(g, "ROI.Name", value = as.character(data$ROI.Name))
  if (identical(node.color, "network")) {
    g <- set_vertex_attr(g, "network", value = as.factor(data$network))
  }
  layout <- create_layout(g, layout = "manual", x = coords$x, y = coords$y)

  # A ggplot2 scale can be passed as edge.color, e.g. scale_edge_color_gradient()
  edge.scale <- NULL
  if (inherits(edge.color, "ggproto")) {
    edge.scale <- edge.color
    edge.color <- "black"
  }

  # Edge aesthetics: weight maps to colour or width, or neither if unweighted
  edge.mapping <- list()
  edge.params <- list(edge_alpha = edge.alpha)
  if (weighted && edge.color.weighted) {
    edge.mapping$edge_colour <- quote(weight)
  } else {
    edge.params$edge_colour <- edge.color
  }
  if (weighted && !edge.color.weighted) {
    edge.mapping$edge_width <- quote(weight)
  } else {
    edge.params$edge_width <- edge.width
  }
  label.params <- list()
  if (weighted && label.edge.weight) {
    edge.mapping$label <- quote(round(weight, 3))
    label.params <- list(label_size = 2, fontface = "bold")
  }

  p <- ggraph(layout) +
    annotation_custom(background,
                      xmin = geom$bg[1] + bg_xmin, xmax = geom$bg[2] + bg_xmax,
                      ymin = geom$bg[3] + bg_ymin, ymax = geom$bg[4] + bg_ymax)

  if (directed) {
    arrow.head <- arrow(length = unit(3, "mm"))
    p <- p + do.call(geom_edge_parallel,
                     c(list(mapping = do.call(aes, edge.mapping),
                            arrow = arrow.head,
                            end_cap = circle(max(node.size) / 2 + 0.6, "mm")),
                       edge.params, label.params,
                       if (length(label.params)) list(angle_calc = "along",
                                                      label_dodge = unit(2.5, "mm"))))
    if (any(diag(conmat) != 0)) {
      p <- p + do.call(geom_edge_loop,
                       c(list(mapping = do.call(aes, edge.mapping),
                              arrow = arrow.head,
                              strength = max(node.size) * 3),
                         edge.params, label.params,
                         if (length(label.params)) list(angle_calc = "none",
                                                        label_dodge = unit(6, "mm"),
                                                        vjust = -1)))
    }
  } else {
    p <- p + do.call(geom_edge_link,
                     c(list(mapping = do.call(aes, edge.mapping)),
                       edge.params, label.params,
                       if (length(label.params)) list(angle_calc = "along",
                                                      label_dodge = unit(2.5, "mm"))))
  }

  if (!is.null(edge.scale)) p <- p + edge.scale
  if (weighted && !edge.color.weighted && !is.null(scale.edge.width)) {
    p <- p + scale_edge_width(range = scale.edge.width)
  }

  if (identical(node.color, "network")) {
    p <- p + geom_node_point(aes(colour = network), size = node.size)
  } else {
    p <- p + geom_node_point(size = node.size, colour = node.color)
  }

  if (labels) {
    p <- p + geom_node_text(aes(label = ROI.Name),
                            size = label.size, repel = TRUE,
                            nudge_x = max(node.size) + 2, nudge_y = max(node.size))
  }

  if (hemi.labels && !is.na(geom$left_x)) {
    p <- p + annotate("text", x = c(geom$left_x, -geom$left_x), y = geom$label_y,
                      label = c("L", "R"), fontface = "bold", size = 4)
  }

  p <- p + coord_fixed(xlim = geom$xlim, ylim = geom$ylim) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  if (show.legend) {
    if (identical(node.color, "network")) p <- p + labs(colour = "Network")
  } else {
    p <- p + theme(legend.position = "none")
  }

  p
}

# Column names used in aes() above, declared to avoid R CMD check notes
utils::globalVariables(c("weight", "network", "ROI.Name"))
