#' Plot brain connectivity as a 3D render
#'
#' \code{brainconn} draws a connectivity matrix as a static 3D render: nodes
#' are shiny spheres, edges are tubes, and the brain is a translucent glass
#' surface, in the style of BrainNet Viewer. The result is a ggplot object, so
#' it can be saved with \code{ggsave()} and combined with other plots.
#'
#' In the \code{"top"}, \code{"bottom"}, \code{"front"} and \code{"back"}
#' views the subject's left hemisphere is always on the left of the plot, so
#' the bottom and front views are mirror images of what a camera below or in
#' front of the subject would see. \code{hemi.labels = TRUE} (the default)
#' marks the hemispheres with "L" and "R".
#'
#' Before brainconn 1.0.0, \code{brainconn()} drew 2D plots; that function is
#' now \code{\link{brainconn2d}}. Calls that use arguments only the 2D plot
#' has (\code{background}, \code{background.alpha}, \code{bg_xmin},
#' \code{bg_xmax}, \code{bg_ymin}, \code{bg_ymax}, \code{label.edge.weight},
#' or a ggraph scale as \code{edge.color}) are passed on to
#' \code{brainconn2d()} with a warning.
#'
#' @author Sidhant Chopra
#'
#' @param atlas Either a string of one of the included atlases (see \code{list_atlases()}) or a \code{data.frame()} that meets specifications, see \code{vignette("brainconn")}.
#' @param conmat An adjacency matrix. Can be binary, weighted, directed or undirected; see the example_* data. If \code{NULL}, only the nodes are plotted.
#' @param view The view to render: one of \code{"top"}, \code{"bottom"}, \code{"left"}, \code{"right"}, \code{"front"}, \code{"back"}; \code{"ortho"} (top, left and front views together); \code{"all"} (all six views); or a numeric \code{c(azimuth, elevation)} in degrees for any other angle. Azimuth 0 is in front of the subject, 90 to their right, 180 behind and -90 to their left; elevation 90 is above and -90 below.
#' @param node.size Size of the nodes (diameter in mm for a human-sized brain; sizes scale with the size of the mesh). Can also be a vector with one value per ROI in the atlas, or one value per plotted node.
#' @param node.color A colour for all nodes, e.g. "blue", or "network" to colour nodes by the network column of the atlas (see \code{\link{network_colors}}). Can also be a vector with one colour per ROI in the atlas, or per plotted node.
#' @param all.nodes If \code{TRUE}, all nodes are shown. If \code{FALSE}, only nodes with connecting edges are shown.
#' @param edge.color A colour for the edges, e.g. "black". Ignored when \code{edge.color.weighted = TRUE}.
#' @param edge.alpha Number between 0-1 to set the transparency of the edges.
#' @param edge.width Width of the edges (relative; 1 is a tube about 1 mm wide in a human-sized brain).
#' @param edge.color.weighted For weighted matrices: if \code{TRUE}, edges are coloured by weight (see \code{edge.palette}); if \code{FALSE}, edge width shows the weight.
#' @param edge.palette Colours for edge weights when \code{edge.color.weighted = TRUE}, from the lowest to the highest weight: a vector of colours, or a palette name from \code{grDevices::hcl.pals()}. The default is blue-white-red for signed weights and light-to-dark red otherwise.
#' @param edge.limits Weights mapped to the ends of \code{edge.palette}. By default symmetric around zero for signed weights, otherwise the range of the weights.
#' @param scale.edge.width For weighted matrices with \code{edge.color.weighted = FALSE}: the range of edge widths that the weights are rescaled to. Default \code{c(0.5, 2)}.
#' @param labels If \code{TRUE}, ROI labels are shown for all plotted nodes.
#' @param label.size Size of the ROI labels.
#' @param show.legend If \code{TRUE}, show legends for the networks and (when edges are coloured by weight) the edge weights.
#' @param thr An optional lower threshold: edges with weights below it are not shown.
#' @param uthr An optional upper threshold: edges with weights above it are not shown.
#' @param hemi.labels If \code{TRUE}, "L" and "R" are drawn on the top, bottom, front and back views.
#' @param mesh The brain surface: \code{"ICBM152"} (the default), \code{NULL} for no surface, or a mesh in the same space as the atlas coordinates (any species), given as mesh file path(s) (see \code{\link{read_brain_mesh}}), a mesh from \code{read_brain_mesh()}, an rgl \code{mesh3d} object or a list with \code{vertices} (n x 3) and \code{faces} (f x 3, 1-based vertex indices).
#' @param mesh.color Colour of the brain surface.
#' @param mesh.alpha Opacity of the brain surface, from 0 (invisible) to 1 (opaque).
#' @param res Size in pixels of the longest side of each rendered view.
#' @param quality Anti-aliasing level, a whole number from 1 (fastest) upwards; 2 is usually enough.
#' @param bg.color Background colour, or \code{NA} for a transparent background.
#' @param ... Arguments only used by the 2D plot (see Details); when given, the call is passed on to \code{\link{brainconn2d}}.
#'
#' @return A ggplot object.
#' @seealso \code{\link{brainconn2d}} for 2D plots and \code{\link{brainconn3D}} for interactive 3D plots.
#' @examples
#' \donttest{
#' x <- example_unweighted_undirected
#' brainconn(atlas = "schaefer300_n7", conmat = x, view = "left")
#' brainconn(atlas = "schaefer300_n7", conmat = example_weighted_undirected,
#'           edge.color.weighted = TRUE, view = "ortho")
#' }
#' @export
brainconn <- function(atlas,
                      conmat = NULL,
                      view = "top",
                      ...,
                      node.size = 4,
                      node.color = "network",
                      all.nodes = FALSE,
                      edge.color = "black",
                      edge.alpha = 0.8,
                      edge.width = 1,
                      edge.color.weighted = FALSE,
                      edge.palette = NULL,
                      edge.limits = NULL,
                      scale.edge.width = NULL,
                      labels = FALSE,
                      label.size = 2.5,
                      show.legend = TRUE,
                      thr = NULL,
                      uthr = NULL,
                      hemi.labels = TRUE,
                      mesh = "ICBM152",
                      mesh.color = "grey85",
                      mesh.alpha = 0.3,
                      res = 1000,
                      quality = 2,
                      bg.color = "white") {

  # Calls written for the old 2D brainconn() are passed on to brainconn2d()
  dots <- list(...)
  args_2d <- c("background", "background.alpha", "bg_xmin", "bg_xmax", "bg_ymin", "bg_ymax",
               "label.edge.weight")
  unknown <- setdiff(names2(dots), args_2d)
  if (length(unknown) > 0) {
    stop("Unused argument(s): ", paste(unknown[unknown != ""], collapse = ", "),
         if (any(unknown == "")) " (and unnamed arguments)", call. = FALSE)
  }
  if (length(dots) > 0 || inherits(edge.color, "ggproto")) {
    warning("brainconn() now draws 3D renders. These arguments are only used by the 2D ",
            "plot, so drawing it with brainconn2d() instead: ",
            paste(c(names(dots), if (inherits(edge.color, "ggproto")) "edge.color (a scale)"),
                  collapse = ", "),
            ". Call brainconn2d() directly to avoid this warning.", call. = FALSE)
    call <- match.call()
    call[[1]] <- brainconn2d
    args_3d_only <- c("edge.palette", "edge.limits", "mesh", "mesh.color", "mesh.alpha", "res",
                      "quality", "bg.color")
    call <- call[!names(call) %in% args_3d_only]
    return(eval(call, parent.frame()))
  }
  if (is.character(conmat)) {
    stop("`conmat` must be a matrix. brainconn() now draws 3D renders; for the 2D plot ",
         "with a background image use brainconn2d().", call. = FALSE)
  }
  if (!isTRUE(brainconn_env$told_3d)) {
    message("brainconn() now draws 3D renders (since brainconn 1.0.0). ",
            "Use brainconn2d() for the previous 2D plots. This message is shown once per session.")
    brainconn_env$told_3d <- TRUE
  }

  if (is.numeric(view)) {
    if (length(view) != 2) stop("A numeric `view` must be c(azimuth, elevation).", call. = FALSE)
  } else {
    view <- match.arg(view, c(names(camera_views), "ortho", "all"))
  }

  data <- resolve_atlas(atlas, parent.frame())
  conmat <- prepare_conmat(conmat, nrow(data), thr, uthr)
  keep <- nodes_to_keep(conmat, all.nodes)
  node.size <- match_per_node(node.size, keep, "node.size")
  node.color <- resolve_node_color(node.color, data)
  network.colors <- NULL
  if (identical(node.color, "network")) {
    # Colours and legend order come from the whole atlas, so they don't change with conmat
    network.colors <- network_colors(data$network)
    network.colors <- network.colors[names(network.colors) %in% data$network[keep]]
    node.color <- network.colors[as.character(data$network[keep])]
  } else {
    node.color <- match_per_node(node.color, keep, "node.color")
  }
  data <- data[keep, , drop = FALSE]
  conmat <- conmat[keep, keep, drop = FALSE]

  mesh <- resolve_mesh(mesh)
  xyz <- cbind(data$x.mni, data$y.mni, data$z.mni)
  extent_points <- if (is.null(mesh)) xyz else mesh$vertices
  extent <- max(apply(extent_points, 2, function(p) diff(range(p))))
  # Sizes are in mm for a human brain (about 170 mm long) and scale with the mesh
  unit <- if (is.null(mesh)) max(extent, 1) / 130 else extent / 170

  nodes <- data.frame(x = xyz[, 1], y = xyz[, 2], z = xyz[, 3],
                      color = unname(node.color), radius = node.size / 2 * unit,
                      stringsAsFactors = FALSE)
  edges <- edge_table(conmat, edge.color, edge.width, edge.color.weighted, edge.palette,
                      edge.limits, scale.edge.width, unit)

  objects <- scene_objects(mesh, mesh.color, mesh.alpha, nodes, edges$table, edge.alpha)
  frame_points <- if (is.null(mesh)) {
    rbind(xyz + max(nodes$radius), xyz - max(nodes$radius))
  } else {
    mesh$vertices
  }

  draw_view <- function(v, legend) {
    camera <- fit_camera(view_camera(v), frame_points)
    img <- render_scene(objects, camera, res = res, quality = quality, bg.color = bg.color)
    view_plot(img, camera, data, legend, labels, label.size, hemi.labels,
              network.colors, edges, bg.color)
  }

  if (identical(view, "ortho")) {
    top <- draw_view("top", show.legend)
    right_col <- plot_grid(draw_view("left", FALSE), draw_view("front", FALSE),
                           nrow = 2, rel_heights = c(1, 1.45))
    return(plot_grid(top, right_col, rel_widths = c(1.8, 1.2)))
  }
  if (identical(view, "all")) {
    panels <- lapply(c("left", "top", "right", "front", "bottom", "back"),
                     function(v) draw_view(v, FALSE))
    grid <- plot_grid(plotlist = panels, nrow = 2)
    if (!show.legend) return(grid)
    legend <- get_legend(draw_legend_only(network.colors, edges))
    if (is.null(legend)) return(grid)
    return(plot_grid(grid, legend, rel_widths = c(1, 0.15)))
  }
  draw_view(view, show.legend)
}

# names() that returns "" rather than NULL for unnamed lists
names2 <- function(x) {
  n <- names(x)
  if (is.null(n)) rep("", length(x)) else n
}

# Edges of conmat with their colours and tube radii. Returns the edge table
# and, if edges are coloured by weight, the palette and limits for a legend.
edge_table <- function(conmat, edge.color, edge.width, edge.color.weighted, edge.palette,
                       edge.limits, scale.edge.width, unit) {
  directed <- !isSymmetric(conmat)
  weighted <- !all(conmat %in% c(0, 1))
  pairs <- if (directed) {
    which(conmat != 0 & row(conmat) != col(conmat), arr.ind = TRUE)
  } else {
    which(conmat != 0 & upper.tri(conmat), arr.ind = TRUE)
  }
  w <- conmat[pairs]
  out <- list(palette = NULL, limits = NULL)
  color <- rep(edge.color, length(w))
  width <- rep(edge.width, length(w))
  if (weighted && length(w) > 0) {
    if (edge.color.weighted) {
      out$palette <- edge_palette(edge.palette, signed = any(w < 0))
      out$limits <- edge_limits(w, edge.limits)
      color <- map_colors(w, out$palette, out$limits)
    } else {
      range_w <- if (is.null(scale.edge.width)) c(0.5, 2) else scale.edge.width
      width <- if (length(unique(abs(w))) == 1) {
        rep(mean(range_w), length(w))
      } else {
        scales::rescale(abs(w), to = range_w)
      }
    }
  }
  out$table <- data.frame(from = pairs[, 1], to = pairs[, 2], weight = w, color = color,
                          radius = width / 2 * unit, directed = rep(directed, length(w)),
                          # A -> B and B -> A are drawn side by side rather than on top of each other
                          reciprocal = directed & conmat[pairs[, c(2, 1), drop = FALSE]] != 0,
                          stringsAsFactors = FALSE)
  out
}

# A ggplot of one rendered view, with labels and legends.
view_plot <- function(img, camera, data, show.legend, labels, label.size, hemi.labels,
                      network.colors, edges, bg.color) {
  w <- camera$width / 2
  h <- camera$height / 2
  p <- ggplot() +
    annotation_raster(img, xmin = -w, xmax = w, ymin = -h, ymax = h) +
    coord_fixed(xlim = c(-w, w), ylim = c(-h, h), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = if (is.na(bg.color)) NA else bg.color,
                                         colour = NA))
  if (labels) {
    pos <- project_points(camera, cbind(data$x.mni, data$y.mni, data$z.mni))
    pos$label <- as.character(data$ROI.Name)
    p <- p + ggrepel::geom_text_repel(data = pos, aes(x = x, y = y, label = label),
                                      size = label.size, seed = 1, max.overlaps = Inf,
                                      min.segment.length = 0.2)
  }
  if (hemi.labels && isTRUE(camera$hemi)) {
    p <- p + annotate("text", x = c(-w, w) * 0.92, y = h * 0.92, label = c("L", "R"),
                      fontface = "bold", size = 4)
  }
  if (show.legend) p <- add_legends(p, network.colors, edges)
  p
}

# Invisible layers that make ggplot draw legends for the network colours and
# the edge weight colours.
add_legends <- function(p, network.colors, edges) {
  if (!is.null(network.colors)) {
    nets <- data.frame(x = 0, y = 0, network = factor(names(network.colors),
                                                       levels = names(network.colors)))
    p <- p + geom_point(data = nets, aes(x = x, y = y, colour = network), alpha = 0) +
      scale_colour_manual(values = network.colors, name = "Network") +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)))
  }
  if (!is.null(edges$palette)) {
    weights <- data.frame(x = 0, y = 0, weight = edges$limits)
    p <- p + geom_point(data = weights, aes(x = x, y = y, fill = weight), alpha = 0) +
      scale_fill_gradientn(colours = edges$palette, limits = edges$limits, name = "Weight")
  }
  p
}

draw_legend_only <- function(network.colors, edges) {
  add_legends(ggplot() + theme_void(), network.colors, edges)
}

utils::globalVariables(c("x", "y", "label"))
