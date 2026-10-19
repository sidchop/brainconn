#' Plots brains in 3D
#'
#' \code{brainconn3D} plots and returns a interactive plotly object of plotted brain connectivity matrix.
#'
#' Edges are drawn as straight lines between nodes; directions are not shown.
#' Hovering over a node shows its name, network, degree, strength and
#' strongest connections. With \code{highlight = TRUE}, hovering over a node
#' also highlights its edges and neighbouring nodes and greys out the rest.
#' Clicking a node keeps it highlighted until you click it again or click the
#' brain surface.
#' @author Sidhant Chopra
#'
#'
#' @param atlas Either a string of one of the included atlases \code{brainconn::list_atlases()} or a \code{data.frame()} that meets specifications, see \code{vignette("brainconn")}
#' @param conmat A adjacency matrix. Can be binary, weighted, directed or undirected, see example_* data. If \code{NULL}, only the nodes are plotted.
#' @param all.nodes If \code{TRUE}, then all nodes will be shown. If \code{FALSE}, then only nodes with connecting edges will be shown.
#' @param node.color A colour for all nodes, e.g. "blue", or "network" to colour nodes by the network column of the atlas (see \code{\link{network_colors}}). Can also be a vector with one colour per ROI in the atlas, or per plotted node.
#' @param node.size A number that determines the diameter of the nodes. Can also be a vector with one value per ROI in the atlas, or one value per plotted node.
#' @param edge.width Number to set the width of the edges.
#' @param edge.color A string that sets the edge color. e.g. "blue". Ignored when \code{edge.color.weighted = TRUE}.
#' @param opacity Number between 0-1 to  set the transparency of the mesh brain
#' @param d.factor A number to change the distance between the nodes and brain surface.
#' @param show.legend if \code{TRUE}, legend will be shown. If \code{FALSE}, then no legend will be shown
#' @param thr a optional value to set a threshold on the conmat (e.g. edges with a weighted value lower than the one set here will not be shown)
#' @param uthr a optional value to set a upper threshold on the conmat (e.g. edges with a weighted value higher than the one set here will not be shown)
#' @param mesh The brain surface: \code{"ICBM152"} (the default), \code{NULL} for no surface, or a mesh in the same space as the atlas coordinates, e.g. mesh file path(s); see \code{\link{read_brain_mesh}} and \code{\link{brainconn}}.
#' @param edge.color.weighted For weighted matrices: if \code{TRUE}, edges are coloured by weight (see \code{edge.palette}).
#' @param edge.palette,edge.limits Colours and limits for edge weights when \code{edge.color.weighted = TRUE}; see \code{\link{brainconn}}.
#' @param highlight If \code{TRUE}, hovering over (or clicking) a node highlights its connections.
#' @return a plotly object
#' @importFrom plotly plot_ly add_trace layout plotly_build
#' @examples
#' \dontrun{
#' library(brainconn)
#' brainconn3D(atlas ="schaefer300_n7", conmat=example_unweighted_undirected, show.legend = FALSE)
#' brainconn3D(atlas = "schaefer300_n7", conmat = example_weighted_undirected,
#'             edge.color.weighted = TRUE, highlight = TRUE)
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
                        uthr = NULL,
                        mesh = "ICBM152",
                        edge.color.weighted = FALSE,
                        edge.palette = NULL,
                        edge.limits = NULL,
                        highlight = FALSE) {

  data <- resolve_atlas(atlas, parent.frame())
  conmat <- prepare_conmat(conmat, nrow(data), thr, uthr)

  keep <- nodes_to_keep(conmat, all.nodes)
  node.size <- rep_len(match_per_node(node.size, keep, "node.size"), sum(keep))
  node.color <- resolve_node_color(node.color, data)
  by_network <- identical(node.color, "network")
  if (by_network) {
    network.colors <- network_colors(data$network)
    node.color <- unname(network.colors[as.character(data$network[keep])])
  } else {
    node.color <- rep_len(match_per_node(node.color, keep, "node.color"), sum(keep))
  }
  data <- data[keep, , drop = FALSE]
  conmat <- conmat[keep, keep, drop = FALSE]

  xyz <- cbind(data$x.mni, data$y.mni, data$z.mni) * d.factor
  edges <- edge_table(conmat, edge.color, 1, edge.color.weighted, edge.palette, edge.limits,
                      NULL, unit = 1)
  colored <- !is.null(edges$palette)
  e <- edges$table

  p <- plot_ly()

  # Brain surface. Hover events on it (without a tooltip) let a click on the
  # brain clear a highlighted node.
  mesh <- resolve_mesh(mesh)
  if (!is.null(mesh)) {
    # Coordinates rounded to 0.25 mm are exact in binary, so they are written
    # to the page with few digits; this keeps the page small (the default mesh
    # has 82k vertices). The colour is set after building, below.
    v <- round(mesh$vertices * 4) / 4
    p <- add_trace(p,
                   type = "mesh3d",
                   x = v[, 1], y = v[, 2], z = v[, 3],
                   i = mesh$faces[, 1] - 1, j = mesh$faces[, 2] - 1, k = mesh$faces[, 3] - 1,
                   opacity = opacity,
                   hoverinfo = if (highlight) "none" else "skip",
                   showlegend = FALSE)
  }

  # Edges: one trace, with NA between edges to break the line
  edge_line <- function(width) {
    line <- list(width = width, color = edge.color)
    if (colored) {
      line$colorscale <- plotly_colorscale(edges$palette)
      line$cmin <- edges$limits[1]
      line$cmax <- edges$limits[2]
    }
    line
  }
  if (nrow(e) > 0) {
    path <- function(v) as.vector(rbind(v[e$from], v[e$to], NA))
    line <- edge_line(edge.width)
    if (colored) {
      line$color <- as.vector(rbind(e$weight, e$weight, NA))
      line$showscale <- show.legend
      # Lower right, below the network legend
      line$colorbar <- list(title = "Weight", len = 0.45, y = 0, yanchor = "bottom")
    }
    p <- add_trace(p,
                   type = "scatter3d",
                   mode = "lines",
                   x = path(xyz[, 1]), y = path(xyz[, 2]), z = path(xyz[, 3]),
                   line = line,
                   opacity = 1,
                   hoverinfo = "skip",
                   showlegend = FALSE)
    if (highlight) {
      # Filled in by the highlighting JavaScript
      p <- add_trace(p,
                     type = "scatter3d",
                     mode = "lines",
                     x = numeric(0), y = numeric(0), z = numeric(0),
                     line = edge_line(edge.width * 2.5),
                     hoverinfo = "skip",
                     showlegend = FALSE)
    }
  }

  # Nodes: one trace per network (or one trace), with per-node colours and
  # the node's index (0-based) as customdata
  hover <- node_hover_text(data, conmat)
  groups <- if (by_network) factor(data$network, levels = names(network.colors)) else
    factor(rep("Nodes", nrow(data)))
  groups <- droplevels(groups)
  for (g in levels(groups)) {
    idx <- which(groups == g)
    p <- add_trace(p,
                   type = "scatter3d",
                   mode = "markers",
                   x = xyz[idx, 1], y = xyz[idx, 2], z = xyz[idx, 3],
                   marker = list(size = I(node.size[idx]), color = I(node.color[idx]),
                                 line = list(width = 0)),
                   name = g,
                   legendgroup = g,
                   customdata = I(idx - 1),
                   text = hover[idx],
                   hoverinfo = "text",
                   showlegend = by_network && !highlight)
  }
  # Highlighting greys out nodes, which would also grey out the legend, so the
  # legend gets its own traces: one point on a node of each network, which the
  # JavaScript removes from the scene once the plot is drawn. Clicking a legend
  # entry still hides its network's nodes, which share its legendgroup.
  if (by_network && highlight) {
    for (g in levels(groups)) {
      first <- which(groups == g)[1]
      p <- add_trace(p,
                     type = "scatter3d",
                     mode = "markers",
                     x = xyz[first, 1], y = xyz[first, 2], z = xyz[first, 3],
                     marker = list(size = node.size[first], color = network.colors[[g]]),
                     name = g,
                     legendgroup = g,
                     hoverinfo = "skip",
                     showlegend = TRUE)
    }
  }

  hidden_axis <- list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE,
                      ticks = "", showticklabels = FALSE, showspikes = FALSE)
  p <- layout(p,
              showlegend = show.legend,
              legend = list(y = 1, yanchor = "top", itemsizing = "constant"),
              scene = list(xaxis = hidden_axis,
                           yaxis = hidden_axis,
                           zaxis = hidden_axis,
                           aspectmode = "data"))

  # plotly_build() turns `color =` into a colour scale, so a single surface
  # colour is set on the built trace
  p <- plotly::plotly_build(p)
  if (!is.null(mesh)) p$x$data[[1]]$color <- "#F0F0F0"
  # The built traces are all the page needs; the unbuilt copy would double its size
  p$x$attrs <- NULL

  if (highlight) {
    n_before_nodes <- as.integer(!is.null(mesh)) + if (nrow(e) > 0) 2L else 0L
    payload <- highlight_payload(xyz, e, colored, nrow(data))
    n_groups <- length(levels(groups))
    payload$nodeTraces <- I(n_before_nodes + seq_len(n_groups) - 1L)
    payload$legendTraces <- I(if (by_network) n_before_nodes + n_groups + seq_len(n_groups) - 1L
                              else integer(0))
    # NA becomes null in JavaScript
    payload$edgeTrace <- if (nrow(e) > 0) as.integer(!is.null(mesh)) else NA
    payload$hlTrace <- if (nrow(e) > 0) payload$edgeTrace + 1L else NA
    payload$dimColor <- "#D9D9D9"
    payload$dimOpacity <- 0.12
    js <- paste(readLines(system.file("js", "brainconn-highlight.js", package = "brainconn")),
                collapse = "\n")
    p <- htmlwidgets::onRender(p, js, data = payload)
  }
  p
}

# Colour stops for plotly from a vector of colours (lowest to highest).
plotly_colorscale <- function(colors, n = 11) {
  pick <- colors[round(seq(1, length(colors), length.out = n))]
  lapply(seq_len(n), function(i) list((i - 1) / (n - 1), pick[i]))
}

# Hover text for each node: name, network, degree, strength and strongest
# connections. For directed matrices, edges in either direction count.
# Strength is the sum of absolute weights.
node_hover_text <- function(data, conmat, top = 5) {
  weighted <- !all(conmat %in% c(0, 1))
  vapply(seq_len(nrow(data)), function(i) {
    w <- ifelse(conmat[i, ] != 0, conmat[i, ], conmat[, i])
    nb <- order(-abs(w))[seq_len(min(top, sum(w != 0)))]
    lines <- c(paste0("<b>", data$ROI.Name[i], "</b>"),
               if ("network" %in% names(data)) paste("Network:", data$network[i]),
               paste("Degree:", sum(w != 0)),
               if (weighted) paste("Strength:", signif(sum(abs(w)), 3)))
    if (length(nb) > 0) {
      strongest <- paste0("&nbsp;&nbsp;", data$ROI.Name[nb],
                          if (weighted) paste0(" (", signif(w[nb], 3), ")"))
      lines <- c(lines, "Connections:", strongest,
                 if (sum(w != 0) > top) paste0("&nbsp;&nbsp;... ", sum(w != 0) - top, " more"))
    }
    paste(lines, collapse = "<br>")
  }, character(1))
}

# For each node, its neighbours and the coordinates (and weights, if edges are
# coloured) of its edges, for the highlighting JavaScript. Indices are 0-based.
highlight_payload <- function(xyz, e, colored, n) {
  neighbors <- lapply(seq_len(n), function(i) {
    I(unique(c(e$to[e$from == i], e$from[e$to == i])) - 1L)
  })
  edges <- lapply(seq_len(n), function(i) {
    k <- which(e$from == i | e$to == i)
    path <- function(v) as.vector(rbind(v[e$from[k]], v[e$to[k]], NA))
    out <- list(x = I(path(xyz[, 1])), y = I(path(xyz[, 2])), z = I(path(xyz[, 3])))
    if (colored) out$w <- I(as.vector(rbind(e$weight[k], e$weight[k], NA)))
    out
  })
  list(neighbors = neighbors, edges = edges)
}
