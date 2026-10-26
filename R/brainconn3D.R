#' Plots brains in 3D
#'
#' \code{brainconn3D} plots and returns a interactive plotly object of plotted brain connectivity matrix.
#'
#' Nodes are drawn as spheres (so they grow and shrink as you zoom) and edges
#' as lines between them; edge directions are not shown. Hovering over a node
#' shows its name, network, degree, strength and strongest connections. With
#' \code{highlight = TRUE}, clicking a node keeps its edges and neighbouring
#' nodes and fades out everything else; click the node again, click anywhere
#' else, or press Escape to show everything again.
#' @author Sidhant Chopra
#'
#'
#' @param atlas Either a string of one of the included atlases \code{brainconn::list_atlases()} or a \code{data.frame()} that meets specifications, see \code{vignette("brainconn")}
#' @param conmat A adjacency matrix. Can be binary, weighted, directed or undirected, see example_* data. If \code{NULL}, only the nodes are plotted.
#' @param all.nodes If \code{TRUE}, then all nodes will be shown. If \code{FALSE}, then only nodes with connecting edges will be shown.
#' @param node.color A colour for all nodes, e.g. "blue", or "network" to colour nodes by the network column of the atlas (see \code{\link{network_colors}}). Can also be a vector with one colour per ROI in the atlas, or per plotted node.
#' @param node.size Size of the nodes, as in \code{\link{brainconn}}: the diameter in mm for a human-sized brain, scaled with the size of the mesh. Can also be a vector with one value per ROI in the atlas, or one value per plotted node.
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
#' @param highlight If \code{TRUE}, clicking a node highlights its connections.
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
                        node.size = 8,
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

  # Node sizes are in mm for a human brain and scale with the mesh, as in brainconn()
  # The default brain is a simplified ICBM152 surface, which is plenty for a
  # translucent backdrop and keeps the page small and fast
  mesh <- if (identical(mesh, "ICBM152")) icbm152_lite else resolve_mesh(mesh)
  extent_points <- if (is.null(mesh)) xyz else mesh$vertices
  extent <- max(apply(extent_points, 2, function(p) diff(range(p))))
  unit <- if (is.null(mesh)) max(extent, 1) / 130 else extent / 170
  radius <- node.size / 2 * unit

  p <- plot_ly()
  trace <- 0L

  # Brain surface. Vertices are rounded to a power-of-two grid fine enough for
  # the brain's size; such numbers are exact in binary, so they are written to
  # the page with few digits. Its colour is set after building, below.
  if (!is.null(mesh)) {
    v <- quantise(mesh$vertices, extent / 600)
    p <- add_trace(p,
                   type = "mesh3d",
                   x = v[, 1], y = v[, 2], z = v[, 3],
                   i = mesh$faces[, 1] - 1, j = mesh$faces[, 2] - 1, k = mesh$faces[, 3] - 1,
                   opacity = opacity,
                   hoverinfo = "skip",
                   showlegend = FALSE)
    trace <- trace + 1L
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
  edge_trace <- hl_trace <- NA
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
    edge_trace <- trace
    trace <- trace + 1L
    if (highlight) {
      # Filled in by the JavaScript with the selected node's edges
      p <- add_trace(p,
                     type = "scatter3d",
                     mode = "lines",
                     x = numeric(0), y = numeric(0), z = numeric(0),
                     line = edge_line(edge.width * 1.5),
                     hoverinfo = "skip",
                     showlegend = FALSE)
      hl_trace <- trace
      trace <- trace + 1L
    }
  }

  # Nodes: spheres, one mesh trace per network (or one trace). Plotly's own
  # hover can't reach nodes inside the brain surface, so the JavaScript finds
  # the node under the mouse itself; the spheres are not pickable.
  # Simpler spheres for big networks keep the page small
  sphere <- unit_sphere(if (nrow(data) > 150) 1 else 2)
  groups <- if (by_network) factor(data$network, levels = names(network.colors)) else
    factor(rep("Nodes", nrow(data)))
  groups <- droplevels(groups)
  node_traces <- list()
  for (g in levels(groups)) {
    idx <- which(groups == g)
    s <- sphere_meshes(sphere, xyz[idx, , drop = FALSE], radius[idx], unit)
    p <- add_trace(p,
                   type = "mesh3d",
                   x = s$vertices[, 1], y = s$vertices[, 2], z = s$vertices[, 3],
                   i = s$faces[, 1] - 1, j = s$faces[, 2] - 1, k = s$faces[, 3] - 1,
                   name = g,
                   legendgroup = g,
                   hoverinfo = "skip",
                   showlegend = FALSE)
    node_traces[[g]] <- list(trace = trace, nodes = I(idx - 1L), colors = I(node.color[idx]))
    trace <- trace + 1L
  }
  # Highlighting fades the node traces and redraws the selected node and its
  # neighbours on this trace (opacity can only be set per trace). It starts as
  # one zero-area triangle.
  sel_trace <- NA
  if (highlight) {
    p <- add_trace(p,
                   type = "mesh3d",
                   x = rep(xyz[1, 1], 3), y = rep(xyz[1, 2], 3), z = rep(xyz[1, 3], 3),
                   i = 0, j = 1, k = 2,
                   hoverinfo = "skip",
                   showlegend = FALSE)
    sel_trace <- trace
    trace <- trace + 1L
  }
  # Legend entries: plotly colours a mesh's legend entry like its first vertex,
  # which fading would grey out, so each network gets a one-point trace for its
  # legend entry, which the JavaScript removes from the scene. Clicking an entry
  # still hides the network's nodes, which share its legendgroup.
  legend_traces <- integer(0)
  if (by_network) {
    for (g in levels(groups)) {
      first <- which(groups == g)[1]
      p <- add_trace(p,
                     type = "scatter3d",
                     mode = "markers",
                     x = xyz[first, 1], y = xyz[first, 2], z = xyz[first, 3],
                     marker = list(size = 8, color = network.colors[[g]]),
                     name = g,
                     legendgroup = g,
                     hoverinfo = "skip",
                     showlegend = TRUE)
      legend_traces <- c(legend_traces, trace)
      trace <- trace + 1L
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

  # plotly_build() would turn `color =` into a colour scale, so colours are set
  # on the built traces
  p <- plotly::plotly_build(p)
  if (!is.null(mesh)) p$x$data[[1]]$color <- "#F0F0F0"
  n_vertices <- nrow(sphere$vertices)
  lighting <- list(ambient = 0.55, diffuse = 0.7, specular = 0.35, roughness = 0.4)
  for (nt in node_traces) {
    tr <- p$x$data[[nt$trace + 1]]
    tr$vertexcolor <- rep(unclass(nt$colors), each = n_vertices)
    tr$flatshading <- FALSE
    tr$lighting <- lighting
    p$x$data[[nt$trace + 1]] <- tr
  }
  if (highlight) {
    p$x$data[[sel_trace + 1]]$vertexcolor <- rep(node.color[1], 3)
    p$x$data[[sel_trace + 1]]$flatshading <- FALSE
    p$x$data[[sel_trace + 1]]$lighting <- lighting
  }
  # The built traces are all the page needs; the unbuilt copy would double its size
  p$x$attrs <- NULL

  payload <- list(
    centres = I(lapply(seq_len(nrow(xyz)), function(i) I(xyz[i, ]))),
    radii = I(radius),
    hover = I(node_hover_text(data, conmat)),
    nodeTraces = unname(node_traces),
    legendTraces = I(legend_traces),
    verticesPerNode = n_vertices,
    highlight = highlight,
    edgeTrace = edge_trace,   # NA becomes null in JavaScript
    hlTrace = hl_trace,
    selTrace = sel_trace,
    nodeOpacity = 0.12,
    dimOpacity = 0.06
  )
  if (highlight) payload <- c(payload, highlight_payload(xyz, e, colored, nrow(data)))
  js <- paste(readLines(system.file("js", "brainconn3D.js", package = "brainconn")),
              collapse = "\n")
  htmlwidgets::onRender(p, js, data = payload)
}

# Round to a power-of-two grid no coarser than `step`: such numbers are exact in
# binary, so they print with few digits.
quantise <- function(x, step) {
  q <- 2^floor(log2(step))
  round(x / q) * q
}

# A unit icosphere: an icosahedron with each triangle split into four
# `subdivisions` times (2 gives 162 vertices).
unit_sphere <- function(subdivisions = 2) {
  key <- paste0("sphere", subdivisions)
  if (!is.null(brainconn_env[[key]])) return(brainconn_env[[key]])
  t <- (1 + sqrt(5)) / 2
  v <- rbind(c(-1, t, 0), c(1, t, 0), c(-1, -t, 0), c(1, -t, 0), c(0, -1, t), c(0, 1, t),
             c(0, -1, -t), c(0, 1, -t), c(t, 0, -1), c(t, 0, 1), c(-t, 0, -1), c(-t, 0, 1))
  f <- rbind(c(1, 12, 6), c(1, 6, 2), c(1, 2, 8), c(1, 8, 11), c(1, 11, 12), c(2, 6, 10),
             c(6, 12, 5), c(12, 11, 3), c(11, 8, 7), c(8, 2, 9), c(4, 10, 5), c(4, 5, 3),
             c(4, 3, 7), c(4, 7, 9), c(4, 9, 10), c(5, 10, 6), c(3, 5, 12), c(7, 3, 11),
             c(9, 7, 8), c(10, 9, 2))
  for (s in seq_len(subdivisions)) {
    midpoints <- new.env()
    mid <- function(a, b) {
      k <- paste(min(a, b), max(a, b))
      if (is.null(midpoints[[k]])) {
        v <<- rbind(v, (v[a, ] + v[b, ]) / 2)
        midpoints[[k]] <- nrow(v)
      }
      midpoints[[k]]
    }
    f <- do.call(rbind, lapply(seq_len(nrow(f)), function(r) {
      a <- f[r, 1]; b <- f[r, 2]; c <- f[r, 3]
      ab <- mid(a, b); bc <- mid(b, c); ca <- mid(c, a)
      rbind(c(a, ab, ca), c(b, bc, ab), c(c, ca, bc), c(ab, bc, ca))
    }))
  }
  sphere <- list(vertices = v / sqrt(rowSums(v^2)), faces = f)
  brainconn_env[[key]] <- sphere
  sphere
}

# One sphere per node, combined into one mesh. Vertices are rounded to a grid
# fine enough for the node sizes.
sphere_meshes <- function(sphere, centres, radius, unit) {
  nv <- nrow(sphere$vertices)
  n <- nrow(centres)
  vertices <- do.call(rbind, lapply(seq_len(n), function(i) {
    sweep(sphere$vertices * radius[i], 2, centres[i, ], "+")
  }))
  faces <- do.call(rbind, lapply(seq_len(n), function(i) sphere$faces + (i - 1) * nv))
  list(vertices = quantise(vertices, unit / 32), faces = faces)
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
