#' Plot a connectivity matrix
#'
#' Plots a region x region connectivity matrix as a heatmap with regions
#' grouped by network: coloured strips and labels mark the networks, and
#' lines mark the borders between them. The matrix can be smoothed (each cell
#' replaced by the mean of its pair of networks), summarised to a network x
#' network matrix, cut to one triangle, combined with a second matrix in the
#' other triangle, or rotated 45 degrees.
#'
#' @param conmat A square region x region matrix (or data.frame).
#' @param atlas An atlas whose \code{network} column gives each region's
#'   network: the name of an included atlas or a data.frame. If it has a
#'   \code{hemi} column, regions are ordered by hemisphere within each network.
#' @param networks Alternatively, a vector with the network of each region.
#'   Networks are shown in factor level order, or in order of first appearance.
#'   If neither \code{atlas} nor \code{networks} is given, the matrix is plotted
#'   as it is, without network strips or borders.
#' @param level \code{"region"} to plot every region, or \code{"network"} to
#'   plot a network x network summary (see \code{\link{network_matrix}}).
#' @param fun How to summarise each pair of networks for \code{level = "network"}
#'   or \code{smooth = TRUE}: \code{"mean"}, \code{"sum"}, \code{"count"},
#'   \code{"density"}, \code{"median"} or a function; see \code{\link{network_matrix}}.
#' @param smooth For \code{level = "region"}: if \code{TRUE}, each cell shows the
#'   summary (by default the mean) of its pair of networks.
#' @param triangle \code{"full"}, \code{"lower"} or \code{"upper"}: which part
#'   of the matrix to show.
#' @param conmat2 An optional second matrix, the same size as \code{conmat}.
#'   \code{conmat} is shown in the lower triangle and \code{conmat2} in the
#'   upper triangle, on the same colour scale (\code{triangle} is ignored).
#' @param order If \code{TRUE}, regions are reordered so each network's regions
#'   are together. If \code{FALSE}, the input order is kept.
#' @param diag If \code{FALSE}, the diagonal (self-connections) is left blank
#'   in region-level plots, unless \code{smooth = TRUE}.
#' @param palette Colours from the lowest to the highest value: a vector of
#'   colours or a palette name from \code{grDevices::hcl.pals()}. By default
#'   blue-white-red for signed values and white-to-red otherwise.
#' @param limits Values mapped to the ends of the palette. By default
#'   symmetric around zero for signed values, otherwise the range of the values.
#' @param borders If \code{TRUE}, draw lines between networks (region level).
#' @param border.color,border.width Colour and width of the network borders.
#' @param labels If \code{TRUE}, write the network names next to the strips.
#' @param label.size Size of the network names.
#' @param values If \code{TRUE}, write each cell's value (network level only).
#' @param diamond If \code{TRUE}, show the lower triangle rotated 45 degrees,
#'   with the diagonal along the top.
#' @param legend.title Title of the colour legend.
#' @param show.legend If \code{TRUE}, show the colour legend.
#' @return A ggplot object.
#' @export
#' @examples
#' x <- example_fc_schaefer300
#' brainconn_matrix(x, atlas = "schaefer300_n7")
#' brainconn_matrix(x, atlas = "schaefer300_n7", smooth = TRUE, triangle = "lower")
#' brainconn_matrix(x, atlas = "schaefer300_n7", level = "network", values = TRUE)
#' brainconn_matrix(x, atlas = "schaefer300_n7", diamond = TRUE)
brainconn_matrix <- function(conmat,
                             atlas = NULL,
                             networks = NULL,
                             level = c("region", "network"),
                             fun = "mean",
                             smooth = FALSE,
                             triangle = c("full", "lower", "upper"),
                             conmat2 = NULL,
                             order = TRUE,
                             diag = FALSE,
                             palette = NULL,
                             limits = NULL,
                             borders = TRUE,
                             border.color = "black",
                             border.width = 0.4,
                             labels = TRUE,
                             label.size = 3,
                             values = FALSE,
                             diamond = FALSE,
                             legend.title = "Value",
                             show.legend = TRUE) {
  level <- match.arg(level)
  triangle <- match.arg(triangle)
  conmat <- as_square_matrix(conmat, "conmat")
  n <- nrow(conmat)
  if (!is.null(conmat2)) {
    conmat2 <- as_square_matrix(conmat2, "conmat2")
    if (nrow(conmat2) != n) stop("`conmat2` must be the same size as `conmat`.", call. = FALSE)
    triangle <- "lower"
  }
  if (diamond) triangle <- "lower"

  # Each region's network, and a hemisphere for ordering within networks
  hemi <- NULL
  if (!is.null(networks)) {
    networks <- resolve_networks(networks, n)
  } else if (!is.null(atlas)) {
    atlas <- resolve_atlas(atlas, parent.frame())
    if (nrow(atlas) != n) {
      stop("`conmat` has ", n, " regions but the atlas has ", nrow(atlas), ".", call. = FALSE)
    }
    networks <- resolve_networks(atlas, n)
    if ("hemi" %in% names(atlas)) hemi <- atlas$hemi
  }
  if (is.null(networks) && (level == "network" || smooth)) {
    stop("Give `atlas` or `networks` to summarise by network.", call. = FALSE)
  }

  if (!is.null(networks) && order) {
    ord <- order(as.integer(networks), if (is.null(hemi)) rep(0, n) else xtfrm(hemi), seq_len(n))
    conmat <- conmat[ord, ord, drop = FALSE]
    if (!is.null(conmat2)) conmat2 <- conmat2[ord, ord, drop = FALSE]
    networks <- networks[ord]
  }

  # The matrix (or matrices) to show
  if (level == "network") {
    mat <- network_matrix(conmat, networks, fun)
    mat2 <- if (!is.null(conmat2)) network_matrix(conmat2, networks, fun)
    cell_networks <- factor(levels(networks), levels = levels(networks))
  } else {
    mat <- if (smooth) network_matrix(conmat, networks, fun, full = TRUE) else conmat
    mat2 <- if (is.null(conmat2)) NULL else if (smooth) {
      network_matrix(conmat2, networks, fun, full = TRUE)
    } else {
      conmat2
    }
    if (!diag && !smooth) {
      diag(mat) <- NA
      if (!is.null(mat2)) diag(mat2) <- NA
    }
    cell_networks <- networks
  }
  k <- nrow(mat)
  cells <- matrix_cells(mat, mat2, triangle)

  all_values <- cells$value[!is.na(cells$value)]
  if (length(all_values) == 0) all_values <- 0
  signed <- any(all_values < 0)
  limits <- if (is.null(limits)) edge_limits(all_values) else limits
  colors <- matrix_palette(palette, signed)

  # Geometry, in matrix coordinates: cell (i, j) is centred at x = j,
  # y = k - i + 1, so row 1 is at the top. Diamond plots are rotated after.
  runs <- if (is.null(cell_networks)) NULL else network_runs(cell_networks)
  strip_w <- k * 0.025
  gap <- k * 0.006
  if (diamond) {
    p <- ggplot() +
      geom_polygon(data = rotate45(cell_polygons(cells, k), k),
                   aes(x = x, y = y, group = cell, fill = value), colour = NA)
  } else if (level == "network") {
    p <- ggplot() +
      geom_tile(data = cells, aes(x = j, y = k - i + 1, fill = value),
                colour = "white", linewidth = 1)
  } else {
    p <- ggplot() +
      geom_raster(data = cells, aes(x = j, y = k - i + 1, fill = value))
  }
  p <- p + scale_fill_gradientn(colours = colors, limits = limits, na.value = "transparent",
                                oob = scales::squish, name = legend.title)

  if (values && level == "network" && !diamond) {
    shown <- cells[!is.na(cells$value), ]
    # White text on the strongest colours, black elsewhere
    # (distance from zero for signed values, from the low end otherwise)
    strength <- abs(scales::rescale(pmin(pmax(shown$value, limits[1]), limits[2]),
                                    from = limits, to = if (signed) c(-1, 1) else c(0, 1)))
    # Whole numbers (e.g. edge counts) without decimals, other values to 2 decimals
    whole <- all(shown$value == round(shown$value))
    shown$label <- formatC(shown$value, digits = if (whole) 0 else 2, format = "f")
    p <- p + geom_text(data = shown,
                       aes(x = j, y = k - i + 1, label = label),
                       colour = ifelse(strength > 0.6, "white", "black"),
                       size = label.size * 0.9)
  }

  # Points the plot must include: the cells, then strips and labels
  extent <- if (diamond) {
    # Corners of the lower triangle: top left, bottom left, bottom right
    rotate45(data.frame(x = c(0.5, 0.5, k + 0.5), y = c(k + 0.5, 0.5, 0.5)), k)
  } else {
    data.frame(x = c(0.5, k + 0.5), y = c(0.5, k + 0.5))
  }
  extent <- extent[, c("x", "y")]

  if (!is.null(runs)) {
    sides <- if (triangle == "upper") c("top", "right") else c("left", "bottom")
    strips <- network_strips(runs, k, sides, strip_w, gap)
    text <- if (labels) network_labels(runs, k, sides, strip_w, gap) else NULL
    lines <- if (borders && level == "region") network_borders(runs, k, triangle) else NULL
    if (diamond) {
      strips <- rotate45(strips, k)
      if (!is.null(text)) {
        text <- rotate45(text, k)
        # Keep the labels on the bottom-left leg readable
        flip <- text$angle > 90
        text$angle[flip] <- text$angle[flip] - 180
        text$hjust[flip] <- 1 - text$hjust[flip]
      }
      if (!is.null(lines)) lines <- rotate45_segments(lines, k)
    }
    colours <- network_colors(cell_networks)
    p <- p + geom_polygon(data = strips, aes(x = x, y = y, group = group),
                          fill = colours[as.character(strips$network)], colour = NA)
    if (!is.null(lines) && nrow(lines) > 0) {
      p <- p + geom_segment(data = lines, aes(x = x, y = y, xend = xend, yend = yend),
                            colour = border.color, linewidth = border.width)
    }
    if (!is.null(text)) {
      p <- p + geom_text(data = text, aes(x = x, y = y, label = label, angle = angle,
                                          hjust = hjust),
                         size = label.size, vjust = 0.5)
    }
    extent <- rbind(extent, strips[, c("x", "y")],
                    if (!is.null(text)) label_extent(text, k, label.size))
  }

  p + coord_fixed(xlim = range(extent$x), ylim = range(extent$y), expand = FALSE,
                  clip = "off") +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10, "mm"),
          legend.position = if (show.legend) "right" else "none")
}

as_square_matrix <- function(x, arg) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (nrow(x) != ncol(x)) stop("`", arg, "` must be square.", call. = FALSE)
  dimnames(x) <- NULL
  x
}

# Default heatmap colours (white in the middle is fine on a white page), or
# the user's palette.
matrix_palette <- function(palette, signed) {
  if (is.null(palette)) {
    palette <- if (signed) {
      c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
        "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F")
    } else {
      c("#FFF5F0", "#FCBBA1", "#FB6A4A", "#CB181D", "#67000D")
    }
  }
  edge_palette(palette, signed)
}

# Cells to draw: (i, j, value) for the chosen triangle, with mat2 (if any) in
# the upper triangle.
matrix_cells <- function(mat, mat2, triangle) {
  k <- nrow(mat)
  cells <- expand.grid(i = seq_len(k), j = seq_len(k))
  lower <- cells$i >= cells$j
  upper <- cells$i <= cells$j
  cells$value <- mat[cbind(cells$i, cells$j)]
  if (!is.null(mat2)) {
    cells$value[cells$i < cells$j] <- mat2[cbind(cells$i, cells$j)][cells$i < cells$j]
  } else if (triangle == "lower") {
    cells <- cells[lower, ]
  } else if (triangle == "upper") {
    cells <- cells[upper, ]
  }
  cells
}

# Consecutive runs of the same network: network, first and last position.
network_runs <- function(networks) {
  r <- rle(as.character(networks))
  end <- cumsum(r$lengths)
  data.frame(network = factor(r$values, levels = levels(networks)),
             start = end - r$lengths + 1, end = end)
}

# Coloured strips (one rectangle per network run, as 4-corner polygons) along
# the given sides of the matrix.
network_strips <- function(runs, k, sides, w, gap) {
  lo <- runs$start - 0.5
  hi <- runs$end + 0.5
  rects <- do.call(rbind, lapply(sides, function(side) {
    switch(side,
           left = data.frame(xmin = 0.5 - gap - w, xmax = 0.5 - gap,
                             ymin = k - hi + 1, ymax = k - lo + 1),
           right = data.frame(xmin = k + 0.5 + gap, xmax = k + 0.5 + gap + w,
                              ymin = k - hi + 1, ymax = k - lo + 1),
           bottom = data.frame(xmin = lo, xmax = hi, ymin = 0.5 - gap - w, ymax = 0.5 - gap),
           top = data.frame(xmin = lo, xmax = hi, ymin = k + 0.5 + gap, ymax = k + 0.5 + gap + w))
  }))
  data.frame(x = c(rbind(rects$xmin, rects$xmax, rects$xmax, rects$xmin)),
             y = c(rbind(rects$ymin, rects$ymin, rects$ymax, rects$ymax)),
             group = rep(seq_len(nrow(rects)), each = 4),
             network = rep(rep(runs$network, length(sides)), each = 4))
}

# Network names next to the strips.
network_labels <- function(runs, k, sides, w, gap) {
  mid <- (runs$start + runs$end) / 2
  out <- lapply(sides, function(side) {
    switch(side,
           left = data.frame(x = 0.5 - 2 * gap - w, y = k - mid + 1, angle = 0, hjust = 1),
           right = data.frame(x = k + 0.5 + 2 * gap + w, y = k - mid + 1, angle = 0, hjust = 0),
           bottom = data.frame(x = mid, y = 0.5 - 2 * gap - w, angle = 90, hjust = 1),
           top = data.frame(x = mid, y = k + 0.5 + 2 * gap + w, angle = 90, hjust = 0))
  })
  out <- do.call(rbind, out)
  out$label <- rep(as.character(runs$network), length(sides))
  out
}

# Approximate far ends of the network labels, so the plot leaves room for
# them (text size doesn't scale with the data, so this is a rough estimate).
label_extent <- function(text, k, label.size) {
  len <- nchar(text$label) * k * 0.011 * label.size / 3
  theta <- text$angle * pi / 180
  sign <- ifelse(text$hjust > 0.5, -1, 1)
  rbind(data.frame(x = text$x, y = text$y),
        data.frame(x = text$x + sign * len * cos(theta), y = text$y + sign * len * sin(theta)))
}

# Lines between networks, limited to the shown triangle. Boundary b lies
# between positions b and b + 1.
network_borders <- function(runs, k, triangle) {
  b <- runs$end[-nrow(runs)]
  if (length(b) == 0) return(NULL)
  lo <- 0.5
  hi <- k + 0.5
  edge <- k - b + 0.5   # y of the boundary below row b
  switch(triangle,
         full = rbind(data.frame(x = b + 0.5, y = lo, xend = b + 0.5, yend = hi),
                      data.frame(x = lo, y = edge, xend = hi, yend = edge)),
         lower = rbind(data.frame(x = b + 0.5, y = lo, xend = b + 0.5, yend = edge),
                       data.frame(x = lo, y = edge, xend = b + 0.5, yend = edge)),
         upper = rbind(data.frame(x = b + 0.5, y = edge, xend = b + 0.5, yend = hi),
                       data.frame(x = b + 0.5, y = edge, xend = hi, yend = edge)))
}

# Each cell as a square polygon (for rotated plots).
cell_polygons <- function(cells, k) {
  x0 <- cells$j
  y0 <- k - cells$i + 1
  data.frame(x = c(rbind(x0 - 0.5, x0 + 0.5, x0 + 0.5, x0 - 0.5)),
             y = c(rbind(y0 - 0.5, y0 - 0.5, y0 + 0.5, y0 + 0.5)),
             cell = rep(seq_len(nrow(cells)), each = 4),
             value = rep(cells$value, each = 4))
}

# Rotate 45 degrees anticlockwise about the centre of a k x k matrix, so the
# diagonal (top-left to bottom-right) becomes horizontal.
rotate45 <- function(d, k) {
  c0 <- (k + 1) / 2
  x <- d$x - c0
  y <- d$y - c0
  d$x <- (x - y) / sqrt(2)
  d$y <- (x + y) / sqrt(2)
  if ("angle" %in% names(d)) d$angle <- d$angle + 45
  d
}

rotate45_segments <- function(d, k) {
  start <- rotate45(data.frame(x = d$x, y = d$y), k)
  end <- rotate45(data.frame(x = d$xend, y = d$yend), k)
  data.frame(x = start$x, y = start$y, xend = end$x, yend = end$y)
}

utils::globalVariables(c("i", "j", "value", "cell", "group", "xend", "yend", "angle", "hjust"))
