#' Plot connectivity between networks as a chord diagram
#'
#' Draws a circular chord diagram of the connectivity within and between
#' networks: each network is a coloured sector of the circle, and each pair of
#' networks with a link is joined by a ribbon coloured by its value. Networks
#' can be grouped (e.g. the 17 Schaefer networks into the 7 Yeo networks),
#' which adds an outer ring naming each group.
#'
#' The input can be a region x region matrix with an atlas (or a
#' \code{networks} vector), which is summarised by network with
#' \code{\link{network_matrix}}, or a network x network matrix with network
#' names as row names.
#'
#' Each sector is split into one slot per link, so ribbons don't overlap
#' where they meet the circle. Links within a network are drawn as loops from
#' the first to the last slot of its sector. For directed matrices, each
#' direction gets its own ribbon.
#'
#' @param conmat A region x region matrix (with \code{atlas} or
#'   \code{networks}), or a network x network matrix with row names.
#' @param atlas An atlas with a \code{network} column: the name of an included
#'   atlas or a data.frame.
#' @param networks Alternatively, a vector with the network of each region.
#' @param group Groups of networks, shown as an outer ring: the name of an
#'   atlas column (e.g. \code{"network7"} for the Schaefer 17-network atlases),
#'   a vector with one group per network, or a named vector mapping network
#'   names to groups. When given, network labels inside the ring drop the
#'   group name (e.g. "DefaultA" is labelled "A" in the "Default" group).
#' @param fun How to summarise each pair of networks; see
#'   \code{\link{network_matrix}}.
#' @param thr Only show links whose absolute value is at least \code{thr}.
#' @param top Only show the strongest links: the top \code{top} percent of the
#'   non-zero links, by absolute value.
#' @param palette Colours for link values, from the lowest to the highest: a
#'   vector of colours, \code{"bkr"} (the blue-black-red scale of Chopra et
#'   al.), \code{"bwr"}, or a palette name from \code{grDevices::hcl.pals()}.
#'   By default dark blue to dark red for signed values.
#' @param limits Values mapped to the ends of the palette. By default symmetric
#'   around zero for signed values, otherwise the range of the values.
#' @param link.alpha Opacity of the ribbons.
#' @param label.size Size of the labels.
#' @param gap,group.gap Gap in degrees between sectors, and between groups.
#' @param legend.title Title of the colour legend.
#' @param show.legend If \code{TRUE}, show the colour legend.
#' @return A ggplot object.
#' @export
#' @examples
#' x <- example_fc_schaefer300
#' brainconn_circle(x, atlas = "schaefer300_n7")
#' brainconn_circle(x, atlas = "schaefer300_n7", top = 30, palette = "bkr")
#'
#' # A network x network matrix, with groups
#' net <- network_matrix(x, "schaefer300_n7")
#' brainconn_circle(net, group = c(Vis = "Sensory", SomMot = "Sensory",
#'                                 DorsAttn = "Attention", SalVentAttn = "Attention",
#'                                 Limbic = "Other", Cont = "Other", Default = "Other"))
brainconn_circle <- function(conmat,
                             atlas = NULL,
                             networks = NULL,
                             group = NULL,
                             fun = "mean",
                             thr = NULL,
                             top = NULL,
                             palette = NULL,
                             limits = NULL,
                             link.alpha = 0.85,
                             label.size = 3.5,
                             gap = 2,
                             group.gap = 6,
                             legend.title = "Value",
                             show.legend = TRUE) {

  # Network x network matrix
  conmat <- as.matrix(conmat)
  atlas_df <- NULL
  if (!is.null(networks) || !is.null(atlas)) {
    if (is.null(networks)) {
      atlas_df <- resolve_atlas(atlas, parent.frame())
      networks <- atlas_df
    }
    networks <- resolve_networks(networks, nrow(conmat))
    net <- network_matrix(conmat, networks, fun)
  } else {
    if (is.null(rownames(conmat)) || nrow(conmat) != ncol(conmat)) {
      stop("Give `atlas` or `networks`, or a square network x network matrix with row names.",
           call. = FALSE)
    }
    net <- conmat
    storage.mode(net) <- "double"
    colnames(net) <- rownames(net)
  }
  nets <- rownames(net)
  k <- length(nets)

  # Groups of networks, and the order of networks around the circle
  groups <- resolve_groups(group, nets, atlas_df, networks)
  ord <- if (is.null(groups)) seq_len(k) else order(as.integer(groups), seq_len(k))
  nets <- nets[ord]
  net <- net[ord, ord, drop = FALSE]
  if (!is.null(groups)) groups <- groups[ord]

  links <- chord_links(net, thr, top)
  sectors <- chord_sectors(k, groups, gap, group.gap)
  ribbons <- chord_ribbons(links, sectors)

  values <- if (nrow(links) > 0) links$value else 0
  signed <- any(values < 0)
  limits <- if (is.null(limits)) edge_limits(values) else limits
  colors <- edge_palette(palette, signed)

  # Rings and labels
  colours <- network_colors(factor(nets, levels = nets))
  ring <- ring_polygons(sectors$start, sectors$end, 1.02, 1.12)
  short <- if (is.null(groups)) nets else short_network_names(nets, as.character(groups))
  label_r <- if (is.null(groups)) 1.17 else 1.07
  net_labels <- tangential_labels((sectors$start + sectors$end) / 2, label_r, short,
                                  outside = is.null(groups))

  p <- ggplot()
  if (nrow(ribbons) > 0) {
    p <- p + geom_polygon(data = ribbons, aes(x = x, y = y, group = link, fill = value),
                          alpha = link.alpha, colour = NA) +
      scale_fill_gradientn(colours = colors, limits = limits, oob = scales::squish,
                           name = legend.title)
  }
  p <- p + geom_polygon(data = ring, aes(x = x, y = y, group = id),
                        fill = colours[ring$id], colour = "white", linewidth = 0.3) +
    geom_text(data = net_labels, aes(x = x, y = y, label = label, angle = angle),
              size = label.size, fontface = if (is.null(groups)) "plain" else "bold",
              colour = if (is.null(groups)) "black" else label_colours(colours))

  extent <- 1.45
  if (!is.null(groups)) {
    spans <- group_spans(sectors, groups)
    p <- p + geom_polygon(data = ring_polygons(spans$start, spans$end, 1.16, 1.18),
                          aes(x = x, y = y, group = id), fill = "grey20") +
      geom_text(data = tangential_labels((spans$start + spans$end) / 2, 1.24, spans$group,
                                         outside = TRUE),
                aes(x = x, y = y, label = label, angle = angle),
                size = label.size, fontface = "bold")
    extent <- 1.5
  }

  p + coord_fixed(xlim = c(-extent, extent), ylim = c(-extent, extent), expand = FALSE,
                  clip = "off") +
    theme_void() +
    theme(legend.position = if (show.legend && nrow(ribbons) > 0) "right" else "none")
}

# A factor with the group of each network (or NULL).
resolve_groups <- function(group, nets, atlas, networks) {
  if (is.null(group)) return(NULL)
  if (!is.null(names(group))) {
    missing <- setdiff(nets, names(group))
    if (length(missing) > 0) {
      stop("`group` has no group for: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    group <- group[nets]
  } else if (is.character(group) && length(group) == 1 && length(nets) > 1) {
    if (is.null(atlas) || !group %in% names(atlas)) {
      stop("`group = \"", group, "\"` must name a column of the atlas.", call. = FALSE)
    }
    group <- atlas[[group]][match(nets, as.character(networks))]
  } else if (length(group) != length(nets)) {
    stop("`group` must have one value per network (", length(nets), ").", call. = FALSE)
  }
  group <- as.character(group)
  factor(group, levels = unique(group))
}

# The links to draw: one per pair of networks (both directions if the matrix
# is not symmetric), without zeros, filtered by `thr` and `top`.
chord_links <- function(net, thr, top) {
  k <- nrow(net)
  keep <- if (isSymmetric(unname(net))) upper.tri(net, diag = TRUE) else matrix(TRUE, k, k)
  pairs <- which(keep & !is.na(net) & net != 0, arr.ind = TRUE)
  links <- data.frame(from = pairs[, 1], to = pairs[, 2], value = net[pairs])
  if (!is.null(thr)) links <- links[abs(links$value) >= thr, , drop = FALSE]
  if (!is.null(top) && nrow(links) > 0) {
    cut <- stats::quantile(abs(links$value), 1 - top / 100, names = FALSE)
    links <- links[abs(links$value) >= cut, , drop = FALSE]
  }
  if (nrow(links) == 0) message("No links to draw.")
  # Weakest first, so the strongest ribbons are drawn on top
  links <- links[order(abs(links$value)), , drop = FALSE]
  links$link <- seq_len(nrow(links))
  links
}

# Start and end angle (degrees, anticlockwise from the x axis) of each sector.
# Sectors run clockwise from the top, with wider gaps between groups.
chord_sectors <- function(k, groups, gap, group.gap) {
  gaps <- rep(gap, k)
  if (!is.null(groups)) {
    next_group <- c(as.character(groups)[-1], as.character(groups)[1])
    gaps[as.character(groups) != next_group] <- group.gap
  }
  width <- (360 - sum(gaps)) / k
  start <- 90 - gaps[k] / 2 - c(0, cumsum(width + gaps)[-k])
  data.frame(start = start, end = start - width)
}

# Ribbon polygons for each link. Each sector is split into equal slots, one
# per link end, ordered so links to sectors just before it come first and
# links to sectors just after it come last; a link within a sector takes the
# first and last slots.
chord_ribbons <- function(links, sectors) {
  if (nrow(links) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), link = integer(0), value = numeric(0)))
  }
  k <- nrow(sectors)
  ends <- rbind(data.frame(link = links$link, sector = links$from, other = links$to, end = 1),
                data.frame(link = links$link, sector = links$to, other = links$from, end = 2))
  self <- links$from[match(ends$link, links$link)] == links$to[match(ends$link, links$link)]
  rel <- (ends$other - ends$sector) %% k
  # Self links: one end first (before everything), the other last
  ends$key <- ifelse(self, ifelse(ends$end == 1, Inf, -Inf), rel)
  ends <- ends[order(ends$sector, -ends$key), ]
  ends$slot <- stats::ave(ends$link, ends$sector, FUN = seq_along)
  ends$n <- stats::ave(ends$link, ends$sector, FUN = length)

  width <- sectors$start - sectors$end
  pad <- width * 0.02
  slot_w <- (width - 2 * pad) / ends$n[match(seq_len(k), ends$sector)]
  ends$a1 <- sectors$start[ends$sector] - pad[ends$sector] - (ends$slot - 1) * slot_w[ends$sector]
  ends$a2 <- ends$a1 - slot_w[ends$sector] * 0.95

  polys <- lapply(seq_len(nrow(links)), function(l) {
    e <- ends[ends$link == links$link[l], ]
    a <- e[e$end == 1, ]
    b <- e[e$end == 2, ]
    if (a$sector == b$sector) {
      # A loop within one sector: the inner edge bends less than the outer
      # edge, so the two don't touch
      mid <- unit_point(mean(c(a$a1, b$a2)))
      inner <- 0.6 * mid
      outer <- 0.4 * mid
    } else {
      inner <- outer <- c(0, 0)
    }
    pts <- rbind(arc_points(1, a$a1, a$a2),
                 bezier_points(unit_point(a$a2), inner, unit_point(b$a1)),
                 arc_points(1, b$a1, b$a2),
                 bezier_points(unit_point(b$a2), outer, unit_point(a$a1)))
    data.frame(x = pts[, 1], y = pts[, 2], link = links$link[l], value = links$value[l])
  })
  do.call(rbind, polys)
}

unit_point <- function(angle) c(cos(angle * pi / 180), sin(angle * pi / 180))

arc_points <- function(r, from, to, n = 30) {
  a <- seq(from, to, length.out = n) * pi / 180
  cbind(r * cos(a), r * sin(a))
}

bezier_points <- function(p0, p1, p2, n = 40) {
  t <- seq(0, 1, length.out = n)
  cbind((1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
        (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2])
}

# Annular sectors between radii r1 and r2, one polygon per sector (id).
ring_polygons <- function(start, end, r1, r2) {
  do.call(rbind, lapply(seq_along(start), function(i) {
    pts <- rbind(arc_points(r2, start[i], end[i]), arc_points(r1, end[i], start[i]))
    data.frame(x = pts[, 1], y = pts[, 2], id = i)
  }))
}

# Start and end angles of each group of sectors.
group_spans <- function(sectors, groups) {
  g <- as.character(groups)
  spans <- lapply(unique(g), function(x) {
    data.frame(group = x, start = max(sectors$start[g == x]), end = min(sectors$end[g == x]))
  })
  do.call(rbind, spans)
}

# Labels written along the circle at the given angles, upright on both halves.
tangential_labels <- function(angle, r, label, outside = TRUE) {
  rot <- angle - 90
  flip <- ((rot %% 360) > 90) & ((rot %% 360) < 270)
  rot[flip] <- rot[flip] + 180
  data.frame(x = r * cos(angle * pi / 180), y = r * sin(angle * pi / 180),
             label = label, angle = rot %% 360)
}

# Network names without their group's name, e.g. "DefaultA" in "Default" -> "A".
short_network_names <- function(nets, groups) {
  short <- ifelse(startsWith(nets, groups) & nchar(nets) > nchar(groups),
                  substring(nets, nchar(groups) + 1), nets)
  sub("^[_ .-]+", "", short)
}

# Black or white text, whichever reads better on each colour.
label_colours <- function(colours) {
  rgb <- grDevices::col2rgb(colours) / 255
  luminance <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
  ifelse(luminance > 0.5, "black", "white")
}

utils::globalVariables(c("link", "id"))
