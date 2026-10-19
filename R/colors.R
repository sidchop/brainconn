# Network and edge colours shared by the plotting functions.

# Standard Yeo/Schaefer network colours (CBIG lookup tables), keyed by a
# normalised network name (lower case, letters and digits only). Common
# alternative names for the 7 networks map to the same colours.
yeo_colors <- c(
  # Yeo 7 networks
  vis = "#781286", visual = "#781286",
  sommot = "#4682B4", somatomotor = "#4682B4", sensorimotor = "#4682B4",
  dorsattn = "#00760E", dorsalattention = "#00760E",
  salventattn = "#C43AFA", ventralattention = "#C43AFA", salience = "#C43AFA",
  limbic = "#DCF8A4",
  cont = "#E69422", control = "#E69422", frontoparietal = "#E69422",
  default = "#CD3E4E", defaultmode = "#CD3E4E",
  subcortex = "#BF9959", subcortical = "#BF9959", subcort = "#BF9959",
  # Yeo 17 networks
  viscent = "#781286", visperi = "#FF0000",
  sommota = "#4682B4", sommotb = "#2ACCA4",
  dorsattna = "#4A9B3C", dorsattnb = "#00760E",
  salventattna = "#C43AFA", salventattnb = "#FF98D5",
  limbica = "#DCF8A4", limbicb = "#7A8732",
  conta = "#E69422", contb = "#87324A", contc = "#778CB0",
  defaulta = "#FFFF00", defaultb = "#CD3E4E", defaultc = "#000082",
  temppar = "#0C30FF"
)

normalise_network_name <- function(x) {
  gsub("[^a-z0-9]", "", tolower(x))
}

#' Colours for brain networks
#'
#' Returns one colour per network. When every network name is a Yeo 7 or
#' Yeo 17 network (as in the Schaefer atlases, e.g. "Default", "DefaultA",
#' "SalVentAttn"), or a common alternative name for one (e.g.
#' "Frontoparietal", "Dorsal Attention", "Subcortical"), the standard Yeo
#' colours are used. Otherwise colours are spaced evenly around the colour
#' wheel, as in ggplot2.
#'
#' All brainconn plots colour networks with this function, so a network has
#' the same colour in every plot.
#'
#' @param networks A vector of network names, e.g. the \code{network} column
#'   of an atlas. Duplicates are allowed.
#' @return A named character vector of colours, one per unique network, in
#'   the order the networks first appear (or the factor level order).
#' @export
#' @examples
#' network_colors(schaefer300_n7$network)
#' network_colors(c("Frontal", "Temporal", "Occipital"))
network_colors <- function(networks) {
  nets <- if (is.factor(networks)) levels(droplevels(networks)) else unique(as.character(networks))
  nets <- nets[!is.na(nets)]
  cols <- yeo_colors[normalise_network_name(nets)]
  if (anyNA(cols)) cols <- scales::hue_pal()(length(nets))
  stats::setNames(unname(cols), nets)
}

# A vector of n colours, from the lowest to the highest weight, for mapping
# edge weights. `palette` may be NULL (dark-to-light blue then light-to-dark
# red for signed weights, light-to-dark red otherwise), a palette name from
# grDevices::hcl.pals(), or a vector of colours to interpolate.
edge_palette <- function(palette, signed, n = 256) {
  if (is.null(palette)) {
    # No white in the middle: weights near zero must stay visible on a pale brain
    palette <- if (signed) {
      c("#08306B", "#2171B5", "#6BAED6", "#FC9272", "#CB181D", "#67000D")
    } else {
      c("#FC9272", "#FB6A4A", "#DE2D26", "#A50F15", "#67000D")
    }
  }
  if (length(palette) == 1) {
    if (!palette %in% grDevices::hcl.pals()) {
      stop("Unknown palette '", palette, "'. Use a name from grDevices::hcl.pals() ",
           "or a vector of colours.", call. = FALSE)
    }
    return(grDevices::hcl.colors(n, palette))
  }
  grDevices::colorRampPalette(palette)(n)
}

# Colour limits for edge weights: symmetric around zero when weights are
# signed, otherwise the range of the weights.
edge_limits <- function(weights, limits = NULL) {
  if (!is.null(limits)) return(limits)
  if (any(weights < 0)) return(c(-1, 1) * max(abs(weights)))
  r <- range(weights)
  if (r[1] == r[2]) r <- r + c(-0.5, 0.5)
  r
}

# Map weights to colours from `colors` over `limits` (values outside are clamped).
map_colors <- function(weights, colors, limits) {
  pos <- (pmin(pmax(weights, limits[1]), limits[2]) - limits[1]) / diff(limits)
  colors[round(pos * (length(colors) - 1)) + 1]
}
