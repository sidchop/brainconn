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

# Named palettes, from the lowest to the highest value. "bkr" (blue-black-red,
# from cyan through blue and black to red and yellow) is the 80-step scale used
# for chord diagrams in Chopra et al.; "bwr" is blue-white-red.
named_palettes <- list(
  bkr = c(
    "#99FFFF", "#66FFFF", "#33FFFF", "#00FFFF", "#00E5FF", "#00CCFF", "#00B2FF", "#0099FF",
    "#0080FF", "#0066FF", "#004DFF", "#0033FF", "#001AFF", "#0000FF", "#0000F5", "#0000EB",
    "#0000E2", "#0000D8", "#0000CE", "#0000C4", "#0000BA", "#0000B1", "#0000A7", "#00009D",
    "#000093", "#000089", "#000080", "#000076", "#00006C", "#000062", "#000058", "#00004E",
    "#000045", "#00003B", "#000031", "#000027", "#00001D", "#000014", "#00000A", "#000000",
    "#000000", "#0A0000", "#140000", "#1D0000", "#270000", "#310000", "#3B0000", "#450000",
    "#4E0000", "#580000", "#620000", "#6C0000", "#760000", "#800000", "#890000", "#930000",
    "#9D0000", "#A70000", "#B10000", "#BA0000", "#C40000", "#CE0000", "#D80000", "#E20000",
    "#EB0000", "#F50000", "#FF0000", "#FF1A00", "#FF3300", "#FF4D00", "#FF6600", "#FF8000",
    "#FF9900", "#FFB200", "#FFCC00", "#FFE500", "#FFFF00", "#FFFF33", "#FFFF66", "#FFFF99"
  ),
  bwr = c("blue", "white", "red")
)

# A vector of n colours, from the lowest to the highest weight, for mapping
# edge weights. `palette` may be NULL (dark-to-light blue then light-to-dark
# red for signed weights, light-to-dark red otherwise), "bkr" or "bwr", a
# palette name from grDevices::hcl.pals(), or a vector of colours to
# interpolate.
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
    if (palette %in% names(named_palettes)) {
      return(grDevices::colorRampPalette(named_palettes[[palette]])(n))
    }
    if (!palette %in% grDevices::hcl.pals()) {
      stop("Unknown palette '", palette, "'. Use \"bkr\", \"bwr\", a name from ",
           "grDevices::hcl.pals() or a vector of colours.", call. = FALSE)
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
