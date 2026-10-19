# Internal helpers shared by brainconn() and brainconn3D().

# Fetch a dataset shipped with the package (atlases, background images, mesh).
# Looks in the package's lazydata first, so it works without the package
# being attached (e.g. when called as brainconn::brainconn()).
package_data <- function(name) {
  lazydata <- getNamespaceInfo("brainconn", "lazydata")
  if (!exists(name, envir = lazydata, inherits = FALSE)) {
    stop("'", name, "' is not a dataset in brainconn.", call. = FALSE)
  }
  get(name, envir = lazydata, inherits = FALSE)
}

# Return the atlas data.frame given either a data.frame or the name of an
# included atlas (or of a data.frame in the caller's environment).
resolve_atlas <- function(atlas, env = parent.frame()) {
  if (is.character(atlas) && length(atlas) == 1) {
    lazydata <- getNamespaceInfo("brainconn", "lazydata")
    if (exists(atlas, envir = lazydata, inherits = FALSE)) {
      atlas <- get(atlas, envir = lazydata, inherits = FALSE)
    } else if (exists(atlas, envir = env)) {
      atlas <- get(atlas, envir = env)
    } else {
      stop("Atlas '", atlas, "' not found. See list_atlases() for the included atlases.",
           call. = FALSE)
    }
  }
  if (!is.data.frame(atlas)) {
    stop("`atlas` must be the name of an included atlas or a data.frame.", call. = FALSE)
  }
  missing_cols <- setdiff(c("ROI.Name", "x.mni", "y.mni", "z.mni"), names(atlas))
  if (length(missing_cols) > 0) {
    stop("Atlas is missing column(s): ", paste(missing_cols, collapse = ", "),
         ". See check_atlas().", call. = FALSE)
  }
  atlas
}

# Coerce conmat to a numeric n x n matrix with NAs set to 0 and thresholds
# applied. A NULL conmat becomes an empty (all-zero) matrix.
prepare_conmat <- function(conmat, n, thr = NULL, uthr = NULL) {
  if (is.null(conmat)) {
    return(matrix(0, nrow = n, ncol = n))
  }
  conmat <- as.matrix(conmat)
  if (!is.numeric(conmat) && !is.logical(conmat)) {
    stop("`conmat` must be a numeric matrix or data.frame.", call. = FALSE)
  }
  if (nrow(conmat) != ncol(conmat)) {
    stop("`conmat` must be square, but is ", nrow(conmat), " x ", ncol(conmat), ".",
         call. = FALSE)
  }
  if (nrow(conmat) != n) {
    stop("`conmat` is ", nrow(conmat), " x ", ncol(conmat), " but the atlas has ", n,
         " regions.", call. = FALSE)
  }
  storage.mode(conmat) <- "double"
  dimnames(conmat) <- NULL
  conmat[is.na(conmat)] <- 0
  if (!is.null(thr)) conmat[conmat < thr] <- 0
  if (!is.null(uthr)) conmat[conmat > uthr] <- 0
  conmat
}

# Logical vector of the nodes to plot: all of them, or only those with at
# least one incoming or outgoing edge. Falls back to all nodes (with a message)
# when there are no edges, so the plot is never empty.
nodes_to_keep <- function(conmat, all.nodes) {
  if (all.nodes) {
    return(rep(TRUE, nrow(conmat)))
  }
  has_edge <- rowSums(conmat != 0) > 0 | colSums(conmat != 0) > 0
  if (!any(has_edge)) {
    message("No edges to plot; showing all nodes.")
    return(rep(TRUE, nrow(conmat)))
  }
  has_edge
}

# node.size may be a single value, one value per atlas region, or one value
# per plotted node. Returns one value per plotted node.
match_node_size <- function(node.size, keep) {
  if (length(node.size) == 1 || length(node.size) == sum(keep)) {
    return(node.size)
  }
  if (length(node.size) == length(keep)) {
    return(node.size[keep])
  }
  stop("`node.size` must have length 1, the number of regions in the atlas (",
       length(keep), "), or the number of plotted nodes (", sum(keep), ").",
       call. = FALSE)
}

# Use the atlas network column for node colours if asked and available.
resolve_node_color <- function(node.color, data) {
  if (identical(node.color, "network") && !"network" %in% names(data)) {
    message("Atlas has no 'network' column; colouring all nodes black.")
    return("black")
  }
  node.color
}

# Raster grob for the background image of a view, mirrored if the view is.
background_grob <- function(background, view, alpha = 1) {
  if (identical(background, "ICBM152")) {
    m <- package_data(paste0("ICBM152_", view))
  } else {
    m <- readImage(background)
  }
  if (length(dim(m)) != 3 || !dim(m)[3] %in% c(3, 4)) {
    stop("Background image must be RGB or RGBA.", call. = FALSE)
  }
  if (view_geometry[[view]]$mirror) m <- m[, rev(seq_len(dim(m)[2])), , drop = FALSE]
  a <- if (dim(m)[3] == 4) m[, , 4] else 1
  w <- matrix(rgb(m[, , 1], m[, , 2], m[, , 3], a * alpha), nrow = dim(m)[1])
  rasterGrob(w)
}
