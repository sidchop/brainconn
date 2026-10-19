#' Summarise a connectivity matrix by network
#'
#' Summarises a region x region connectivity matrix into a network x network
#' matrix, e.g. the mean connectivity within and between each pair of
#' networks, or the number or density of edges.
#'
#' Self-connections (the diagonal) are left out. For an undirected
#' (symmetric) matrix, each edge within a network is counted once; for a
#' directed matrix, the value for networks A and B summarises the edges from
#' regions in A to regions in B.
#'
#' @param conmat A square region x region matrix (or data.frame).
#' @param networks The network of each region: a vector with one value per
#'   region, or an atlas (a data.frame with a \code{network} column, or the
#'   name of an included atlas). Networks are ordered by factor level, or by
#'   first appearance.
#' @param fun How to summarise the edges of each pair of networks:
#'   \describe{
#'     \item{"mean"}{mean value over all region pairs (zeros included)}
#'     \item{"sum"}{sum of values}
#'     \item{"count"}{number of non-zero edges}
#'     \item{"density"}{proportion of region pairs with a non-zero edge}
#'     \item{"median"}{median value over all region pairs}
#'   }
#'   or a function that takes a vector of values and returns one number.
#' @param full If \code{TRUE}, return a region x region matrix in which each
#'   cell holds the summary for its pair of networks (a "smoothed" matrix,
#'   useful for plotting). The diagonal holds the within-network summary.
#' @return A numeric matrix with network names as dimnames, or a region x
#'   region matrix when \code{full = TRUE}.
#' @export
#' @examples
#' net <- network_matrix(example_fc_schaefer300, "schaefer300_n7")
#' round(net, 2)
#'
#' # Number of edges within and between networks
#' network_matrix(example_unweighted_undirected, "schaefer300_n7", fun = "count")
network_matrix <- function(conmat, networks, fun = "mean", full = FALSE) {
  conmat <- as.matrix(conmat)
  storage.mode(conmat) <- "double"
  if (nrow(conmat) != ncol(conmat)) stop("`conmat` must be square.", call. = FALSE)
  networks <- resolve_networks(networks, nrow(conmat))
  fun <- summary_function(fun)

  directed <- !isSymmetric(unname(conmat))
  groups <- levels(networks)
  idx <- split(seq_along(networks), networks)
  out <- matrix(NA_real_, length(groups), length(groups), dimnames = list(groups, groups))
  for (a in groups) {
    for (b in groups) {
      if (!directed && match(b, groups) < match(a, groups)) next
      block <- conmat[idx[[a]], idx[[b]], drop = FALSE]
      values <- if (a == b) {
        # Leave out self-connections; undirected edges counted once
        if (directed) block[row(block) != col(block)] else block[upper.tri(block)]
      } else {
        as.vector(block)
      }
      out[a, b] <- if (length(values) == 0) NA_real_ else fun(values)
      if (!directed) out[b, a] <- out[a, b]
    }
  }
  if (!full) return(out)
  smoothed <- out[as.integer(networks), as.integer(networks)]
  dimnames(smoothed) <- dimnames(conmat)
  smoothed
}

# A factor of network memberships, one per region, from a vector or an atlas.
resolve_networks <- function(networks, n) {
  if (is.data.frame(networks) || (is.character(networks) && length(networks) == 1 && n > 1)) {
    atlas <- resolve_atlas(networks, parent.frame(2))
    if (!"network" %in% names(atlas)) {
      stop("The atlas has no `network` column.", call. = FALSE)
    }
    networks <- atlas$network
  }
  if (length(networks) != n) {
    stop("`networks` has ", length(networks), " values but the matrix has ", n, " regions.",
         call. = FALSE)
  }
  if (anyNA(networks)) stop("`networks` must not contain NA.", call. = FALSE)
  if (is.factor(networks)) droplevels(networks) else
    factor(networks, levels = unique(as.character(networks)))
}

summary_function <- function(fun) {
  if (is.function(fun)) return(fun)
  switch(match.arg(fun, c("mean", "sum", "count", "density", "median")),
         mean = function(v) mean(v, na.rm = TRUE),
         sum = function(v) sum(v, na.rm = TRUE),
         count = function(v) sum(v != 0, na.rm = TRUE),
         density = function(v) mean(v != 0, na.rm = TRUE),
         median = function(v) stats::median(v, na.rm = TRUE))
}
