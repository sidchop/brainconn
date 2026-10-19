#' Simulated functional connectivity matrix (Schaefer 300, 7 networks)
#'
#' A simulated (not real) 300 x 300 functional connectivity matrix for the
#' \code{schaefer300_n7} atlas, for trying out \code{\link{brainconn_matrix}},
#' \code{\link{network_matrix}} and other functions on a dense, signed matrix.
#' Values are network block means, loosely based on typical resting-state
#' patterns (e.g. the default network is anti-correlated with the attention
#' networks), plus noise. The diagonal is 1. See \code{data-raw/} in the
#' package source for how it was made.
#'
#' @format A 300 x 300 symmetric numeric matrix with values between -1 and 1,
#'   in the region order of \code{schaefer300_n7}.
"example_fc_schaefer300"
