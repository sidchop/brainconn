#' List the included atlases
#'
#' Prints the names of the atlases included with brainconn, which can be
#' passed as the \code{atlas} argument of \code{brainconn()} and
#' \code{brainconn3D()}.
#'
#' @return Invisibly, a character vector of atlas names.
#' @importFrom utils data
#' @export
#'
#' @examples
#' list_atlases()
list_atlases <- function() {
  atlas.list <- data(package = "brainconn")$results[, "Item"]
  atlas.list <- atlas.list[!grepl("ICBM|example", atlas.list)]
  message("Brainconn currently contains the following atlases:")
  message(paste(atlas.list, collapse = "\n"))
  message("You can also input your own custom atlas, as long as it meets certain requirements. See vignette and use the check_atlas() function to make sure the requirements are met.")
  invisible(atlas.list)
}
