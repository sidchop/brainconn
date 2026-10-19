#' Check a custom atlas
#'
#' Checks that a custom atlas meets the requirements of \code{brainconn()} and
#' \code{brainconn3D()}.
#'
#' @param atlas a data.frame with at least 4 columns named: c("ROI.Name", "x.mni", "y.mni", "z.mni"), see \code{vignette("brainconn")}. An optional "network" column is used to colour nodes.
#' @return Invisibly, \code{TRUE} if the atlas passes and \code{FALSE} if it is not a data.frame. Errors if columns are missing or not numeric.
#' @export
#' @examples
#' \dontrun{
#' library(brainconn)
#' check_atlas(custom_atlas_example)
#' }
check_atlas <- function(atlas) {
  if (!is.data.frame(atlas)) {
    message("Please convert atlas to a dataframe (e.g. as.data.frame())")
    return(invisible(FALSE))
  }
  essential_cols <- c("ROI.Name", "x.mni", "y.mni", "z.mni")
  missing_cols <- setdiff(essential_cols, names(atlas))
  if (length(missing_cols) > 0) {
    stop(paste("File missing", missing_cols, "column."))
  }
  if (!all(vapply(atlas[c("x.mni", "y.mni", "z.mni")], is.numeric, logical(1)))) {
    stop("x.mni, y.mni and z.mni columns need to be numeric.")
  }
  message("Atlas fits brainconn specifications and should work with brainconn() and brainconn3D().")
  invisible(TRUE)
}
