#' Title
#'
#' @param atlas a .data.frame with a 4 columns named: c("ROI.Name", "x.mni", "y.mni", "z.mni"), see \code{vignette("brainconn")}
#' @return a message
#'@export
#' @examples
#' \dontrun{
#'library(brainconn)
#'check_atlas(custom_atlas_example)
#'}
#'
#===================================
# This function checks user input atlas to make sure it will play nice with the
# brainconn and brainconn3d functions.
#===================================
check_atlas <- function(atlas) {
  if(!is.data.frame(atlas)) {
    message("Please convert atlas to a dataframe (e.g. as.dataframe())")
    return(invisible(NULL))
  }
  essential_cols <-  c("ROI.Name", "x.mni", "y.mni", "z.mni")
  col.check <- essential_cols %in% names(atlas)
  pass <- !any(col.check == FALSE)
  if(!pass){
    stop(paste("File missing", essential_cols[which(col.check == FALSE)], "column."))
  }
  pass <- is.integer(atlas$x.mni) & is.integer(atlas$y.mni) & is.integer(atlas$z.mni)
  if(!pass){
    stop("x.mni, y.mni and z.mni columns need to be integers.")
  }

  if(pass){
    message("Atlas fits brainconn specifications and should work with brainconn() and brainconn3d().")
  }
}

