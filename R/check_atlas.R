#' Title
#'
#' @param atlas a .data.frame with a 4 collums named: c("ROI.Name", "x.mni", "y.mni", "z.mni"), see \code{vignette("brainconn")}
#' @return a message
#'@export
#' @examples
#' \dontrun{
#'library(brainconn)
#'check_atlas(custom_atlas_example)
#'}
#'
#===================================
# This function checks user inputed atlas to make sure it will play nice with the
# brainconn and brainconn3d functions.
#===================================
check_atlas <- function(atlas) {
  if(!is.data.frame(atlas)) {message("Please convert atlas to a dataframe (e.g. as.dataframe())")}
  essential_cols <-  c("ROI.Name", "x.mni", "y.mni", "z.mni")
  col.check <- essential_cols %in% names(atlas)
  ifelse(any(col.check==F), pass <- F, pass <- T)
  if(pass == F){
    stop(paste("File missing", essential_cols[which(col.check == F)], "column."))
  }
  ifelse(is.integer(atlas$x.mni) & is.integer(atlas$y.mni) & is.integer(atlas$x.mni),  pass <- T, pass <- F)
  if(pass == F){
    stop("x.mni, y.mni and z.mni columns need to be integers.")
  }

  if(pass == T){
    message("Atlas fits brainconn specifications and should work with brainconn() and brainconn3d().")
  }
  if(pass == F){
    stop(paste("File missing", essential_cols[which(col.check == F)], "column."))
  }
}

