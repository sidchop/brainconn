#' Title
#'
#' @param atlas
#'
#' @return
#' @export
#'
#' @examples
#'
#===================================
# This function converts user inputed atlas
# first checks collums names and then converts
# it into .rda file that can be accessed by brain conn
#===================================
add_atlas <- function(atlas) {
  stopifnot(is.data.frame(atlas))
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
    usethis::use_data(atlas, internal = FALSE, overwrite = TRUE)
    message("Atlas added to brainconn. Use list_atlases() to see all atlases. Reloading brainconn..")
    detach("package:brainconn", unload=TRUE)
    library("brainconn")
  }
  if(pass == F){
    stop(paste("File missing", essential_cols[which(col.check == F)], "column."))
  }
}

