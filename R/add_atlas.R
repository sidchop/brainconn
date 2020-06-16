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

  ifelse(all(col.check)==T,
  message("File contains essential columns: ROI.Name, x.mni , y.mni  & z.mni"),
  stop(paste("File missing", essential_cols[which(col.check == F)], "column.")))
  usethis::use_data(atlas)
}

