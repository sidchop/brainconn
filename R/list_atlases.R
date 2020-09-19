#' Title
#'
#' @return
#' @export
#'
#' @examples
list_atlases <- function() {
  d <- data(package = "brainconn")
  atlas.list <- d[[3]]
  atlas.list <- atlas.list[,3]
  atlas.list <- atlas.list[!grepl("ICBM",  atlas.list)]
  atlas.list <- atlas.list[!grepl("example",  atlas.list)]
  message("Brainconn currently contains the following atlases:")
  for (a in 1:length(atlas.list)) {
    message(atlas.list[a])
  }
  message("You can also input your own custom atlas, as along as it meets certain requirement. See vignette and use the check_atlas() function to make sure the requirements are met.")
}
