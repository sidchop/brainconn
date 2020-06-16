#' Title
#'
#' @return
#' @export
#'
#' @examples
list_atlases <- function() {
  d <- data(package = "brainconn")
  atlas.list <- d$results[,3]
  atlas.list <- atlas.list[!grepl("ICBM",  atlas.list)]
  message("Brainconn currently contains the following atlases (you can add your own using the make_atlas() function): ")
  for (a in 1:length(atlas.list)) {
    message(atlas.list[a])
  }
}
