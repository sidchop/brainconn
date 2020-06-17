#' Title
#'
#' @return
#' @export
#'
#' @examples
list_atlases <- function() {
  d <- data(package = "brainconn")
  atlas.list <- ls(readRDS(system.file("data", "Rdata.rds", package = "brainconn")))
  atlas.list <- atlas.list[!grepl("ICBM",  atlas.list)]
  message("Brainconn currently contains the following atlases (you can add your own using the make_atlas() function): ")
  for (a in 1:length(atlas.list)) {
    message(atlas.list[a])
  }
}
