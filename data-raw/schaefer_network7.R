# Add a `network7` column to the Schaefer 17-network atlases: the Yeo 7
# network each 17-network belongs to (e.g. DefaultA -> Default,
# TempPar -> Default). Used by brainconn_circle(group = "network7").
network7_of <- function(network17) {
  out <- sub("^(Vis|SomMot|DorsAttn|SalVentAttn|Limbic|Cont|Default).*$", "\\1", network17)
  out[network17 == "TempPar"] <- "Default"
  out
}
files <- list.files("data", pattern = "^schaefer[0-9]+_n17\\.rda$", full.names = TRUE)
for (f in files) {
  e <- new.env()
  name <- load(f, envir = e)
  atlas <- e[[name]]
  atlas$network7 <- network7_of(atlas$network)
  stopifnot(all(atlas$network7 %in% c("Vis", "SomMot", "DorsAttn", "SalVentAttn", "Limbic",
                                       "Cont", "Default")))
  assign(name, atlas, envir = e)
  save(list = name, envir = e, file = f, compress = "xz")
  message(name, ": ", paste(unique(paste(atlas$network, "->", atlas$network7)), collapse = ", "))
}
