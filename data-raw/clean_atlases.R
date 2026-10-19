# Fixes to the included atlases (brainconn 1.0.0):
# - schaefer300_17_cog_lang: the region name column was `ROI.name`, so the
#   plotting functions rejected it; renamed to `ROI.Name`.
# - stages_study: network names contained a non-breaking space, a byte-order
#   mark and a trailing tab ("Dorsal Attention", "Ventral Attention").
# - shen_368: region names were padded with spaces.
fix <- function(name, f) {
  file <- file.path("data", paste0(name, ".rda"))
  e <- new.env()
  load(file, envir = e)
  assign(name, f(e[[name]]), envir = e)
  save(list = name, envir = e, file = file, compress = "xz")
}
clean_text <- function(x) {
  x <- gsub(" ", " ", as.character(x))
  x <- gsub("﻿", "", x)
  trimws(x)
}
fix("schaefer300_17_cog_lang", function(d) {
  names(d)[names(d) == "ROI.name"] <- "ROI.Name"
  d
})
fix("stages_study", function(d) {
  d$network <- clean_text(d$network)
  d
})
fix("shen_368", function(d) {
  d$ROI.Name <- clean_text(d$ROI.Name)
  d
})
