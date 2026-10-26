# The GIFs in vignettes/animations.Rmd. The vignette's own code is run here, so
# the GIFs are exactly what it makes: every R chunk is run (including the
# eval = FALSE ones that write the GIFs), except those that only show figures.
# Needs gifski (only for this script).
devtools::load_all()
rmd <- readLines("vignettes/animations.Rmd")
starts <- grep("^```\\{r", rmd)
ends <- grep("^```$", rmd)
chunks <- lapply(seq_along(starts), function(k) {
  rmd[(starts[k] + 1):(ends[ends > starts[k]][1] - 1)]
})
headers <- rmd[starts]
run <- !grepl("setup|include_graphics", headers) &
  !vapply(chunks, function(code) any(grepl("include_graphics", code)), logical(1))
old <- setwd(tempdir())
for (code in chunks[run]) eval(parse(text = code), envir = globalenv())
setwd(old)
file.copy(file.path(tempdir(), "spread.gif"), "vignettes/figures/animation-spread.gif",
          overwrite = TRUE)
file.copy(file.path(tempdir(), "wave.gif"), "vignettes/figures/animation-wave.gif",
          overwrite = TRUE)
for (f in c("animation-spread.gif", "animation-wave.gif")) {
  message(f, ": ", round(file.size(file.path("vignettes/figures", f)) / 1e6, 1), " MB")
}
