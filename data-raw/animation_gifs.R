# The GIFs in vignettes/animations.Rmd. The vignette's own code is run here, so
# the GIFs are exactly what it makes: every R chunk is run (including the
# eval = FALSE ones that write the GIFs), except those that only show figures.
# Needs gifski, and rayrender for the bounce GIF (only for this script).
# Making the bounce GIF takes about 10 minutes; leave it out of `make` to skip it.
make <- c("spread", "wave", "bounce")

devtools::load_all()
rmd <- readLines("vignettes/animations.Rmd")
starts <- grep("^```\\{r", rmd)
ends <- grep("^```$", rmd)
chunks <- lapply(seq_along(starts), function(k) {
  rmd[(starts[k] + 1):(ends[ends > starts[k]][1] - 1)]
})
headers <- rmd[starts]
uses <- function(word) vapply(chunks, function(code) any(grepl(word, code)), logical(1))
run <- !grepl("setup", headers) & !uses("include_graphics")
for (gif in setdiff(c("spread", "wave", "bounce"), make)) run <- run & !uses(paste0(gif, "\\.gif"))
old <- setwd(tempdir())
for (code in chunks[run]) eval(parse(text = code), envir = globalenv())
setwd(old)
for (gif in make) {
  file <- file.path("vignettes/figures", paste0("animation-", gif, ".gif"))
  file.copy(file.path(tempdir(), paste0(gif, ".gif")), file, overwrite = TRUE)
  message(file, ": ", round(file.size(file) / 1e6, 1), " MB")
}
