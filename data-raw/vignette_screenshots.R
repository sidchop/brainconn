# Screenshots of brainconn3D() for the vignettes, with a node clicked so its
# connections are highlighted. Taken in headless Chrome (chromote, only needed
# here) at twice the pixel density, so they are sharp on high-resolution screens.
devtools::load_all()
library(chromote)
chromote::set_chrome_args(c(chromote::default_chrome_args(), "--use-angle=swiftshader",
                            "--enable-unsafe-swiftshader"))

# Open the plot, click its best-connected node (one that is in front, so the
# click reaches it) and save a screenshot
screenshot_highlight <- function(p, conmat, file, width = 900, height = 560) {
  p <- plotly::config(p, displayModeBar = FALSE)
  page <- file.path(tempdir(), "screenshot.html")
  htmlwidgets::saveWidget(p, page, selfcontained = FALSE)
  b <- ChromoteSession$new(width = width, height = height)
  on.exit(b$close())
  b$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = 2,
                                       mobile = FALSE)
  b$Page$navigate(paste0("file://", page))
  Sys.sleep(6)
  js <- function(code) b$Runtime$evaluate(code)$result$value
  shown <- rowSums(conmat != 0) > 0
  degree <- rowSums(conmat[shown, shown] != 0)
  for (node in order(-degree) - 1) {
    s <- jsonlite::fromJSON(js(sprintf(
      "JSON.stringify(document.querySelector('.js-plotly-plot').brainconnScreen(%d))", node)))
    hit <- js(sprintf("String(document.querySelector('.js-plotly-plot').brainconnNodeAt(%f, %f))",
                      s$x, s$y))
    if (s$x > 100 && s$x < width - 300 && identical(hit, as.character(node))) break
  }
  for (type in c("mousePressed", "mouseReleased")) {
    b$Input$dispatchMouseEvent(type = type, x = s$x, y = s$y, button = "left", clickCount = 1)
  }
  Sys.sleep(0.5)
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = s$x, y = s$y)
  Sys.sleep(0.5)
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), file)
  message(file, ": node ", node, " (", degree[node + 1], " connections)")
}

# Human brain: a random signed network on schaefer100_n7
set.seed(3)
m <- matrix(0, 100, 100)
m[sample(which(upper.tri(m)), 250)] <- runif(250, -1, 1)
m <- m + t(m)
screenshot_highlight(
  brainconn3D("schaefer100_n7", conmat = m, edge.color.weighted = TRUE, highlight = TRUE,
              show.legend = TRUE, edge.width = 2),
  m, "vignettes/figures/brainconn3D-highlight.png")

# Rat brain: the random network of the "other brains" vignette
set.seed(2)
n <- nrow(rat_atlas)
rat_conmat <- matrix(0, n, n)
rat_conmat[sample(which(upper.tri(rat_conmat)), 60)] <- runif(60, 0.2, 1)
rat_conmat <- rat_conmat + t(rat_conmat)
p <- brainconn3D(atlas = rat_atlas, conmat = rat_conmat, mesh = rat_mesh, highlight = TRUE,
                 show.legend = TRUE)
p <- plotly::layout(p, scene = list(camera = list(eye = list(x = -1.4, y = 0.9, z = 0.9))))
screenshot_highlight(p, rat_conmat, "vignettes/figures/rat-interactive.png")
