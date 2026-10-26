# The README animation of brainconn3D(): the brain rotates, a node is hovered
# (tooltip) and clicked (highlight), and the highlight is cleared. Recorded in
# headless Chrome with chromote and encoded with gifski (both only needed here).
devtools::load_all()
library(chromote)
out <- "man/figures/README-brainconn3D.gif"
width <- 960
height <- 600

# The 250 strongest connections of the example functional connectivity matrix
fc <- example_fc_schaefer300
diag(fc) <- 0
cutoff <- sort(abs(fc[upper.tri(fc)]), decreasing = TRUE)[250]
fc[abs(fc) < cutoff] <- 0
p <- brainconn3D(atlas = "schaefer300_n7", conmat = fc, highlight = TRUE,
                 edge.color.weighted = TRUE, show.legend = TRUE, edge.width = 2)
p <- plotly::config(p, displayModeBar = FALSE)
page <- file.path(tempdir(), "readme_gif.html")
htmlwidgets::saveWidget(p, page, selfcontained = FALSE)

chromote::set_chrome_args(c(chromote::default_chrome_args(), "--use-angle=swiftshader",
                            "--enable-unsafe-swiftshader"))
b <- ChromoteSession$new(width = width, height = height)
b$Page$navigate(paste0("file://", page))
Sys.sleep(6)
js <- function(code) b$Runtime$evaluate(code, awaitPromise = TRUE)$result$value

# A drawn mouse pointer (screenshots don't include the real one)
js("var c = document.createElement('div'); c.id = 'fake-cursor';
    c.style.cssText = 'position:fixed;left:-50px;top:-50px;width:22px;height:22px;z-index:2000;pointer-events:none';
    c.innerHTML = '<svg width=\"22\" height=\"22\" viewBox=\"0 0 22 22\"><path d=\"M2 2 L2 18 L6.5 13.5 L10 21 L13 19.5 L9.5 12.5 L16 12.5 Z\" fill=\"black\" stroke=\"white\" stroke-width=\"1.5\"/></svg>';
    document.body.appendChild(c); 1")
cursor <- c(width - 60, height - 60)
move <- function(x, y) {
  cursor <<- c(x, y)
  js(sprintf("var c = document.getElementById('fake-cursor'); c.style.left = '%fpx'; c.style.top = '%fpx'; 1", x, y))
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
}
click <- function() {
  b$Input$dispatchMouseEvent(type = "mousePressed", x = cursor[1], y = cursor[2], button = "left", clickCount = 1)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = cursor[1], y = cursor[2], button = "left", clickCount = 1)
}
camera <- function(angle, elevation = 0.55, r = 1.9) {
  js(sprintf("Plotly.relayout(document.querySelector('.js-plotly-plot'),
               {'scene.camera.eye': {x: %f, y: %f, z: %f}}).then(function() { return 1; })",
             r * cos(angle), r * sin(angle), elevation))
}

# After a scripted camera change, the scene's camera takes a moment to settle;
# wait until it stops moving before working out where nodes are on screen
settle <- function() {
  view <- function() js("Array.prototype.join.call(document.querySelector('.js-plotly-plot')._fullLayout.scene._scene.glplot.cameraParams.view)")
  last <- view()
  for (i in 1:30) {
    Sys.sleep(0.2)
    now <- view()
    if (identical(now, last)) break
    last <- now
  }
}

frames <- character(0)
frame_dir <- Sys.getenv("GIF_FRAMES", tempdir())
snap <- function(n = 1) {
  Sys.sleep(0.15)
  shot <- b$Page$captureScreenshot(format = "png")$data
  for (i in seq_len(n)) {
    f <- file.path(frame_dir, sprintf("frame%04d.png", length(frames) + 1))
    writeBin(jsonlite::base64_dec(shot), f)
    frames <<- c(frames, f)
  }
}

# 1. A full turn
start <- -2.3
for (a in seq(start, start + 2 * pi, length.out = 40)[-40]) { camera(a); snap() }
camera(start)
settle()

# 2. Hover over a well-connected node that is in front at this angle (the
#    plot picks the node nearest the viewer under the pointer)
shown <- rowSums(fc != 0) > 0
degree <- rowSums(fc[shown, shown] != 0)
screen <- function(i) {
  jsonlite::fromJSON(js(sprintf(
    "JSON.stringify(document.querySelector('.js-plotly-plot').brainconnScreen(%d))", i)))
}
picked <- function(i, s) {
  identical(js(sprintf("String(document.querySelector('.js-plotly-plot').brainconnNodeAt(%f, %f))",
                       s$x, s$y)), as.character(i))
}
for (hub in order(-degree) - 1) {
  target <- screen(hub)
  if (target$x > 150 && target$x < width - 250 && picked(hub, target)) break
}
message("hub: node ", hub, " with ", degree[hub + 1], " connections")
from <- cursor
for (t in seq(0, 1, length.out = 14)[-1]) {
  move(from[1] + (target$x - from[1]) * t, from[2] + (target$y - from[2]) * t)
  snap()
}
snap(8)

# 3. Click: only its connections stay
click()
snap(14)

# 4. Turn while highlighted (moving the pointer off the node first)
move(width - 60, 80)
# (turning towards the back of the brain keeps the highlighted node in front)
for (a in seq(start, start + pi / 2.5, length.out = 16)[-1]) { camera(a); snap() }
snap(6)

# 5. Escape shows everything again
b$Input$dispatchKeyEvent(type = "keyDown", key = "Escape", code = "Escape")
b$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape")
snap(12)
b$close()

gifski::gifski(frames, out, width = width, height = height, delay = 0.09, progress = FALSE)
message(out, ": ", round(file.size(out) / 1e6, 1), " MB, ", length(frames), " frames")
