# Open a saved htmlwidget in headless Chrome (via chromote) and return the
# session, so tests can send real mouse and keyboard events and inspect the
# page. Skips when chromote or Chrome is not available, on CRAN, and on CI
# (headless WebGL support varies between CI runners).
open_widget <- function(widget, width = 1000, height = 650) {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("chromote")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  testthat::skip_if(is.null(chrome) || !nzchar(chrome), "Chrome not available")

  dir <- tempfile("widget")
  dir.create(dir)
  file <- file.path(dir, "index.html")
  htmlwidgets::saveWidget(widget, file, selfcontained = FALSE)
  chromote::set_chrome_args(c(chromote::default_chrome_args(), "--use-angle=swiftshader",
                              "--enable-unsafe-swiftshader"))
  session <- chromote::ChromoteSession$new(width = width, height = height)
  session$Page$navigate(paste0("file://", file))
  # Wait for the plot to be drawn
  for (i in 1:50) {
    ready <- session$Runtime$evaluate(
      "!!(document.querySelector('.js-plotly-plot') && document.querySelector('.js-plotly-plot')._fullLayout)"
    )$result$value
    if (isTRUE(ready)) break
    Sys.sleep(0.2)
  }
  Sys.sleep(1)
  session
}

# Evaluate a JavaScript expression on the page; the value is returned as JSON
# and parsed. `gd` refers to the plot.
page_eval <- function(session, js) {
  out <- session$Runtime$evaluate(paste0(
    "JSON.stringify((function(){ var gd = document.querySelector('.js-plotly-plot'); return ",
    js, "; })())"))
  jsonlite::fromJSON(out$result$value, simplifyVector = TRUE)
}

mouse_move <- function(session, x, y) {
  session$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
  Sys.sleep(0.2)
}

mouse_click <- function(session, x, y) {
  session$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = "left",
                                    clickCount = 1)
  session$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button = "left",
                                   clickCount = 1)
  Sys.sleep(0.3)
}

press_escape <- function(session) {
  session$Input$dispatchKeyEvent(type = "keyDown", key = "Escape", code = "Escape")
  session$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape")
  Sys.sleep(0.3)
}
