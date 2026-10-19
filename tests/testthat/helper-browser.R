# Run JavaScript against a saved htmlwidget in headless Chrome and return the
# JSON result. `js` is the body of function(el, x, data) run after the widget
# renders; it must return a value, which is written into the page title.
# Skips the test when Chrome is not available.
run_widget_js <- function(widget, js, data = NULL) {
  chrome <- c(Sys.getenv("CHROMOTE_CHROME"),
              "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
              Sys.which(c("google-chrome", "chromium", "chromium-browser")))
  chrome <- chrome[nzchar(chrome) & file.exists(chrome)]
  testthat::skip_if(length(chrome) == 0, "Chrome not available")
  testthat::skip_on_cran()
  testthat::skip_on_ci()  # headless WebGL support varies between CI runners

  hook <- paste0("function(el, x, data) {",
                 "  try { var out = (function() {", js, "})();",
                 "    document.title = 'RESULT:' + JSON.stringify(out); }",
                 "  catch (e) { document.title = 'ERROR:' + e.message; } }")
  widget <- htmlwidgets::onRender(widget, hook, data = data)
  dir <- tempfile("widget")
  dir.create(dir)
  file <- file.path(dir, "index.html")
  htmlwidgets::saveWidget(widget, file, selfcontained = FALSE)
  dom <- suppressWarnings(system2(chrome[1], c(
    "--headless=new", "--disable-gpu", "--use-angle=swiftshader", "--enable-unsafe-swiftshader",
    "--no-first-run", "--virtual-time-budget=10000", "--dump-dom",
    shQuote(paste0("file://", file))), stdout = TRUE, stderr = FALSE, timeout = 60))
  title <- regmatches(dom, regexpr("<title>.*?</title>", dom))
  title <- gsub("^<title>|</title>$", "", title)
  title <- title[grepl("^(RESULT|ERROR):", title)]
  testthat::expect_length(title, 1)
  if (startsWith(title, "ERROR:")) stop("JavaScript error: ", sub("^ERROR:", "", title))
  jsonlite::fromJSON(xml2_unescape(sub("^RESULT:", "", title)), simplifyVector = FALSE)
}

# Undo HTML escaping of the page title
xml2_unescape <- function(x) {
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}
