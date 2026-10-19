traces_3d <- function(p) {
  b <- plotly::plotly_build(p)
  vapply(b$x$data, function(t) paste0(t$type, ":", if (is.null(t$mode)) "" else t$mode),
         character(1))
}

# Toy atlas with unique names: a triangle 1-2-3 plus a pendant node 4 on node 1
toy3d_atlas <- data.frame(
  ROI.Name = c("a", "b", "c", "d", "e"),
  x.mni = c(-40, 40, 0, -20, 30), y.mni = c(0, 0, 40, -50, -30), z.mni = c(10, 10, 20, 0, 5),
  network = c("A", "A", "B", "B", "C")
)
toy3d_conmat <- matrix(0, 5, 5)
toy3d_conmat[1, 2] <- toy3d_conmat[2, 1] <- 0.5
toy3d_conmat[2, 3] <- toy3d_conmat[3, 2] <- 0.2
toy3d_conmat[1, 3] <- toy3d_conmat[3, 1] <- -0.8
toy3d_conmat[1, 4] <- toy3d_conmat[4, 1] <- 0.3

test_that("brainconn3D draws mesh, edges and one node trace per network", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat)
  expect_equal(traces_3d(p), c("mesh3d:", "scatter3d:lines", "scatter3d:markers",
                               "scatter3d:markers"))
  b <- plotly::plotly_build(p)
  # Node e has no edges, so only networks A and B are shown
  expect_equal(vapply(b$x$data[3:4], `[[`, "", "name"), c("A", "B"))
  # Network colours come from network_colors()
  expect_equal(as.character(unlist(b$x$data[[3]]$marker$color)),
               rep(network_colors(c("A", "B", "C"))[["A"]], 2))
  # customdata holds each node's 0-based index
  expect_equal(sort(unlist(lapply(b$x$data[3:4], `[[`, "customdata"))), 0:3)
})

test_that("hover text lists degree, strength and strongest connections", {
  b <- plotly::plotly_build(brainconn3D(toy3d_atlas, conmat = toy3d_conmat))
  text_a <- b$x$data[[3]]$text[[1]]
  expect_match(text_a, "<b>a</b>")
  expect_match(text_a, "Degree: 3")
  expect_match(text_a, "Strength: 1.6")
  # Strongest first: c (|-0.8|), then b (0.5), then d (0.3)
  expect_match(text_a, "c \\(-0.8\\).*b \\(0.5\\).*d \\(0.3\\)")
})

test_that("edges can be coloured by weight", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, edge.color.weighted = TRUE,
                   show.legend = TRUE)
  b <- plotly::plotly_build(p)
  line <- b$x$data[[2]]$line
  expect_equal(c(line$cmin, line$cmax), c(-0.8, 0.8))
  expect_true(line$showscale)
  expect_equal(sort(unique(na.omit(unlist(line$color)))), c(-0.8, 0.2, 0.3, 0.5))
})

test_that("brainconn3D works without a mesh and with custom node colours", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, mesh = NULL,
                   node.color = c("red", "green", "blue", "black", "grey"))
  expect_equal(traces_3d(p), c("scatter3d:lines", "scatter3d:markers"))
  b <- plotly::plotly_build(p)
  expect_equal(as.character(unlist(b$x$data[[2]]$marker$color)), c("red", "green", "blue", "black"))
})

test_that("highlight payload lists each node's neighbours and edges", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, highlight = TRUE,
                   edge.color.weighted = TRUE)
  # mesh, edges, highlighted edges, two node traces, two legend traces
  expect_equal(traces_3d(p), c("mesh3d:", "scatter3d:lines", "scatter3d:lines",
                               rep("scatter3d:markers", 4)))
  data <- p$jsHooks$render[[1]]$data
  expect_equal(as.vector(unlist(data$nodeTraces)), c(3, 4))
  expect_equal(as.vector(unlist(data$legendTraces)), c(5, 6))
  b <- plotly::plotly_build(p)
  expect_equal(vapply(b$x$data[4:7], function(t) isTRUE(t$showlegend), logical(1)),
               c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(c(data$edgeTrace, data$hlTrace), c(1, 2))
  neighbors <- lapply(data$neighbors, function(x) sort(as.vector(x)))
  # 0-based: a-b, b-c, a-c, a-d
  expect_equal(neighbors, list(c(1, 2, 3), c(0, 2), c(0, 1), 0))
  # Node a has three edges: 3 points (from, to, gap) each, with weights
  expect_length(data$edges[[1]]$x, 9)
  expect_equal(sort(unique(na.omit(as.vector(data$edges[[1]]$w)))), c(-0.8, 0.3, 0.5))
  # The JSON keeps a single neighbour as an array
  json <- htmlwidgets:::toJSON(data)
  expect_match(json, '"neighbors":\\[(\\[[0-9,]+\\],){3}\\[0\\]\\]')
})

test_that("highlighting a node in the browser shows its edges and greys out other nodes", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, highlight = TRUE)
  res <- run_widget_js(p, "
    var gd = el, d = data;
    function state() {
      var hl = gd.data[d.hl];
      var colors = {};
      d.nodeTraces.forEach(function(t) {
        var c = gd.data[t].marker.color, ids = gd.data[t].customdata;
        if (!Array.isArray(c)) { c = [c]; ids = [ids]; }
        ids.forEach(function(id, k) { colors[id] = c[k]; });
      });
      return {hlPoints: (hl.x || []).filter(function(v) { return v !== null; }).length,
              edgeOpacity: gd.data[d.edge].opacity, colors: colors};
    }
    el.brainconnHighlight(3);
    var on = state();
    el.brainconnHighlight(null);
    var legend = [5, 6].map(function(t) {
      return {x: gd.data[t].x, color: gd.data[t].marker.color};
    });
    return {on: on, off: state(), legend: legend};",
    data = list(hl = 2, edge = 1, nodeTraces = I(c(3, 4))))
  dim <- "#D9D9D9"
  # Node d (index 3) has one edge, to node a (index 0)
  expect_equal(res$on$hlPoints, 2)
  expect_equal(res$on$edgeOpacity, 0.12)
  expect_false(res$on$colors[["0"]] == dim)
  expect_false(res$on$colors[["3"]] == dim)
  expect_equal(c(res$on$colors[["1"]], res$on$colors[["2"]]), c(dim, dim))
  # Reset restores everything
  expect_equal(res$off$hlPoints, 0)
  expect_equal(res$off$edgeOpacity, 1)
  expect_false(any(unlist(res$off$colors) == dim))
  # Legend traces are moved out of the scene but keep their network colours
  expect_equal(lapply(res$legend, function(l) l$x), list(list(NULL), list(NULL)))
  expect_equal(vapply(res$legend, function(l) l$color, ""),
               unname(network_colors(toy3d_atlas$network)[c("A", "B")]))
})

test_that("brainconn3D handles inputs that used to fail", {
  expect_no_error(plotly::plotly_build(
    brainconn3D("schaefer300_n7", conmat = example_unweighted_directed, highlight = TRUE)))
  expect_no_error(plotly::plotly_build(
    brainconn3D("schaefer300_n7", conmat = example_unweighted_undirected, all.nodes = TRUE)))
  expect_no_error(plotly::plotly_build(
    brainconn3D("schaefer300_n7", conmat = example_weighted_undirected, thr = 0.5)))
  expect_message(p <- brainconn3D(custom_atlas_example, conmat = example_unweighted_undirected),
                 "no 'network' column")
  expect_no_error(plotly::plotly_build(p))
  expect_message(p <- brainconn3D("schaefer300_n7", highlight = TRUE), "No edges")
  expect_no_error(plotly::plotly_build(p))
  expect_true(is.na(p$jsHooks$render[[1]]$data$hlTrace))
})

test_that("brainconn3D draws each undirected edge once, in one scene", {
  p <- brainconn3D("schaefer300_n7", conmat = example_unweighted_undirected)
  b <- plotly::plotly_build(p)
  expect_equal(grep("^scene", names(b$x$layout), value = TRUE), "scene")
  x <- as.matrix(example_unweighted_undirected)
  edge_trace <- b$x$data[[which(traces_3d(p) == "scatter3d:lines")]]
  expect_equal(sum(!is.na(edge_trace$x)), 2 * sum(x[upper.tri(x)] != 0))
})

test_that("brainconn3D edges start and end at node positions", {
  b <- plotly::plotly_build(brainconn3D(toy3d_atlas, conmat = toy3d_conmat, d.factor = 1))
  edge <- b$x$data[[2]]
  pts <- unique(na.omit(data.frame(x = edge$x, y = edge$y, z = edge$z)))
  expect_true(all(paste(pts$x, pts$y, pts$z) %in%
                    paste(toy3d_atlas$x.mni, toy3d_atlas$y.mni, toy3d_atlas$z.mni)))
})

test_that("brainconn3D applies thresholds before choosing nodes", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, thr = 0.4)
  b <- plotly::plotly_build(p)
  markers <- b$x$data[grepl("markers", traces_3d(p))]
  shown <- sub("<b>(.*?)</b>.*", "\\1", unlist(lapply(markers, `[[`, "text")))
  expect_setequal(shown, c("a", "b"))
})

test_that("the page stays small: one mesh colour, no unbuilt copy, short numbers", {
  p <- brainconn3D("schaefer300_n7", conmat = example_unweighted_undirected)
  expect_null(p$x$attrs)
  mesh <- p$x$data[[1]]
  expect_equal(mesh$color, "#F0F0F0")
  expect_null(mesh$facecolor)
  # Vertices are on a 0.25 mm grid, so they print with few digits
  expect_true(all(mesh$x * 4 == round(mesh$x * 4)))
  expect_lt(nchar(htmlwidgets:::toJSON(htmlwidgets:::createPayload(p))), 6e6)
})
