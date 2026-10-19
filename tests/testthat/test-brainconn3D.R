traces_3d <- function(p) {
  vapply(p$x$data, function(t) paste0(t$type, ":", if (is.null(t$mode)) "" else t$mode),
         character(1))
}
hook_data <- function(p) p$jsHooks$render[[1]]$data

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

test_that("nodes are sphere meshes, one trace per network, with legend entries", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat)
  # brain, edges, node spheres for networks A and B (node e has no edges), legend points
  expect_equal(traces_3d(p), c("mesh3d:", "scatter3d:lines", "mesh3d:", "mesh3d:",
                               "scatter3d:markers", "scatter3d:markers"))
  nodes_a <- p$x$data[[3]]
  expect_equal(nodes_a$name, "A")
  # Two nodes in A, each a 162-vertex sphere coloured like its network
  expect_length(nodes_a$x, 2 * 162)
  expect_equal(unique(nodes_a$vertexcolor), network_colors(toy3d_atlas$network)[["A"]])
  expect_false(isTRUE(nodes_a$showlegend))
  expect_equal(vapply(p$x$data[5:6], `[[`, "", "name"), c("A", "B"))
  expect_equal(hook_data(p)$legendTraces, I(c(4L, 5L)))
})

test_that("sphere sizes are in mm and scale with the mesh", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, node.size = 10)
  # node.size is the diameter in mm for a 170 mm long brain; ICBM152 is 174 mm
  icbm_length <- diff(range(icbm152_lite$vertices[, 2]))
  expect_equal(unique(hook_data(p)$radii), 5 * icbm_length / 170)
  nodes_a <- p$x$data[[3]]
  first <- seq_len(162)
  expect_equal(diff(range(nodes_a$x[first])), 10 * icbm_length / 170, tolerance = 0.05)
  # A brain a tenth of the size gets spheres a tenth of the size
  small <- list(vertices = resolve_mesh("ICBM152")$vertices / 10,
                faces = resolve_mesh("ICBM152")$faces)
  atlas_small <- toy3d_atlas
  atlas_small[c("x.mni", "y.mni", "z.mni")] <- atlas_small[c("x.mni", "y.mni", "z.mni")] / 10
  p <- brainconn3D(atlas_small, conmat = toy3d_conmat, node.size = 10, mesh = small)
  expect_equal(unique(hook_data(p)$radii), 5 * diff(range(small$vertices[, 2])) / 170)
})

test_that("hover text lists degree, strength and strongest connections", {
  text_a <- hook_data(brainconn3D(toy3d_atlas, conmat = toy3d_conmat))$hover[[1]]
  expect_match(text_a, "<b>a</b>")
  expect_match(text_a, "Degree: 3")
  expect_match(text_a, "Strength: 1.6")
  # Strongest first: c (|-0.8|), then b (0.5), then d (0.3)
  expect_match(text_a, "c \\(-0.8\\).*b \\(0.5\\).*d \\(0.3\\)")
})

test_that("edges can be coloured by weight", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, edge.color.weighted = TRUE,
                   show.legend = TRUE)
  line <- p$x$data[[2]]$line
  expect_equal(c(line$cmin, line$cmax), c(-0.8, 0.8))
  expect_true(line$showscale)
  expect_equal(sort(unique(na.omit(unlist(line$color)))), c(-0.8, 0.2, 0.3, 0.5))
})

test_that("brainconn3D works without a mesh and with custom node colours", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, mesh = NULL,
                   node.color = c("red", "green", "blue", "black", "grey"))
  expect_equal(traces_3d(p), c("scatter3d:lines", "mesh3d:"))
  expect_equal(unique(p$x$data[[2]]$vertexcolor), c("red", "green", "blue", "black"))
})

test_that("highlight payload lists each node's neighbours and edges", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, highlight = TRUE,
                   edge.color.weighted = TRUE)
  # brain, edges, highlighted edges, node spheres A and B, selection spheres, legend points
  expect_equal(traces_3d(p), c("mesh3d:", "scatter3d:lines", "scatter3d:lines", "mesh3d:",
                               "mesh3d:", "mesh3d:", "scatter3d:markers", "scatter3d:markers"))
  data <- hook_data(p)
  expect_true(data$highlight)
  expect_equal(c(data$edgeTrace, data$hlTrace, data$selTrace), c(1, 2, 5))
  expect_equal(vapply(data$nodeTraces, `[[`, 0L, "trace"), c(3L, 4L))
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

test_that("in the browser, hovering shows a tooltip and clicking highlights a node", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, highlight = TRUE, show.legend = TRUE)
  b <- open_widget(p)
  on.exit(b$close())
  # Legend points are moved out of the scene but keep their colours
  expect_true(all(is.na(page_eval(b, "[gd.data[6].x[0], gd.data[7].x[0]]"))))
  expect_equal(page_eval(b, "[gd.data[6].marker.color, gd.data[7].marker.color]"),
               unname(network_colors(toy3d_atlas$network)[c("A", "B")]))

  # Node d (index 3) is inside the brain surface, which blocks plotly's own picking
  d <- page_eval(b, "gd.brainconnScreen(3)")
  mouse_move(b, d$x, d$y)
  expect_equal(page_eval(b, "document.querySelector('.brainconn-tooltip').style.display"), "block")
  expect_match(page_eval(b, "document.querySelector('.brainconn-tooltip').innerHTML"), "<b>d</b>")

  # A real click selects it: its one edge (to a) is drawn, the rest fade
  mouse_click(b, d$x, d$y)
  expect_equal(page_eval(b, "gd.brainconnSelected()"), 3)
  state <- function() page_eval(b, paste0(
    "{edges: gd.data[2].x.filter(function(v) { return v !== null; }).length,",
    " edgeOpacity: gd.data[1].opacity, nodeOpacity: gd.data[3].opacity,",
    " selected: gd.data[5].x.length, selectedOpacity: gd.data[5].opacity}"))
  on <- state()
  expect_equal(on$edges, 2)
  expect_equal(on$edgeOpacity, 0.06)
  expect_equal(on$nodeOpacity, 0.12)
  # Spheres of d and its neighbour a, opaque
  expect_equal(on$selected, 2 * 162)
  expect_equal(on$selectedOpacity, 1)

  # Escape shows everything again; so does clicking the node again
  press_escape(b)
  off <- state()
  expect_equal(c(off$edges, off$edgeOpacity, off$nodeOpacity, off$selectedOpacity), c(0, 1, 1, 0))
  mouse_click(b, d$x, d$y)
  mouse_click(b, d$x, d$y)
  expect_null(page_eval(b, "gd.brainconnSelected()"))

  # Dragging (rotating) doesn't select; clicking empty space clears
  b$Input$dispatchMouseEvent(type = "mousePressed", x = d$x, y = d$y, button = "left", clickCount = 1)
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = d$x + 40, y = d$y, buttons = 1)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = d$x + 40, y = d$y, button = "left", clickCount = 1)
  Sys.sleep(0.3)
  expect_null(page_eval(b, "gd.brainconnSelected()"))
  mouse_click(b, d$x, d$y)
  mouse_click(b, 5, 640)
  expect_null(page_eval(b, "gd.brainconnSelected()"))
})

test_that("brainconn3D handles inputs that used to fail", {
  expect_s3_class(brainconn3D("schaefer300_n7", conmat = example_unweighted_directed,
                              highlight = TRUE), "plotly")
  expect_s3_class(brainconn3D("schaefer300_n7", conmat = example_unweighted_undirected,
                              all.nodes = TRUE), "plotly")
  expect_s3_class(brainconn3D("schaefer300_n7", conmat = example_weighted_undirected, thr = 0.5),
                  "plotly")
  expect_message(p <- brainconn3D(custom_atlas_example, conmat = example_unweighted_undirected),
                 "no 'network' column")
  expect_s3_class(p, "plotly")
  expect_message(p <- brainconn3D("schaefer300_n7", highlight = TRUE), "No edges")
  expect_true(is.na(hook_data(p)$hlTrace))
})

test_that("brainconn3D draws each undirected edge once, in one scene", {
  p <- brainconn3D("schaefer300_n7", conmat = example_unweighted_undirected)
  expect_equal(grep("^scene", names(p$x$layout), value = TRUE), "scene")
  x <- as.matrix(example_unweighted_undirected)
  edge_trace <- p$x$data[[which(traces_3d(p) == "scatter3d:lines")]]
  expect_equal(sum(!is.na(edge_trace$x)), 2 * sum(x[upper.tri(x)] != 0))
})

test_that("brainconn3D edges start and end at node positions", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, d.factor = 1)
  edge <- p$x$data[[2]]
  pts <- unique(na.omit(data.frame(x = edge$x, y = edge$y, z = edge$z)))
  expect_true(all(paste(pts$x, pts$y, pts$z) %in%
                    paste(toy3d_atlas$x.mni, toy3d_atlas$y.mni, toy3d_atlas$z.mni)))
})

test_that("brainconn3D applies thresholds before choosing nodes", {
  p <- brainconn3D(toy3d_atlas, conmat = toy3d_conmat, thr = 0.4)
  shown <- sub("<b>(.*?)</b>.*", "\\1", unlist(hook_data(p)$hover))
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
