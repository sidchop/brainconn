# Small atlas with one node in each hemisphere, front and back, and unique names
toy_atlas <- data.frame(
  ROI.Name = c("L_front", "L_back", "R_front", "R_back"),
  x.mni = c(-40L, -40L, 40L, 40L),
  y.mni = c(40L, -60L, 40L, -60L),
  z.mni = c(10L, 20L, 10L, 20L),
  network = c("A", "B", "A", "B")
)
toy_conmat <- matrix(c(0, 1, 0, 1,
                       1, 0, 1, 0,
                       0, 1, 0, 0,
                       1, 0, 0, 0), 4, 4, byrow = TRUE)

# Node positions as drawn, keyed by ROI name
node_positions <- function(p) {
  data.frame(x = p$data$x, y = p$data$y, row.names = p$data$ROI.Name)
}

# Does a ggplot or cowplot object draw a legend anywhere? (looks for a legend
# title grob anywhere in the drawn grob tree)
has_legend <- function(p) {
  grob_names <- function(g) {
    kids <- c(if (inherits(g, "gtable")) g$grobs, if (inherits(g, "gTree")) g$children)
    c(g$name, unlist(lapply(kids, grob_names)))
  }
  any(grepl("^guide\\.title", grob_names(ggplot2::ggplotGrob(p))))
}

all_views <- c("top", "bottom", "left", "right", "front", "back")

test_that("check_atlas works correctly", {
  valid_atlas <- data.frame(
    ROI.Name = c("Region1", "Region2"),
    x.mni = as.integer(c(10, 20)),
    y.mni = as.integer(c(15, 25)),
    z.mni = as.integer(c(5, 35))
  )
  expect_message(check_atlas(valid_atlas), "Atlas fits brainconn specifications")

  # Non-integer coordinates are fine
  valid_atlas$x.mni <- c(10.5, 20.2)
  expect_message(check_atlas(valid_atlas), "Atlas fits brainconn specifications")

  invalid_atlas <- valid_atlas[, c("ROI.Name", "x.mni", "y.mni")]
  expect_error(check_atlas(invalid_atlas), "File missing z.mni column")

  valid_atlas$x.mni <- c("a", "b")
  expect_error(check_atlas(valid_atlas), "need to be numeric")

  expect_message(check_atlas(list()), "Please convert atlas to a dataframe")
})

test_that("list_atlases lists atlases but not backgrounds or examples", {
  atlases <- suppressMessages(list_atlases())
  expect_true(all(c("schaefer300_n7", "aal90", "dk68") %in% atlases))
  expect_false(any(grepl("ICBM|example", atlases)))
})

test_that("left hemisphere is on the correct side in each view", {
  # Viewed from above or behind, the subject's left is on the viewer's left;
  # viewed from below or the front, it is on the viewer's right.
  for (v in c("top", "back")) {
    pos <- node_positions(brainconn(toy_atlas, conmat = toy_conmat, view = v))
    expect_lt(pos["L_front", "x"], 0)
    expect_gt(pos["R_front", "x"], 0)
  }
  for (v in c("bottom", "front")) {
    pos <- node_positions(brainconn(toy_atlas, conmat = toy_conmat, view = v))
    expect_gt(pos["L_front", "x"], 0)
    expect_lt(pos["R_front", "x"], 0)
  }
})

test_that("anterior points the correct way in each view", {
  for (v in c("top", "bottom")) {
    pos <- node_positions(brainconn(toy_atlas, conmat = toy_conmat, view = v))
    expect_gt(pos["L_front", "y"], pos["L_back", "y"])
  }
  # Left view: front of the brain on the viewer's left; right view: on the right
  pos <- node_positions(brainconn(toy_atlas, conmat = toy_conmat, view = "left"))
  expect_lt(pos["L_front", "x"], pos["L_back", "x"])
  pos <- node_positions(brainconn(toy_atlas, conmat = toy_conmat, view = "right"))
  expect_gt(pos["R_front", "x"], pos["R_back", "x"])
})

test_that("hemisphere labels match the side the left hemisphere is drawn on", {
  for (v in c("top", "bottom", "front", "back")) {
    p <- brainconn(toy_atlas, conmat = toy_conmat, view = v)
    text_layer <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
    lab <- ggplot2::ggplot_build(p)$data[[text_layer]]
    pos <- node_positions(p)
    expect_equal(sign(lab$x[lab$label == "L"]), sign(pos["L_front", "x"]), info = v)
  }
  p <- brainconn(toy_atlas, conmat = toy_conmat, hemi.labels = FALSE)
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})

test_that("nodes nearest the viewer are drawn last in side views", {
  hemi_order <- function(view) {
    substr(rownames(node_positions(brainconn(toy_atlas, conmat = toy_conmat, view = view))), 1, 1)
  }
  expect_equal(hemi_order("left"), c("R", "R", "L", "L"))
  expect_equal(hemi_order("right"), c("L", "L", "R", "R"))
})

test_that("edges stay attached to the right nodes after reordering", {
  p <- brainconn(toy_atlas, conmat = toy_conmat, view = "left")
  edges <- ggraph::get_edges()(p$data)
  pairs <- apply(cbind(edges$node1.ROI.Name, edges$node2.ROI.Name), 1,
                 function(x) paste(sort(x), collapse = "-"))
  expect_setequal(pairs, c("L_back-L_front", "L_front-R_back", "L_back-R_front"))
})

test_that("brainconn builds for every example matrix and view", {
  examples <- c("example_unweighted_undirected", "example_weighted_undirected",
                "example_unweighted_directed", "example_weighted_directed")
  for (ex in examples) {
    for (v in c(all_views)) {
      p <- brainconn("schaefer300_n7", conmat = get(ex), view = v)
      expect_s3_class(p, "ggraph")
      expect_no_error(ggplot2::ggplot_build(p))
    }
    expect_s3_class(brainconn("schaefer300_n7", conmat = get(ex), view = "ortho"), "ggplot")
  }
})

test_that("brainconn weighted options build", {
  x <- example_weighted_undirected
  for (cw in c(TRUE, FALSE)) {
    for (lw in c(TRUE, FALSE)) {
      p <- brainconn("schaefer300_n7", conmat = x, edge.color.weighted = cw,
                     label.edge.weight = lw, scale.edge.width = c(1, 3))
      expect_no_error(ggplot2::ggplot_build(p))
    }
  }
  p <- brainconn("schaefer300_n7", conmat = example_weighted_directed,
                 edge.color.weighted = TRUE, label.edge.weight = TRUE,
                 edge.color = ggraph::scale_edge_color_gradient(low = "blue", high = "red"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("brainconn handles inputs that used to fail", {
  # No conmat: plots all nodes
  p <- expect_message(brainconn("schaefer300_n7"), "No edges")
  expect_equal(nrow(p$data), 300)

  # Custom atlas without a network column
  p <- expect_message(brainconn(custom_atlas_example, conmat = example_unweighted_undirected),
                      "no 'network' column")
  expect_no_error(ggplot2::ggplot_build(p))

  # Negative weights, including nodes whose weights sum to zero
  m <- toy_conmat
  m[1, 2] <- m[2, 1] <- -1
  p <- brainconn(toy_atlas, conmat = m)
  expect_equal(nrow(p$data), 4)
  expect_no_error(ggplot2::ggplot_build(p))

  # node.size: one per atlas region, or one per plotted node
  x <- example_unweighted_undirected
  n_plotted <- sum(rowSums(x != 0) > 0)
  expect_no_error(ggplot2::ggplot_build(brainconn("schaefer300_n7", conmat = x, node.size = 1:300)))
  expect_no_error(ggplot2::ggplot_build(brainconn("schaefer300_n7", conmat = x, node.size = seq_len(n_plotted))))
  expect_error(brainconn("schaefer300_n7", conmat = x, node.size = 1:7), "node.size")

  # Atlas name of a data.frame in the caller's environment
  my_atlas <- toy_atlas
  expect_s3_class(brainconn("my_atlas", conmat = toy_conmat), "ggraph")
})

test_that("brainconn gives clear errors for bad input", {
  expect_error(brainconn("not_an_atlas"), "not found")
  expect_error(brainconn(toy_atlas, conmat = matrix(0, 3, 3)), "atlas has 4 regions")
  expect_error(brainconn(toy_atlas, conmat = matrix(0, 4, 3)), "square")
  expect_error(brainconn(toy_atlas, conmat = toy_conmat, view = "sideways"))
  expect_error(brainconn(toy_atlas, conmat = toy_conmat, view = "ortho", background = "x.png"),
               "ortho")
})

test_that("thresholds remove edges and their nodes", {
  m <- toy_conmat
  m[1, 2] <- m[2, 1] <- 0.2
  m[1, 4] <- m[4, 1] <- 0.9
  m[2, 3] <- m[3, 2] <- 0.5
  p <- brainconn(toy_atlas, conmat = m, thr = 0.4)
  expect_setequal(p$data$ROI.Name, c("L_front", "L_back", "R_front", "R_back"))
  p <- brainconn(toy_atlas, conmat = m, thr = 0.6)
  expect_setequal(p$data$ROI.Name, c("L_front", "R_back"))
  p <- brainconn(toy_atlas, conmat = m, uthr = 0.6)
  expect_setequal(p$data$ROI.Name, c("L_front", "L_back", "R_front"))
})

test_that("show.legend is respected, including in ortho view", {
  x <- example_unweighted_undirected
  expect_true(has_legend(brainconn("schaefer300_n7", conmat = x, view = "top")))
  expect_false(has_legend(brainconn("schaefer300_n7", conmat = x, view = "top",
                                    show.legend = FALSE)))
  expect_true(has_legend(brainconn("schaefer300_n7", conmat = x, view = "ortho")))
  expect_false(has_legend(brainconn("schaefer300_n7", conmat = x, view = "ortho",
                                    show.legend = FALSE)))
})

# brainconn3D -----------------------------------------------------------------

traces_3d <- function(p) {
  b <- plotly::plotly_build(p)
  vapply(b$x$data, function(t) paste0(t$type, ":", if (is.null(t$mode)) "" else t$mode),
         character(1))
}

test_that("brainconn3D draws mesh, nodes and edges in one scene", {
  p <- brainconn3D("schaefer300_n7", conmat = example_unweighted_undirected)
  b <- plotly::plotly_build(p)
  types <- traces_3d(p)
  expect_equal(sum(types == "mesh3d:"), 1)
  expect_equal(sum(types == "scatter3d:lines"), 1)
  expect_true(all(grepl("^(mesh3d|scatter3d:(lines|markers))", types)))
  expect_equal(grep("^scene", names(b$x$layout), value = TRUE), "scene")

  # Each undirected edge drawn once: two points per edge
  x <- as.matrix(example_unweighted_undirected)
  n_edges <- sum(x[upper.tri(x)] != 0)
  edge_trace <- b$x$data[[which(types == "scatter3d:lines")]]
  expect_equal(sum(!is.na(edge_trace$x)), 2 * n_edges)
})

test_that("brainconn3D edges start and end at node positions", {
  p <- brainconn3D(toy_atlas, conmat = toy_conmat, d.factor = 1)
  b <- plotly::plotly_build(p)
  types <- traces_3d(p)
  edge <- b$x$data[[which(types == "scatter3d:lines")]]
  pts <- unique(na.omit(data.frame(x = edge$x, y = edge$y, z = edge$z)))
  nodes <- unique(toy_atlas[, c("x.mni", "y.mni", "z.mni")])
  expect_true(all(paste(pts$x, pts$y, pts$z) %in% paste(nodes$x.mni, nodes$y.mni, nodes$z.mni)))
})

test_that("brainconn3D handles inputs that used to fail", {
  expect_no_error(plotly::plotly_build(
    brainconn3D("schaefer300_n7", conmat = example_unweighted_directed)))
  expect_no_error(plotly::plotly_build(
    brainconn3D("schaefer300_n7", conmat = example_weighted_undirected, thr = 0.5)))
  expect_no_error(plotly::plotly_build(
    brainconn3D("schaefer300_n7", conmat = example_unweighted_undirected, node.color = "red")))
  expect_no_error(plotly::plotly_build(
    brainconn3D("schaefer300_n7", conmat = example_unweighted_undirected, all.nodes = TRUE)))
  expect_message(p <- brainconn3D(custom_atlas_example, conmat = example_unweighted_undirected),
                 "no 'network' column")
  expect_no_error(plotly::plotly_build(p))
  expect_message(p <- brainconn3D("schaefer300_n7"), "No edges")
  expect_no_error(plotly::plotly_build(p))
})

test_that("brainconn3D applies thresholds before choosing nodes", {
  m <- toy_conmat
  m[1, 2] <- m[2, 1] <- 0.2
  m[1, 4] <- m[4, 1] <- 0.9
  m[2, 3] <- m[3, 2] <- 0.5
  p <- brainconn3D(toy_atlas, conmat = m, thr = 0.6)
  b <- plotly::plotly_build(p)
  markers <- b$x$data[grepl("markers", traces_3d(p))]
  expect_setequal(unlist(lapply(markers, `[[`, "text")), c("L_front", "R_back"))
})
