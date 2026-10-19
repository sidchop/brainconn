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

test_that("left hemisphere is on the left in top, bottom, front and back views", {
  for (v in c("top", "bottom", "front", "back")) {
    pos <- node_positions(brainconn2d(toy_atlas, conmat = toy_conmat, view = v))
    expect_lt(pos["L_front", "x"], 0, label = v)
    expect_gt(pos["R_front", "x"], 0, label = v)
  }
})

test_that("bottom and front backgrounds are mirrored to match the nodes", {
  img <- array(0, c(2, 3, 4))
  img[, 1, 1] <- 1  # red in the first column only
  img[, , 4] <- 1
  png_file <- tempfile(fileext = ".png")
  OpenImageR::writeImage(img, png_file)
  first_col <- function(view) as.vector(background_grob(png_file, view)$raster[1, 1])
  expect_equal(first_col("top"), "#FF0000FF")
  expect_equal(first_col("back"), "#FF0000FF")
  expect_equal(first_col("bottom"), "#000000FF")
  expect_equal(first_col("front"), "#000000FF")
})

test_that("anterior points the correct way in each view", {
  for (v in c("top", "bottom")) {
    pos <- node_positions(brainconn2d(toy_atlas, conmat = toy_conmat, view = v))
    expect_gt(pos["L_front", "y"], pos["L_back", "y"])
  }
  # Left view: front of the brain on the viewer's left; right view: on the right
  pos <- node_positions(brainconn2d(toy_atlas, conmat = toy_conmat, view = "left"))
  expect_lt(pos["L_front", "x"], pos["L_back", "x"])
  pos <- node_positions(brainconn2d(toy_atlas, conmat = toy_conmat, view = "right"))
  expect_gt(pos["R_front", "x"], pos["R_back", "x"])
})

test_that("hemisphere labels match the side the left hemisphere is drawn on", {
  for (v in c("top", "bottom", "front", "back")) {
    p <- brainconn2d(toy_atlas, conmat = toy_conmat, view = v)
    text_layer <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
    lab <- ggplot2::ggplot_build(p)$data[[text_layer]]
    pos <- node_positions(p)
    expect_equal(sign(lab$x[lab$label == "L"]), sign(pos["L_front", "x"]), info = v)
  }
  p <- brainconn2d(toy_atlas, conmat = toy_conmat, hemi.labels = FALSE)
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})

test_that("nodes nearest the viewer are drawn last in side views", {
  hemi_order <- function(view) {
    substr(rownames(node_positions(brainconn2d(toy_atlas, conmat = toy_conmat, view = view))), 1, 1)
  }
  expect_equal(hemi_order("left"), c("R", "R", "L", "L"))
  expect_equal(hemi_order("right"), c("L", "L", "R", "R"))
})

test_that("edges stay attached to the right nodes after reordering", {
  p <- brainconn2d(toy_atlas, conmat = toy_conmat, view = "left")
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
      p <- brainconn2d("schaefer300_n7", conmat = get(ex), view = v)
      expect_s3_class(p, "ggraph")
      expect_no_error(ggplot2::ggplot_build(p))
    }
    expect_s3_class(brainconn2d("schaefer300_n7", conmat = get(ex), view = "ortho"), "ggplot")
  }
})

test_that("brainconn weighted options build", {
  x <- example_weighted_undirected
  for (cw in c(TRUE, FALSE)) {
    for (lw in c(TRUE, FALSE)) {
      p <- brainconn2d("schaefer300_n7", conmat = x, edge.color.weighted = cw,
                     label.edge.weight = lw, scale.edge.width = c(1, 3))
      expect_no_error(ggplot2::ggplot_build(p))
    }
  }
  p <- brainconn2d("schaefer300_n7", conmat = example_weighted_directed,
                 edge.color.weighted = TRUE, label.edge.weight = TRUE,
                 edge.color = ggraph::scale_edge_color_gradient(low = "blue", high = "red"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("brainconn handles inputs that used to fail", {
  # No conmat: plots all nodes
  p <- expect_message(brainconn2d("schaefer300_n7"), "No edges")
  expect_equal(nrow(p$data), 300)

  # Custom atlas without a network column
  p <- expect_message(brainconn2d(custom_atlas_example, conmat = example_unweighted_undirected),
                      "no 'network' column")
  expect_no_error(ggplot2::ggplot_build(p))

  # Negative weights, including nodes whose weights sum to zero
  m <- toy_conmat
  m[1, 2] <- m[2, 1] <- -1
  p <- brainconn2d(toy_atlas, conmat = m)
  expect_equal(nrow(p$data), 4)
  expect_no_error(ggplot2::ggplot_build(p))

  # node.size: one per atlas region, or one per plotted node
  x <- example_unweighted_undirected
  n_plotted <- sum(rowSums(x != 0) > 0)
  expect_no_error(ggplot2::ggplot_build(brainconn2d("schaefer300_n7", conmat = x, node.size = 1:300)))
  expect_no_error(ggplot2::ggplot_build(brainconn2d("schaefer300_n7", conmat = x, node.size = seq_len(n_plotted))))
  expect_error(brainconn2d("schaefer300_n7", conmat = x, node.size = 1:7), "node.size")

  # Atlas name of a data.frame in the caller's environment
  my_atlas <- toy_atlas
  expect_s3_class(brainconn2d("my_atlas", conmat = toy_conmat), "ggraph")
})

test_that("brainconn gives clear errors for bad input", {
  expect_error(brainconn2d("not_an_atlas"), "not found")
  expect_error(brainconn2d(toy_atlas, conmat = matrix(0, 3, 3)), "atlas has 4 regions")
  expect_error(brainconn2d(toy_atlas, conmat = matrix(0, 4, 3)), "square")
  expect_error(brainconn2d(toy_atlas, conmat = toy_conmat, view = "sideways"))
  expect_error(brainconn2d(toy_atlas, conmat = toy_conmat, view = "ortho", background = "x.png"),
               "ortho")
})

test_that("thresholds remove edges and their nodes", {
  m <- toy_conmat
  m[1, 2] <- m[2, 1] <- 0.2
  m[1, 4] <- m[4, 1] <- 0.9
  m[2, 3] <- m[3, 2] <- 0.5
  p <- brainconn2d(toy_atlas, conmat = m, thr = 0.4)
  expect_setequal(p$data$ROI.Name, c("L_front", "L_back", "R_front", "R_back"))
  p <- brainconn2d(toy_atlas, conmat = m, thr = 0.6)
  expect_setequal(p$data$ROI.Name, c("L_front", "R_back"))
  p <- brainconn2d(toy_atlas, conmat = m, uthr = 0.6)
  expect_setequal(p$data$ROI.Name, c("L_front", "L_back", "R_front"))
})

test_that("show.legend is respected, including in ortho view", {
  x <- example_unweighted_undirected
  expect_true(has_legend(brainconn2d("schaefer300_n7", conmat = x, view = "top")))
  expect_false(has_legend(brainconn2d("schaefer300_n7", conmat = x, view = "top",
                                    show.legend = FALSE)))
  expect_true(has_legend(brainconn2d("schaefer300_n7", conmat = x, view = "ortho")))
  expect_false(has_legend(brainconn2d("schaefer300_n7", conmat = x, view = "ortho",
                                    show.legend = FALSE)))
})
