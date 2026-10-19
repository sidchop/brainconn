# Tests for the static 3D brainconn(). Renders are kept small so tests are quick.

# Two nodes: left-anterior (red) and right-posterior (blue)
lr_atlas <- data.frame(
  ROI.Name = c("L_ant", "R_post"),
  x.mni = c(-40, 40), y.mni = c(40, -40), z.mni = c(0, 0),
  network = c("A", "B")
)
lr_conmat <- matrix(c(0, 1, 1, 0), 2, 2)

render_small <- function(...) {
  suppressMessages(brainconn(..., res = 160, quality = 1))
}

# Centre (in pixels, from the left) of the pixels close to a colour
color_centre_x <- function(img, rgb) {
  d <- abs(img[, , 1] - rgb[1]) + abs(img[, , 2] - rgb[2]) + abs(img[, , 3] - rgb[3])
  hits <- which(d < 0.6, arr.ind = TRUE)
  mean(hits[, "col"])
}

# The rendered image of a brainconn() plot, as an RGB array with values 0-1
image_of <- function(p) {
  layer <- Filter(function(l) inherits(l$geom, "GeomRasterAnn"), p$layers)[[1]]
  r <- as.matrix(layer$geom_params$raster)
  array(t(grDevices::col2rgb(r) / 255), c(dim(r), 3))
}

test_that("left hemisphere is on the left of the image in top, bottom, front and back views", {
  for (v in c("top", "bottom", "front", "back")) {
    p <- render_small(atlas = lr_atlas, conmat = lr_conmat, view = v, mesh = NULL,
                      node.color = c("red", "blue"), node.size = 20, edge.alpha = 0)
    img <- image_of(p)
    red <- color_centre_x(img, c(1, 0, 0))
    blue <- color_centre_x(img, c(0, 0, 1))
    expect_lt(red, blue, label = paste(v, "red x"))
  }
})

test_that("anterior is towards the viewer's left in the left view and right in the right view", {
  centres <- function(v) {
    p <- render_small(atlas = lr_atlas, conmat = lr_conmat, view = v, mesh = NULL,
                      node.color = c("red", "blue"), node.size = 20, edge.alpha = 0)
    img <- image_of(p)
    c(red = color_centre_x(img, c(1, 0, 0)), blue = color_centre_x(img, c(0, 0, 1)))
  }
  left <- centres("left")
  expect_lt(left[["red"]], left[["blue"]])
  right <- centres("right")
  expect_gt(right[["red"]], right[["blue"]])
})

test_that("projected node positions match where nodes are drawn", {
  for (v in list("top", "front", c(-120, 30))) {
    cam <- fit_camera(view_camera(v), as.matrix(expand.grid(c(-60, 60), c(-60, 60), c(-60, 60))))
    pos <- project_points(cam, as.matrix(lr_atlas[, c("x.mni", "y.mni", "z.mni")]))
    nodes <- data.frame(x = lr_atlas$x.mni, y = lr_atlas$y.mni, z = lr_atlas$z.mni,
                        color = c("red", "blue"), radius = 6)
    edges <- data.frame(from = integer(0), to = integer(0), color = character(0),
                        radius = numeric(0), directed = logical(0), reciprocal = logical(0))
    img <- render_scene(scene_objects(NULL, "grey", 1, nodes, edges, 1), cam, res = 200,
                        quality = 1)
    red_col <- color_centre_x(img, c(1, 0, 0))
    expected_col <- (pos$x[1] / cam$width + 0.5) * dim(img)[2]
    expect_equal(red_col, expected_col, tolerance = 0.05 * dim(img)[2],
                 label = paste(format(v), "red column"))
  }
})

test_that("brainconn returns a ggplot for every view type", {
  x <- example_unweighted_undirected
  for (v in list("top", "left", c(45, 10))) {
    expect_s3_class(render_small(atlas = "schaefer300_n7", conmat = x, view = v), "ggplot")
  }
  expect_s3_class(render_small(atlas = "schaefer300_n7", conmat = x, view = "ortho"), "ggplot")
  expect_s3_class(render_small(atlas = "schaefer300_n7", conmat = x, view = "all"), "ggplot")
  expect_error(render_small(atlas = "schaefer300_n7", conmat = x, view = "sideways"))
  expect_error(render_small(atlas = "schaefer300_n7", conmat = x, view = c(1, 2, 3)), "azimuth")
})

test_that("legends show networks and, when coloured by weight, edge weights", {
  p <- render_small(atlas = "schaefer300_n7", conmat = example_weighted_undirected,
                    edge.color.weighted = TRUE)
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$plot$scales$get_scales("colour")$name, "Network")
  expect_equal(b$plot$scales$get_scales("fill")$name, "Weight")
  expect_equal(b$plot$scales$get_scales("fill")$get_limits(), c(1, 10))

  p <- render_small(atlas = "schaefer300_n7", conmat = example_weighted_undirected,
                    show.legend = FALSE)
  expect_length(p$layers, 2)  # raster + hemisphere labels only
})

test_that("network legend follows the atlas order", {
  p <- render_small(atlas = "schaefer300_n7", conmat = example_unweighted_undirected)
  nets <- unique(schaefer300_n7$network)
  shown <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$get_limits()
  expect_equal(shown, nets[nets %in% shown])
})

test_that("calls with 2D-only arguments are passed to brainconn2d with a warning", {
  x <- example_unweighted_undirected
  expect_warning(p <- brainconn("schaefer300_n7", conmat = x, background.alpha = 0.5,
                                view = "left"),
                 "brainconn2d")
  expect_s3_class(p, "ggraph")
  expect_warning(brainconn("schaefer300_n7", conmat = example_weighted_undirected,
                           edge.color.weighted = TRUE,
                           edge.color = ggraph::scale_edge_color_gradient()),
                 "edge.color")
  expect_error(brainconn("schaefer300_n7", conmat = x, foo = 1), "Unused argument")
  expect_error(brainconn("schaefer300_n7", "ICBM152"), "brainconn2d")
})

test_that("the 3D message is shown once per session", {
  brainconn_env$told_3d <- NULL
  x <- example_unweighted_undirected
  expect_message(brainconn("schaefer300_n7", conmat = x, res = 100, quality = 1), "3D renders")
  expect_no_message(brainconn("schaefer300_n7", conmat = x, res = 100, quality = 1))
})

test_that("node sizes and colours can be given per node", {
  x <- example_unweighted_undirected
  n_plotted <- sum(rowSums(as.matrix(x) != 0) > 0)
  expect_s3_class(render_small(atlas = "schaefer300_n7", conmat = x, node.size = 1:300,
                               node.color = rep(c("red", "blue"), 150)), "ggplot")
  expect_s3_class(render_small(atlas = "schaefer300_n7", conmat = x,
                               node.size = seq_len(n_plotted)), "ggplot")
  expect_error(render_small(atlas = "schaefer300_n7", conmat = x, node.color = c("red", "blue")),
               "node.color")
})

test_that("custom meshes are accepted and scale node sizes", {
  # A small box, about the size of a rat brain (20 mm long)
  box <- list(vertices = as.matrix(expand.grid(x = c(-5, 5), y = c(-10, 10), z = c(-4, 4))),
              faces = rbind(c(1, 2, 4), c(1, 4, 3), c(5, 6, 8), c(5, 8, 7), c(1, 2, 6),
                            c(1, 6, 5), c(3, 4, 8), c(3, 8, 7), c(1, 3, 7), c(1, 7, 5),
                            c(2, 4, 8), c(2, 8, 6)))
  rat_atlas <- data.frame(ROI.Name = c("a", "b", "c"), x.mni = c(-3, 3, 0),
                          y.mni = c(5, 5, -6), z.mni = c(1, 1, -1), network = "N")
  rat_conmat <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3, 3)
  p <- render_small(atlas = rat_atlas, conmat = rat_conmat, mesh = box)
  expect_s3_class(p, "ggplot")
  # The view is fitted to the mesh, not to MNI space
  expect_lt(diff(ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range), 15)

  mesh3d <- structure(list(vb = rbind(t(box$vertices), 1), it = t(box$faces)), class = "mesh3d")
  expect_s3_class(render_small(atlas = rat_atlas, conmat = rat_conmat, mesh = mesh3d), "ggplot")
  expect_error(resolve_mesh(list(vertices = box$vertices, faces = box$faces + 100)), "1-based")
  expect_error(resolve_mesh(42), "mesh")
})

test_that("edge_table maps weights to colours, widths and directions", {
  m <- matrix(0, 3, 3)
  m[1, 2] <- 2; m[2, 1] <- 2; m[1, 3] <- -4; m[3, 1] <- -4
  e <- edge_table(m, "black", 1, TRUE, NULL, NULL, NULL, unit = 1)
  expect_equal(e$limits, c(-4, 4))
  expect_equal(nrow(e$table), 2)
  expect_false(any(e$table$directed))
  # Most negative weight gets the first palette colour
  expect_equal(e$table$color[e$table$weight == -4], e$palette[1])

  e <- edge_table(m, "black", 1, FALSE, NULL, NULL, c(1, 3), unit = 1)
  expect_equal(sort(e$table$radius * 2), c(1, 3))

  d <- matrix(0, 3, 3)
  d[1, 2] <- 1; d[2, 1] <- 1; d[2, 3] <- 1
  e <- edge_table(d, "black", 1, FALSE, NULL, NULL, NULL, unit = 1)
  expect_true(all(e$table$directed))
  expect_equal(sum(e$table$reciprocal), 2)

  empty <- edge_table(matrix(0, 2, 2), "black", 1, FALSE, NULL, NULL, NULL, unit = 1)
  expect_equal(nrow(empty$table), 0)
})

test_that("a transparent background keeps the translucent brain surface", {
  p <- render_small(atlas = "schaefer300_n7", conmat = example_unweighted_undirected,
                    bg.color = NA)
  layer <- Filter(function(l) inherits(l$geom, "GeomRasterAnn"), p$layers)[[1]]
  alpha <- grDevices::col2rgb(as.matrix(layer$geom_params$raster), alpha = TRUE)[4, ] / 255
  expect_equal(min(alpha), 0)             # background
  expect_gt(mean(alpha > 0.05 & alpha < 0.9), 0.2)  # glass brain, partly transparent
})
