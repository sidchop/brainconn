# Regions given out of network order: A, B, A, B
mat_net <- c("A", "B", "A", "B")
mat_x <- matrix(c(1, 0.1, 0.5, 0.2,
                  0.1, 1, -0.3, 0.6,
                  0.5, -0.3, 1, 0.4,
                  0.2, 0.6, 0.4, 1), 4, 4, byrow = TRUE)

layer_data_of <- function(p, geom) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))[1]
  ggplot2::layer_data(p, i)
}

# Cell values as a matrix, from a region-level (raster) plot
cell_matrix <- function(p, k) {
  d <- p$layers[[1]]$data
  out <- matrix(NA_real_, k, k)
  out[cbind(d$i, d$j)] <- d$value
  out
}

test_that("regions are ordered by network", {
  p <- brainconn_matrix(mat_x, networks = mat_net)
  m <- cell_matrix(p, 4)
  # New order: regions 1, 3 (A), then 2, 4 (B)
  expect_equal(m[1, 2], mat_x[1, 3])
  expect_equal(m[3, 4], mat_x[2, 4])
  expect_true(all(is.na(diag(m))))
  # Keep the input order when asked
  m <- cell_matrix(brainconn_matrix(mat_x, networks = mat_net, order = FALSE, diag = TRUE), 4)
  expect_equal(m, mat_x)
})

test_that("strips, borders and labels follow the networks", {
  p <- brainconn_matrix(mat_x, networks = mat_net)
  strips <- layer_data_of(p, "GeomPolygon")
  # Two networks on two sides
  expect_equal(length(unique(strips$group)), 4)
  expect_setequal(unique(strips$fill), network_colors(c("A", "B")))
  borders <- layer_data_of(p, "GeomSegment")
  # One boundary (after position 2): one vertical and one horizontal line
  expect_equal(nrow(borders), 2)
  expect_equal(borders$x[1], 2.5)
  labels <- layer_data_of(p, "GeomText")
  expect_equal(sort(unique(labels$label)), c("A", "B"))
})

test_that("triangles keep the right cells, with conmat2 in the upper triangle", {
  lower <- cell_matrix(brainconn_matrix(mat_x, networks = mat_net, triangle = "lower"), 4)
  expect_true(all(is.na(lower[upper.tri(lower)])))
  expect_false(anyNA(lower[lower.tri(lower)]))
  upper <- cell_matrix(brainconn_matrix(mat_x, networks = mat_net, triangle = "upper"), 4)
  expect_true(all(is.na(upper[lower.tri(upper)])))

  both <- cell_matrix(brainconn_matrix(mat_x, conmat2 = -mat_x, networks = mat_net), 4)
  ord <- c(1, 3, 2, 4)
  expect_equal(both[lower.tri(both)], mat_x[ord, ord][lower.tri(both)])
  expect_equal(both[upper.tri(both)], -mat_x[ord, ord][upper.tri(both)])
})

test_that("smoothing gives each network pair a single value", {
  m <- cell_matrix(brainconn_matrix(mat_x, networks = mat_net, smooth = TRUE), 4)
  net <- network_matrix(mat_x, mat_net)
  expect_equal(m[1:2, 3:4], matrix(net["A", "B"], 2, 2))
  expect_equal(m[1, 1], net["A", "A"])
})

test_that("network level plots one tile per network pair, with values", {
  p <- brainconn_matrix(example_fc_schaefer300, atlas = "schaefer300_n7", level = "network",
                        values = TRUE)
  tiles <- layer_data_of(p, "GeomTile")
  expect_equal(nrow(tiles), 49)
  txt <- p$layers[[2]]$data
  expect_equal(nrow(txt), 49)
  expect_equal(txt$label[1], formatC(txt$value[1], digits = 2, format = "f"))
  counts <- brainconn_matrix(example_unweighted_undirected, atlas = "schaefer300_n7",
                             level = "network", fun = "count", values = TRUE)
  expect_true(all(c("0", "4") %in% counts$layers[[2]]$data$label))
  p <- brainconn_matrix(example_fc_schaefer300, atlas = "schaefer300_n7", level = "network",
                        triangle = "lower")
  expect_equal(nrow(layer_data_of(p, "GeomTile")), 28)
})

test_that("colour limits are symmetric for signed values", {
  p <- brainconn_matrix(mat_x, networks = mat_net)
  fill <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")
  expect_equal(fill$get_limits(), c(-0.6, 0.6))
  p <- brainconn_matrix(abs(mat_x), networks = mat_net, limits = c(0, 2))
  expect_equal(ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$get_limits(), c(0, 2))
})

test_that("diamond plots rotate the lower triangle", {
  p <- brainconn_matrix(mat_x, networks = mat_net, diamond = TRUE)
  d <- p$layers[[1]]$data
  centres <- aggregate(cbind(x, y) ~ cell, d, mean)
  # Cell ids follow the lower-triangle cells in column-major order
  ij <- expand.grid(i = 1:4, j = 1:4)
  ij <- ij[ij$i >= ij$j, ]
  centres$i <- ij$i[centres$cell]
  centres$j <- ij$j[centres$cell]
  diag_y <- centres$y[centres$i == centres$j]
  # The diagonal is horizontal and along the top
  expect_lt(diff(range(diag_y)), 1e-8)
  expect_equal(max(centres$y), diag_y[1])
  # The bottom-left cell of the matrix is the lowest point, in the middle
  apex <- centres[centres$i == 4 & centres$j == 1, ]
  expect_equal(apex$y, min(centres$y))
  expect_equal(apex$x, 0, tolerance = 1e-8)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plots without networks, and clear errors", {
  p <- brainconn_matrix(mat_x)
  expect_length(p$layers, 1)
  expect_error(brainconn_matrix(mat_x, level = "network"), "atlas")
  expect_error(brainconn_matrix(mat_x, atlas = "schaefer300_n7"), "300")
  expect_error(brainconn_matrix(mat_x, conmat2 = diag(3), networks = mat_net), "same size")
})
