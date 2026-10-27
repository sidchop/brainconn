# Tests for the experimental realistic style of brainconn(). Most use a small
# ellipsoid brain, so they are quick.

# The rendered image of a brainconn() plot, as an RGBA array with values 0-1
image_of <- function(p) {
  layer <- Filter(function(l) inherits(l$geom, "GeomRasterAnn"), p$layers)[[1]]
  r <- as.matrix(layer$geom_params$raster)
  array(t(grDevices::col2rgb(r, alpha = TRUE) / 255), c(dim(r), 4))
}

egg <- ellipsoid_mesh(n_lat = 40, n_lon = 60)

# The brain is path traced with rayrender, which needs XQuartz on macOS
skip_if_no_rayrender <- function() {
  skip_if_not(requireNamespace("rayrender", quietly = TRUE), "rayrender can't be loaded")
}

render_realistic_small <- function(...) {
  suppressMessages(brainconn(toy_rat_atlas, toy_rat_conmat, mesh = egg, style = "realistic",
                             res = 160, quality = 1, ...))
}

test_that("tube_mesh makes rings of the given radius, facing outwards", {
  m <- tube_mesh(list(cbind(0, 0, 0:4)), list(rep(0.5, 5)), sides = 6)
  expect_equal(nrow(m$vertices), 5 * 6)
  expect_equal(nrow(m$faces), 4 * 6 * 2)
  expect_true(all(m$faces >= 1 & m$faces <= 30))
  expect_equal(sqrt(rowSums(m$vertices[, 1:2]^2)), rep(0.5, 30))
  expect_equal(rowSums(m$normals^2), rep(1, 30))
  # Each face's normal (from its winding) points away from the tube's axis
  for (k in seq_len(nrow(m$faces))) {
    v <- m$vertices[m$faces[k, ], ]
    n <- cross3(v[2, ] - v[1, ], v[3, ] - v[1, ])
    expect_gt(sum(n[1:2] * colMeans(v)[1:2]), 0)
  }
})

test_that("vertex normals point out of the brain, whichever way faces are wound", {
  m <- resolve_mesh(egg)
  n <- vertex_normals(m$vertices, m$faces)
  expect_true(all(rowSums(n * m$vertices) > 0))
  n2 <- vertex_normals(m$vertices, m$faces[, 3:1])
  expect_equal(n2, n)
})

test_that("vessels are the same every time and leave the user's random numbers alone", {
  mesh <- resolve_mesh(egg)
  unit <- 22 / 170
  make <- function() with_seed(1, brain_vessels(brain_geometry(mesh, unit), unit))
  set.seed(5)
  expected <- runif(1)
  set.seed(5)
  a <- make()
  expect_equal(runif(1), expected)
  expect_identical(make(), a)
  expect_gt(nrow(a$veins$faces), 0)
  expect_gt(nrow(a$arteries$faces), 0)
  # Vessels lie on the brain surface (up to a little above it)
  r <- sqrt((a$veins$vertices[, 1] / 6)^2 + (a$veins$vertices[, 2] / 11)^2 +
              (a$veins$vertices[, 3] / 4.5)^2)
  expect_gt(stats::quantile(r, 0.1), 0.9)
  expect_lt(stats::quantile(r, 0.9), 1.2)
})

test_that("the realistic style renders a tissue-coloured brain on a dark background", {
  skip_if_no_rayrender()
  p <- render_realistic_small(view = "left")
  expect_s3_class(p, "ggplot")
  img <- image_of(p)
  bg <- unname(grDevices::col2rgb("grey5")[, 1] / 255)
  expect_equal(img[1, 1, 1:3], bg, tolerance = 0.01)
  brain <- img[, , 1] + img[, , 2] + img[, , 3] > 0.6
  expect_gt(mean(brain), 0.3)
  # Pinkish: more red than green or blue
  expect_gt(mean(img[, , 1][brain]), mean(img[, , 3][brain]) + 0.1)
  # The default edge colour is light, and L/R labels and legends are white
  expect_identical(p$theme$text$colour, "white")
})

test_that("the realistic style draws nodes over the opaque brain", {
  skip_if_no_rayrender()
  # A big green node inside the brain shows through it
  atlas <- toy_rat_atlas
  atlas$network <- "A"
  p <- suppressMessages(brainconn(atlas, all.nodes = TRUE, mesh = egg, style = "realistic",
                                  view = "top", res = 160, quality = 1, node.size = 30,
                                  node.color = "green", hemi.labels = FALSE,
                                  show.legend = FALSE))
  img <- image_of(p)
  green <- img[, , 2] > img[, , 1] + 0.2 & img[, , 2] > img[, , 3] + 0.2
  expect_gt(sum(green), 50)
})

test_that("realistic renders work without a mesh and on a transparent background", {
  p <- suppressMessages(brainconn(toy_rat_atlas, toy_rat_conmat, mesh = NULL,
                                  style = "realistic", res = 120, quality = 1, bg.color = NA))
  img <- image_of(p)
  expect_equal(min(img[, , 4]), 0)
  expect_gt(max(img[, , 4]), 0.8)
  skip_if_no_rayrender()
  p <- render_realistic_small(bg.color = NA)
  expect_gt(mean(image_of(p)[, , 4] > 0.95), 0.3)  # the opaque brain
})

test_that("background colours come out as given in both styles", {
  skip_if_no_rayrender()
  for (style in c("glass", "realistic")) {
    p <- suppressMessages(brainconn(toy_rat_atlas, toy_rat_conmat, mesh = egg, style = style,
                                    res = 120, quality = 1, bg.color = "grey40"))
    expect_equal(image_of(p)[1, 1, 1:3], rep(0.4, 3), tolerance = 0.01)
  }
})

test_that("text is white on dark backgrounds and black otherwise", {
  expect_identical(text_color("grey5"), "white")
  expect_identical(text_color("navy"), "white")
  expect_identical(text_color("white"), "black")
  expect_identical(text_color("lightyellow"), "black")
  expect_identical(text_color(NA), "black")
})

test_that("the realistic style works on the human brain", {
  skip_on_cran()
  skip_if_no_rayrender()
  p <- suppressMessages(brainconn("schaefer300_n7", example_unweighted_undirected,
                                  style = "realistic", view = "ortho", res = 150, quality = 1))
  expect_s3_class(p, "ggplot")
})

test_that("nodes fall, bounce lower each time, and come to rest", {
  parts <- realistic_parts(resolve_mesh(egg), 22 / 170)
  plan <- with_seed(1, bounce_plan(parts, data.frame(x = c(-3, 3), y = c(6, -6),
                                                     z = c(1, 0), radius = 0.3), roll = 0))
  expect_false(any(plan$roller))
  for (i in 1:2) {
    hits <- plan$impacts[plan$impacts$node == i, ]
    expect_equal(hits$time[1], plan$delay[i] + plan$fall)
    expect_true(all(diff(hits$time) > 0))
    expect_equal(hits$speed[-1] / hits$speed[-nrow(hits)], rep(0.5, nrow(hits) - 1))
    expect_equal(bounce_height(plan, i, plan$delay[i] - 0.01), plan$drop)
    expect_equal(bounce_height(plan, i, hits$time[1]), 0, tolerance = 1e-9)
    expect_equal(bounce_height(plan, i, plan$rest[i] + 0.5), 0)
    heights <- vapply(seq(0, plan$end, by = 0.01), function(t) bounce_height(plan, i, t), 1)
    expect_true(all(heights >= 0 & heights <= plan$drop))
  }
  # Landing points are on the top of the surface
  expect_true(all(plan$normal[, 3] > 0.2))
})

test_that("the brain squishes where nodes land and then settles", {
  parts <- realistic_parts(resolve_mesh(egg), 22 / 170)
  plan <- with_seed(1, bounce_plan(parts, data.frame(x = 0, y = 0, z = 0, radius = 0.3),
                                   roll = 0))
  v <- parts$mesh$vertices
  hit <- plan$impacts$time[1]
  expect_identical(squish_points(v, plan, hit + 0.06, squish = 0, unit = 22 / 170), v)
  moved <- squish_points(v, plan, hit + 0.06, squish = 1, unit = 22 / 170)
  shift <- sqrt(rowSums((moved - v)^2))
  near <- which.min(rowSums(sweep(v, 2, plan$point[1, ])^2))
  # The landing point is pushed in, more than the far side of the brain
  expect_lt(sum((moved[near, ] - v[near, ]) * plan$normal[1, ]), 0)
  expect_gt(shift[near], stats::median(shift))
  expect_equal(squish_points(v, plan, max(plan$impacts$time) + 2, 1, 22 / 170), v)
})

test_that("some nodes roll downhill and fall off the brain, the others stay", {
  parts <- realistic_parts(resolve_mesh(egg), 22 / 170)
  nodes <- data.frame(x = c(-4, 4, -3, 3), y = c(5, -5, -5, 5), z = 0, radius = 0.3)
  plan <- with_seed(2, bounce_plan(parts, nodes, roll = 1))
  expect_true(any(plan$roller))
  for (i in which(plan$roller)) {
    path <- plan$path[[i]]
    expect_lt(path[nrow(path), 3], path[1, 3])
    expect_true(all(diff(plan$path_time[[i]]) >= 0))
    # Rolling starts where the bouncing stopped
    expect_equal(node_position(plan, i, plan$rest[i]), plan$centre[i, ])
    expect_null(node_position(plan, i, plan$gone[i] + 0.01))
    expect_lt(plan$gone[i], plan$sink[1])
  }
  none <- with_seed(2, bounce_plan(parts, nodes, roll = 0))
  for (i in seq_len(nrow(nodes))) {
    expect_equal(node_position(none, i, none$end), none$centre[i, ])
  }
})

test_that("brainconn_bounce writes a GIF", {
  skip_on_cran()
  skip_if_no_rayrender()
  skip_if_not_installed("gifski")
  file <- tempfile(fileext = ".gif")
  out <- brainconn_bounce(toy_rat_atlas, toy_rat_conmat, mesh = egg, file = file, res = 80,
                          fps = 2)
  expect_identical(out, file)
  expect_identical(readBin(file, "raw", 6), charToRaw("GIF89a"))
})
