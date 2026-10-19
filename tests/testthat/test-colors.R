test_that("Yeo networks get the standard colours", {
  cols <- network_colors(schaefer300_n7$network)
  expect_equal(names(cols), unique(schaefer300_n7$network))
  expect_equal(cols[["Default"]], "#CD3E4E")
  expect_equal(cols[["Vis"]], "#781286")

  cols17 <- network_colors(schaefer300_n17$network)
  expect_length(cols17, 17)
  expect_equal(cols17[["DefaultA"]], "#FFFF00")
  expect_equal(cols17[["ContA"]], "#E69422")
  expect_equal(cols17[["TempPar"]], "#0C30FF")
})

test_that("common alternative network names get Yeo colours", {
  cols <- network_colors(c("Visual", "Dorsal Attention", "Frontoparietal", "Subcortical"))
  expect_equal(unname(cols), c("#781286", "#00760E", "#E69422", "#BF9959"))
  # stages_study has stray whitespace and a byte-order mark in one name
  expect_false(anyNA(network_colors(stages_study$network)))
  expect_equal(network_colors(stages_study$network)[["Default"]], "#CD3E4E")
})

test_that("other networks get evenly spaced colours", {
  cols <- network_colors(c("Frontal", "Temporal", "Frontal", "Occipital"))
  expect_equal(names(cols), c("Frontal", "Temporal", "Occipital"))
  expect_equal(unname(cols), scales::hue_pal()(3))
  expect_equal(names(network_colors(factor(c("b", "a"), levels = c("b", "a")))), c("b", "a"))
})

test_that("edge palettes and limits", {
  expect_length(edge_palette(NULL, signed = TRUE), 256)
  expect_equal(edge_palette(c("white", "black"), signed = FALSE, n = 3),
               c("#FFFFFF", "#7F7F7F", "#000000"))
  expect_error(edge_palette("not-a-palette", signed = FALSE), "Unknown palette")
  expect_equal(edge_limits(c(-1, 3)), c(-3, 3))
  expect_equal(edge_limits(c(2, 5)), c(2, 5))
  expect_equal(edge_limits(c(2, 2)), c(1.5, 2.5))
  expect_equal(map_colors(c(-10, 0, 10), c("a", "b", "c"), c(-1, 1)), c("a", "b", "c"))
})
