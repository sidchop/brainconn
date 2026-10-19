# Four regions: a1, a2 in network A; b1, b2 in network B
toy_net <- c("A", "A", "B", "B")
toy_und <- matrix(c(0, 2, 1, 0,
                    2, 0, 0, 3,
                    1, 0, 0, 4,
                    0, 3, 4, 0), 4, 4, byrow = TRUE)

test_that("mean, sum, count and density of an undirected matrix", {
  # Within A: one pair (a1-a2) = 2. Within B: one pair (b1-b2) = 4.
  # Between: a1-b1 = 1, a1-b2 = 0, a2-b1 = 0, a2-b2 = 3.
  m <- network_matrix(toy_und, toy_net)
  expect_equal(dimnames(m), list(c("A", "B"), c("A", "B")))
  expect_equal(m, matrix(c(2, 1, 1, 4), 2, dimnames = dimnames(m)))
  expect_equal(network_matrix(toy_und, toy_net, "sum")["A", "B"], 4)
  expect_equal(network_matrix(toy_und, toy_net, "count"),
               matrix(c(1, 2, 2, 1), 2, dimnames = dimnames(m)))
  expect_equal(network_matrix(toy_und, toy_net, "density")["A", "B"], 0.5)
  expect_equal(network_matrix(toy_und, toy_net, "median")["A", "B"], 0.5)
  expect_equal(network_matrix(toy_und, toy_net, max)["A", "B"], 3)
})

test_that("self-connections are left out", {
  x <- toy_und
  diag(x) <- 100
  expect_equal(network_matrix(x, toy_net), network_matrix(toy_und, toy_net))
})

test_that("directed matrices summarise edges from one network to another", {
  d <- matrix(0, 4, 4)
  d[1, 3] <- 5   # A -> B
  d[4, 2] <- 1   # B -> A
  d[1, 2] <- 2   # A -> A
  m <- network_matrix(d, toy_net, "sum")
  expect_equal(m["A", "B"], 5)
  expect_equal(m["B", "A"], 1)
  expect_equal(m["A", "A"], 2)
  # Within a network both directions are possible: 2 of 2 pairs
  expect_equal(network_matrix(d, toy_net, "count")["A", "A"], 1)
  expect_equal(network_matrix(d, toy_net, "density")["A", "A"], 0.5)
})

test_that("full = TRUE gives a smoothed region x region matrix", {
  s <- network_matrix(toy_und, toy_net, full = TRUE)
  expect_equal(dim(s), c(4, 4))
  expect_equal(s[1, 2], 2)
  expect_equal(s[1, 1], 2)
  expect_equal(s[3, 1], 1)
  expect_equal(s[4, 4], 4)
})

test_that("networks can come from an atlas, and keep factor or appearance order", {
  m <- network_matrix(example_fc_schaefer300, "schaefer300_n7")
  expect_equal(rownames(m), unique(schaefer300_n7$network))
  expect_true(isSymmetric(m))
  expect_equal(m, network_matrix(example_fc_schaefer300, schaefer300_n7))
  f <- factor(toy_net, levels = c("B", "A"))
  expect_equal(rownames(network_matrix(toy_und, f)), c("B", "A"))
  # Counts add up to the number of edges
  x <- as.matrix(example_unweighted_undirected)
  counts <- network_matrix(x, "schaefer300_n7", "count")
  expect_equal(sum(counts[upper.tri(counts, diag = TRUE)]), sum(x[upper.tri(x)] != 0))
})

test_that("network_matrix gives clear errors", {
  expect_error(network_matrix(toy_und, c("A", "B")), "4 regions")
  expect_error(network_matrix(matrix(0, 2, 3), c("A", "B")), "square")
  expect_error(network_matrix(toy_und, c("A", NA, "B", "B")), "NA")
  expect_error(network_matrix(toy_und, toy_net, "mode"))
})
