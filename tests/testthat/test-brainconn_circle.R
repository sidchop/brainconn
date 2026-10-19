# A small network x network matrix
chord_net <- matrix(c(0.5, 0.2, 0, -0.4,
                      0.2, 0, 0.1, 0,
                      0, 0.1, 0.3, 0,
                      -0.4, 0, 0, 0), 4, 4,
                    dimnames = list(c("A1", "A2", "B1", "B2"), c("A1", "A2", "B1", "B2")))

angle_of <- function(x, y) (atan2(y, x) * 180 / pi) %% 360

# Is an angle (degrees) inside a sector that runs clockwise from start to end?
in_sector <- function(a, start, end) {
  width <- start - end
  ((start - a) %% 360) <= width + 1e-6
}

test_that("one ribbon per non-zero link, drawn weakest first", {
  links <- chord_links(chord_net, NULL, NULL)
  # Upper triangle with diagonal: A1-A1, A1-A2, A2-B1, A1-B2, B1-B1
  expect_equal(nrow(links), 5)
  expect_equal(abs(links$value), sort(abs(links$value)))
  expect_equal(nrow(chord_links(chord_net, thr = 0.25, NULL)), 3)
  expect_equal(sort(abs(chord_links(chord_net, NULL, top = 40)$value)), c(0.4, 0.5))
})

test_that("every ribbon starts and ends inside its networks' sectors", {
  links <- chord_links(chord_net, NULL, NULL)
  sectors <- chord_sectors(4, NULL, 2, 6)
  ribbons <- chord_ribbons(links, sectors)
  on_circle <- abs(sqrt(ribbons$x^2 + ribbons$y^2) - 1) < 1e-9
  for (l in links$link) {
    pts <- ribbons[ribbons$link == l & on_circle, ]
    a <- angle_of(pts$x, pts$y)
    from <- links$from[links$link == l]
    to <- links$to[links$link == l]
    ok <- in_sector(a, sectors$start[from] %% 360, sectors$end[from] %% 360) |
      in_sector(a, sectors$start[to] %% 360, sectors$end[to] %% 360)
    expect_true(all(ok), label = paste("link", l))
  }
})

test_that("each sector is split into one slot per link end, without overlaps", {
  links <- chord_links(chord_net, NULL, NULL)
  sectors <- chord_sectors(4, NULL, 2, 6)
  ribbons <- chord_ribbons(links, sectors)
  on_circle <- abs(sqrt(ribbons$x^2 + ribbons$y^2) - 1) < 1e-9
  pts <- ribbons[on_circle, ]
  pts$angle <- angle_of(pts$x, pts$y)
  # A1 has 4 link ends (its own loop counts twice, plus A2 and B2)
  in_a1 <- in_sector(pts$angle, sectors$start[1] %% 360, sectors$end[1] %% 360)
  expect_equal(sum(table(pts$link[in_a1]) > 0), 3)
  # Slots in a sector don't overlap
  arcs <- do.call(rbind, lapply(split(pts[in_a1, ], pts$link[in_a1]), function(d) {
    # a loop has two separate arcs in the sector; split them by a gap in angle
    a <- sort(d$angle)
    breaks <- which(diff(a) > 1)
    starts <- c(1, breaks + 1)
    ends <- c(breaks, length(a))
    data.frame(lo = a[starts], hi = a[ends])
  }))
  arcs <- arcs[order(arcs$lo), ]
  expect_equal(nrow(arcs), 4)
  expect_true(all(arcs$lo[-1] >= arcs$hi[-nrow(arcs)] - 1e-9))
})

test_that("sectors run clockwise from the top, with wider gaps between groups", {
  s <- chord_sectors(4, NULL, 2, 6)
  expect_equal(s$start[1], 89)
  expect_true(all(diff(s$start) < 0))
  expect_equal(unique(round(s$start - s$end, 8)), 88)
  g <- chord_sectors(4, factor(c("A", "A", "B", "B")), 2, 6)
  gaps <- g$end[-4] - g$start[-1]
  expect_equal(gaps, c(2, 6, 2))
})

test_that("brainconn_circle summarises region matrices by network", {
  p <- brainconn_circle(example_fc_schaefer300, atlas = "schaefer300_n7")
  expect_s3_class(p, "ggplot")
  ribbons <- p$layers[[1]]$data
  # 7 networks: 28 pairs (with within-network loops), all non-zero
  expect_equal(length(unique(ribbons$link)), 28)
  fill <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")
  expect_equal(fill$get_limits()[1], -fill$get_limits()[2])
  labels <- p$layers[[3]]$data$label
  expect_equal(labels, unique(schaefer300_n7$network))
})

test_that("groups add an outer ring and shorten network labels", {
  set.seed(1)
  m <- matrix(rnorm(300^2), 300)
  m <- m + t(m)
  p <- brainconn_circle(m, atlas = "schaefer300_n17", group = "network7", top = 20)
  texts <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  inner <- texts[[1]]$data$label
  outer <- texts[[2]]$data$label
  expect_true(all(c("A", "B", "C", "Cent", "Peri", "TempPar") %in% inner))
  expect_equal(outer, c("Vis", "SomMot", "DorsAttn", "SalVentAttn", "Limbic", "Cont", "Default"))

  named <- brainconn_circle(chord_net, group = c(A1 = "A", A2 = "A", B1 = "B", B2 = "B"))
  texts <- Filter(function(l) inherits(l$geom, "GeomText"), named$layers)
  expect_equal(texts[[1]]$data$label, c("1", "2", "1", "2"))
  expect_error(brainconn_circle(chord_net, group = c(A1 = "A")), "no group for")
  expect_error(brainconn_circle(chord_net, group = "network7"), "column")
})

test_that("directed matrices get a ribbon per direction", {
  d <- matrix(0, 3, 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  d[1, 2] <- 1
  d[2, 1] <- -1
  expect_equal(nrow(chord_links(d, NULL, NULL)), 2)
  expect_s3_class(brainconn_circle(d), "ggplot")
})

test_that("brainconn_circle gives clear errors and handles no links", {
  expect_error(brainconn_circle(matrix(1, 3, 3)), "row names")
  expect_message(p <- brainconn_circle(chord_net, thr = 10), "No links")
  expect_s3_class(p, "ggplot")
})
