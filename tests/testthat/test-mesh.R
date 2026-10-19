test_that("meshes are read from files in many formats", {
  skip_if_not_installed("freesurferformats")
  m <- ellipsoid_mesh()
  writers <- list(
    gii = freesurferformats::write.fs.surface.gii,
    ply = freesurferformats::write.fs.surface.ply,
    obj = freesurferformats::write.fs.surface.obj,
    stl = freesurferformats::write.fs.surface.stl,
    vtk = freesurferformats::write.fs.surface.vtk,
    pial = freesurferformats::write.fs.surface   # FreeSurfer binary, e.g. lh.pial
  )
  if (!requireNamespace("gifti", quietly = TRUE)) writers$gii <- NULL
  for (ext in names(writers)) {
    file <- tempfile(fileext = paste0(".", ext))
    writers[[ext]](file, m$vertices, m$faces)
    mesh <- read_brain_mesh(file)
    expect_s3_class(mesh, "brainconn_mesh")
    expect_equal(nrow(mesh$faces), nrow(m$faces), label = ext)
    expect_equal(range(mesh$vertices[, 2]), range(m$vertices[, 2]), tolerance = 1e-3,
                 label = ext)
    # Faces still refer to the same corners
    expect_equal(mesh$vertices[mesh$faces[1, ], ], m$vertices[m$faces[1, ], ],
                 tolerance = 1e-3, label = ext)
  }
})

test_that("several files are combined into one mesh", {
  skip_if_not_installed("freesurferformats")
  skip_if_not_installed("gifti")
  left <- ellipsoid_mesh()
  left$vertices[, 1] <- left$vertices[, 1] - 7
  right <- ellipsoid_mesh()
  right$vertices[, 1] <- right$vertices[, 1] + 7
  files <- c(tempfile(fileext = ".gii"), tempfile(fileext = ".gii"))
  freesurferformats::write.fs.surface.gii(files[1], left$vertices, left$faces)
  freesurferformats::write.fs.surface.gii(files[2], right$vertices, right$faces)
  both <- read_brain_mesh(files)
  expect_equal(nrow(both$vertices), 2 * nrow(left$vertices))
  expect_equal(nrow(both$faces), 2 * nrow(left$faces))
  # The right hemisphere's faces point at its own (offset) vertices
  expect_true(all(both$vertices[both$faces[nrow(left$faces) + 1, ], 1] > 0))
  expect_output(print(both), "brainconn_mesh")
  expect_error(read_brain_mesh("no_such_file.gii"), "not found")
})

test_that("mesh files work in brainconn() and brainconn3D(), with sizes that fit the mesh", {
  skip_if_not_installed("freesurferformats")
  m <- ellipsoid_mesh()
  file <- tempfile(fileext = ".ply")
  freesurferformats::write.fs.surface.ply(file, m$vertices, m$faces)
  p <- suppressMessages(brainconn(rat_atlas, conmat = rat_conmat, mesh = file, view = "top",
                                  res = 200, quality = 1, labels = TRUE))
  panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  # The view is fitted to the 12 x 22 mm mesh, not to MNI space
  expect_lt(diff(panel$x.range), 15)
  expect_lt(diff(panel$y.range), 25)
  # Labels are placed on the left and right of the image for left and right regions
  labels <- Filter(function(l) inherits(l$geom, "GeomTextRepel"), p$layers)[[1]]$data
  expect_true(all(labels$x[startsWith(labels$label, "L_")] < 0))
  expect_true(all(labels$x[startsWith(labels$label, "R_")] > 0))

  p3d <- brainconn3D(rat_atlas, conmat = rat_conmat, mesh = file)
  mesh_trace <- plotly::plotly_build(p3d)$x$data[[1]]
  expect_equal(mesh_trace$type, "mesh3d")
  expect_length(mesh_trace$x, nrow(m$vertices))
})
