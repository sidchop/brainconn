# Brain surface meshes.

# A mesh is a list of `vertices` (n x 3 numeric, x/y/z in the same space as
# the atlas coordinates) and `faces` (f x 3 integer, 1-based row indices of
# `vertices`).
new_brain_mesh <- function(vertices, faces) {
  vertices <- as.matrix(vertices)
  faces <- as.matrix(faces)
  if (!is.numeric(vertices) || ncol(vertices) != 3) {
    stop("Mesh vertices must be a numeric matrix with 3 columns (x, y, z).", call. = FALSE)
  }
  if (!is.numeric(faces) || ncol(faces) != 3) {
    stop("Mesh faces must be a matrix with 3 columns (vertex indices).", call. = FALSE)
  }
  storage.mode(faces) <- "integer"
  if (min(faces) < 1 || max(faces) > nrow(vertices)) {
    stop("Mesh faces must contain 1-based vertex indices between 1 and ", nrow(vertices),
         ".", call. = FALSE)
  }
  dimnames(vertices) <- NULL
  dimnames(faces) <- NULL
  structure(list(vertices = vertices, faces = faces), class = "brainconn_mesh")
}

# Turn the `mesh` argument of the 3D plotting functions into a brainconn_mesh
# (or NULL for no surface).
resolve_mesh <- function(mesh) {
  if (is.null(mesh)) return(NULL)
  if (inherits(mesh, "brainconn_mesh")) return(mesh)
  if (identical(mesh, "ICBM152")) {
    vb <- package_data("ICBM152_mesh_vb")
    return(new_brain_mesh(t(vb[1:3, ]), t(package_data("ICBM152_mesh_it"))))
  }
  if (inherits(mesh, "mesh3d")) {
    vb <- mesh$vb
    if (nrow(vb) == 4) vb <- vb[1:3, ] / rep(vb[4, ], each = 3)
    return(new_brain_mesh(t(vb), t(mesh$it)))
  }
  if (is.list(mesh) && all(c("vertices", "faces") %in% names(mesh))) {
    return(new_brain_mesh(mesh$vertices, mesh$faces))
  }
  stop("`mesh` must be \"ICBM152\", NULL, an rgl mesh3d object, or a list with ",
       "`vertices` and `faces`.", call. = FALSE)
}
