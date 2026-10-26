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
  if (is.character(mesh)) return(read_brain_mesh(mesh))
  stop("`mesh` must be \"ICBM152\", NULL, mesh file path(s), an rgl mesh3d object, or a ",
       "list with `vertices` and `faces`.", call. = FALSE)
}

# File formats read by freesurferformats, by file extension. Files without a
# known extension (e.g. lh.pial, rh.white) are FreeSurfer binary surfaces.
mesh_formats <- c(gii = "gii", ply = "ply", obj = "obj", stl = "stl", vtk = "vtk",
                  off = "off", mz3 = "mz3", asc = "asc", byu = "byu", geo = "geo",
                  tri = "tri", ico = "ico")

#' Read a brain surface mesh
#'
#' Reads one or more surface mesh files for use as the brain surface in
#' \code{\link{brainconn}} and \code{\link{brainconn3D}} (their \code{mesh}
#' argument also accepts the file paths directly). Several files, e.g. a left
#' and a right hemisphere, are combined into one mesh. This makes it possible
#' to plot connectivity on any brain, such as a macaque or rat template, as long
#' as the atlas coordinates are in the same space as the mesh.
#'
#' Supported formats (by file extension): GIfTI (\code{.gii}), PLY, OBJ, STL,
#' VTK (ASCII), OFF, MZ3, FreeSurfer ASCII (\code{.asc}), BYU, GEO, TRI and ICO.
#' Files with any other extension, such as \code{lh.pial} or
#' \code{rh.white}, are read as FreeSurfer binary surfaces. Files are read with
#' the freesurferformats package, which must be installed (GIfTI files also
#' need the gifti package).
#'
#' Coordinates are assumed to be RAS: x increases to the subject's right, y to
#' the front and z upwards, as in MNI space.
#'
#' @param file Path(s) to mesh files.
#' @return A \code{brainconn_mesh}: a list with \code{vertices} (an n x 3
#'   matrix of coordinates) and \code{faces} (an f x 3 matrix of 1-based vertex
#'   indices).
#' @export
#' @seealso \code{\link{rat_brain}} and \code{\link{macaque_brain}} for rat and
#'   macaque meshes and atlases included with brainconn.
#' @examples
#' \dontrun{
#' # A macaque template surface (e.g. from the NMT), with an atlas in the same space
#' mesh <- read_brain_mesh(c("lh.surf.gii", "rh.surf.gii"))
#' brainconn(atlas = macaque_atlas, conmat = macaque_conmat, mesh = mesh)
#' brainconn3D(atlas = macaque_atlas, conmat = macaque_conmat, mesh = mesh)
#' }
read_brain_mesh <- function(file) {
  if (!requireNamespace("freesurferformats", quietly = TRUE)) {
    stop("Reading mesh files needs the freesurferformats package: ",
         "install.packages(\"freesurferformats\")", call. = FALSE)
  }
  missing <- file[!file.exists(file)]
  if (length(missing) > 0) {
    stop("Mesh file not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  parts <- lapply(file, function(f) {
    ext <- tolower(tools::file_ext(sub("[.]gz$", "", f)))
    format <- if (ext %in% names(mesh_formats)) mesh_formats[[ext]] else "bin"
    if (format == "gii" && !requireNamespace("gifti", quietly = TRUE)) {
      stop("Reading GIfTI files needs the gifti package: install.packages(\"gifti\")",
           call. = FALSE)
    }
    surface <- freesurferformats::read.fs.surface(f, format = format)
    list(vertices = surface$vertices, faces = surface$faces)
  })
  offset <- cumsum(c(0, vapply(parts, function(m) nrow(m$vertices), numeric(1))))
  new_brain_mesh(do.call(rbind, lapply(parts, `[[`, "vertices")),
                 do.call(rbind, lapply(seq_along(parts), function(i) parts[[i]]$faces + offset[i])))
}

#' @export
print.brainconn_mesh <- function(x, ...) {
  r <- apply(x$vertices, 2, range)
  cat("<brainconn_mesh> ", nrow(x$vertices), " vertices, ", nrow(x$faces), " faces\n", sep = "")
  cat("  x: ", signif(r[1, 1], 4), " to ", signif(r[2, 1], 4),
      ", y: ", signif(r[1, 2], 4), " to ", signif(r[2, 2], 4),
      ", z: ", signif(r[1, 3], 4), " to ", signif(r[2, 3], 4), "\n", sep = "")
  invisible(x)
}
