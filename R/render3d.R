# Static 3D rendering for brainconn(), using the rayvertex software rasterizer.
#
# Coordinates are assumed to be RAS: x points right, y anterior, z superior
# (as in MNI space). All cameras are orthographic.

# Named views. `dir` points from the brain towards the camera and `up` is the
# screen's up direction. `mirror` flips the rendered image left-right so that
# the subject's left hemisphere is on the left of the plot, as in brainconn2d().
camera_views <- list(
  top    = list(dir = c(0, 0, 1),  up = c(0, 1, 0), mirror = FALSE, hemi = TRUE),
  bottom = list(dir = c(0, 0, -1), up = c(0, 1, 0), mirror = TRUE,  hemi = TRUE),
  left   = list(dir = c(-1, 0, 0), up = c(0, 0, 1), mirror = FALSE, hemi = FALSE),
  right  = list(dir = c(1, 0, 0),  up = c(0, 0, 1), mirror = FALSE, hemi = FALSE),
  front  = list(dir = c(0, 1, 0),  up = c(0, 0, 1), mirror = TRUE,  hemi = TRUE),
  back   = list(dir = c(0, -1, 0), up = c(0, 0, 1), mirror = FALSE, hemi = TRUE)
)

cross3 <- function(a, b) {
  c(a[2] * b[3] - a[3] * b[2], a[3] * b[1] - a[1] * b[3], a[1] * b[2] - a[2] * b[1])
}
unit3 <- function(a) a / sqrt(sum(a^2))

# Camera direction for a named view, or for c(azimuth, elevation) in degrees:
# azimuth 0 = in front of the subject, 90 = to their right, 180 = behind,
# -90 = to their left; elevation 90 = above, -90 = below.
view_camera <- function(view) {
  if (is.character(view)) return(camera_views[[view]])
  az <- view[1] * pi / 180
  el <- view[2] * pi / 180
  dir <- c(sin(az) * cos(el), cos(az) * cos(el), sin(el))
  up <- c(0, 0, 1) - dir * dir[3]
  if (sum(up^2) < 1e-12) up <- c(-sin(az), -cos(az), 0) * sign(view[2])
  list(dir = dir, up = unit3(up), mirror = FALSE, hemi = FALSE)
}

# Orthonormal screen axes (right, up) for a camera.
camera_basis <- function(cam) {
  forward <- -cam$dir
  right <- unit3(cross3(forward, cam$up))
  list(right = right, up = cross3(right, forward))
}

# Fit an orthographic camera around `points` (n x 3), with a margin around
# them. Returns the camera with lookat/lookfrom, the screen basis and the
# width/height of the view in data units.
fit_camera <- function(cam, points, margin = 0.04) {
  basis <- camera_basis(cam)
  sx <- range(points %*% basis$right)
  sy <- range(points %*% basis$up)
  depth <- sum(colMeans(apply(points, 2, range)) * cam$dir)
  lookat <- basis$right * mean(sx) + basis$up * mean(sy) + cam$dir * depth
  extent <- max(apply(points, 2, function(p) diff(range(p))))
  pad <- margin * max(diff(sx), diff(sy))
  c(cam, basis, list(lookat = lookat,
                     lookfrom = lookat + cam$dir * (2 * extent + 10),
                     far = 4 * extent + 40,
                     width = diff(sx) + 2 * pad,
                     height = diff(sy) + 2 * pad))
}

# Screen positions of points (n x 3) in a fitted camera, in data units with
# (0, 0) at the centre of the image. Matches the (possibly mirrored) image.
project_points <- function(camera, points) {
  rel <- sweep(points, 2, camera$lookat)
  x <- as.vector(rel %*% camera$right)
  if (camera$mirror) x <- -x
  data.frame(x = x, y = as.vector(rel %*% camera$up))
}

# Lights that follow the camera: a key light above and to the right of the
# viewer, and a weaker fill light from the other side.
camera_lights <- function(camera) {
  key <- camera$dir + 0.5 * camera$up + 0.3 * camera$right
  fill <- camera$dir - 0.4 * camera$right - 0.2 * camera$up
  rayvertex::add_light(rayvertex::directional_light(key, intensity = 0.85),
                       rayvertex::directional_light(fill, intensity = 0.35))
}

shiny_material <- function(color, ambient = 0.3, specular = 0.5, dissolve = 1) {
  rayvertex::material_list(type = "phong", diffuse = color, ambient = color,
                           ambient_intensity = ambient, specular = "white",
                           specular_intensity = specular, shininess = 30,
                           dissolve = dissolve)
}

# rayvertex objects for the brain surface, nodes and edges.
#   nodes: data.frame with x, y, z, color, radius
#   edges: data.frame with from/to node row indices, color, radius, directed,
#          reciprocal (edge also exists in the other direction)
scene_objects <- function(mesh, mesh.color, mesh.alpha, nodes, edges, edge.alpha) {
  objects <- list()
  if (!is.null(mesh)) {
    surface <- rayvertex::construct_mesh(
      vertices = mesh$vertices, indices = mesh$faces - 1L,
      material = shiny_material(mesh.color, ambient = 0.25, specular = 0.4, dissolve = mesh.alpha))
    objects[[1]] <- rayvertex::smooth_normals_mesh(surface)
  }
  xyz <- as.matrix(nodes[, c("x", "y", "z")])
  for (i in seq_len(nrow(nodes))) {
    objects[[length(objects) + 1]] <- rayvertex::sphere_mesh(
      position = xyz[i, ], radius = nodes$radius[i],
      material = shiny_material(nodes$color[i], ambient = 0.35))
  }
  for (k in seq_len(nrow(edges))) {
    a <- xyz[edges$from[k], ]
    b <- xyz[edges$to[k], ]
    material <- shiny_material(edges$color[k], ambient = 0.4, specular = 0.3, dissolve = edge.alpha)
    if (edges$directed[k]) {
      along <- unit3(b - a)
      if (edges$reciprocal[k]) {
        # Shift sideways; the opposite edge has the opposite `along`, so it shifts the other way
        side <- cross3(along, c(0, 0, 1))
        if (sum(side^2) < 1e-12) side <- cross3(along, c(0, 1, 0))
        shift <- unit3(side) * edges$radius[k] * 1.6
        a <- a + shift
        b <- b + shift
      }
      # Shaft stops at the arrowhead, which ends at the surface of the target node
      tip <- b - along * nodes$radius[edges$to[k]]
      base <- tip - along * edges$radius[k] * 5
      objects[[length(objects) + 1]] <- rayvertex::segment_mesh(
        start = a, end = base, radius = edges$radius[k], material = material)
      objects[[length(objects) + 1]] <- rayvertex::cone_mesh(
        start = base, end = tip, radius = edges$radius[k] * 2.5, material = material)
    } else {
      objects[[length(objects) + 1]] <- rayvertex::segment_mesh(
        start = a, end = b, radius = edges$radius[k], material = material)
    }
  }
  objects
}

# Render the scene from a fitted camera. Returns an RGBA array (rows from the
# top of the image) with the longest side `res` pixels.
render_scene <- function(objects, camera, res = 1000, quality = 2, bg.color = "white") {
  aspect <- camera$height / camera$width
  width <- if (aspect <= 1) res else round(res / aspect)
  height <- if (aspect <= 1) round(res * aspect) else res
  scene <- rayvertex::scene_from_list(objects)
  rasterize <- function(background) {
    img <- rayvertex::rasterize_scene(
      scene, plot = FALSE, width = width, height = height,
      fov = 0, ortho_dimensions = c(camera$width, camera$height),
      lookat = camera$lookat, lookfrom = camera$lookfrom, camera_up = camera$up,
      near_plane = 0.1, far_plane = camera$far, fsaa = quality,
      light_info = camera_lights(camera), shadow_map = FALSE, background = background)
    array(pmin(pmax(unclass(img), 0), 1), dim = dim(img))
  }
  if (is.na(bg.color)) {
    # rayvertex's own transparent background drops translucent surfaces, so
    # recover each pixel's opacity from renders over white and over black:
    # over white = alpha * colour + (1 - alpha), over black = alpha * colour.
    on_white <- rasterize("white")
    on_black <- rasterize("black")
    alpha <- 1 - (on_white[, , 1] + on_white[, , 2] + on_white[, , 3] -
                    on_black[, , 1] - on_black[, , 2] - on_black[, , 3]) / 3
    alpha <- pmin(pmax(alpha, 0), 1)
    img <- on_black
    for (ch in 1:3) img[, , ch] <- ifelse(alpha > 0, pmin(on_black[, , ch] / alpha, 1), 0)
    img[, , 4] <- alpha
  } else {
    img <- rasterize(bg.color)
  }
  if (camera$mirror) img <- img[, rev(seq_len(dim(img)[2])), , drop = FALSE]
  img
}
