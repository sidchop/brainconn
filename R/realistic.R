# The experimental "realistic" style of brainconn(): a path-traced, wet-looking
# brain with blood vessels on its surface, and the network drawn as glowing
# nodes and edges seen through the brain, like an x-ray. The brain is path
# traced with rayrender (the renderer behind rayshader's high-quality renders);
# the network is rendered with rayvertex and laid over it.
#
# The vessels are illustrative, not anatomical. Each vessel system is a tree of
# shortest paths over the surface, from a few root points to many random tips:
# veins drain to the dorsal midline (into the superior sagittal sinus), and
# arteries spread from roots near the Sylvian fissure, the medial frontal lobe
# and the posterior-inferior surface. Sizes are given in mm for a human brain
# and scale with the mesh, like node and edge sizes.

# Unit normals per vertex (area-weighted face normals), pointing out of the
# brain.
vertex_normals <- function(vertices, faces) {
  a <- vertices[faces[, 1], , drop = FALSE]
  ab <- vertices[faces[, 2], , drop = FALSE] - a
  ac <- vertices[faces[, 3], , drop = FALSE] - a
  n <- cbind(ab[, 2] * ac[, 3] - ab[, 3] * ac[, 2],
             ab[, 3] * ac[, 1] - ab[, 1] * ac[, 3],
             ab[, 1] * ac[, 2] - ab[, 2] * ac[, 1])
  sums <- rowsum(rbind(n, n, n), c(faces[, 1], faces[, 2], faces[, 3]))
  out <- matrix(0, nrow(vertices), 3)
  out[as.integer(rownames(sums)), ] <- sums
  out <- out / pmax(sqrt(rowSums(out^2)), 1e-12)
  # Faces wound the other way give inward normals; most normals point away from the centre
  if (mean(rowSums(out * sweep(vertices, 2, colMeans(vertices)))) < 0) out <- -out
  out
}

# Each edge of a triangle mesh once, as a two-column matrix of vertex indices.
mesh_edges <- function(faces) {
  e <- rbind(faces[, 1:2], faces[, 2:3], faces[, c(3, 1)])
  e <- cbind(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2]))
  e[!duplicated(e), , drop = FALSE]
}

# Laplacian smoothing of per-vertex values (one row per vertex): each step
# moves every value halfway towards the mean of its neighbours.
smooth_vertex_values <- function(values, edges, iterations) {
  values <- as.matrix(values)
  n <- nrow(values)
  from <- c(edges[, 1], edges[, 2])
  to <- c(edges[, 2], edges[, 1])
  degree <- pmax(tabulate(from, n), 1)
  for (k in seq_len(iterations)) {
    sums <- rowsum(values[to, , drop = FALSE], from)
    mean_nb <- matrix(0, n, ncol(values))
    mean_nb[as.integer(rownames(sums)), ] <- sums
    values <- 0.5 * values + 0.5 * mean_nb / degree
  }
  values
}

# Maximum of a per-vertex value over its neighbours, `iterations` edges away.
dilate_vertex_values <- function(values, edges, iterations) {
  from <- c(edges[, 1], edges[, 2])
  to <- c(edges[, 2], edges[, 1])
  for (k in seq_len(iterations)) {
    nb <- values[to]
    o <- order(from, -nb)
    first <- o[!duplicated(from[o])]
    values[from[first]] <- pmax(values[from[first]], nb[first])
  }
  values
}

# Run code with a fixed random seed, leaving the user's random numbers alone.
with_seed <- function(seed, code) {
  old <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
  on.exit(if (is.null(old)) {
    rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", old, envir = globalenv())
  })
  set.seed(seed)
  code
}

# A tree of vessels from `roots` (vertex indices) to `tips`: the union of
# shortest paths over the mesh. Path costs vary smoothly over the surface, so
# vessels meander, and are higher in sulci. The paths are found a few times,
# each time making the edges already used cheaper, so that vessels join into
# trunks rather than running side by side.
vessel_tree <- function(geometry, roots, tips, noise = 1, sulcus.cost = 0.15, rounds = 3,
                        gamma = 0.6) {
  edges <- geometry$edges
  n <- nrow(geometry$vertices)
  field <- smooth_vertex_values(stats::rnorm(n), edges, geometry$iterations$noise)[, 1]
  field <- (field - mean(field)) / max(stats::sd(field), 1e-12)
  vertex_cost <- exp(noise * 0.5 * field) * (1 + sulcus.cost * pmax(-geometry$sulc, 0))
  base <- geometry$edge_length * (vertex_cost[edges[, 1]] + vertex_cost[edges[, 2]]) / 2
  root <- n + 1
  g <- igraph::graph_from_edgelist(rbind(edges, cbind(root, roots)), directed = FALSE)
  weight <- base
  for (r in seq_len(rounds)) {
    sp <- suppressWarnings(igraph::shortest_paths(
      g, from = root, to = tips, output = "both", algorithm = "dijkstra",
      weights = c(weight, rep(1e-9, length(roots)))))
    used <- tabulate(unlist(lapply(sp$epath, as.integer)), igraph::ecount(g))[seq_len(nrow(edges))]
    weight <- base / (1 + used)^gamma
  }
  paths <- lapply(sp$vpath, as.integer)
  paths <- paths[lengths(paths) > 2]
  flow <- tabulate(unlist(paths), root)
  list(paths = paths, flow = flow, root = root)
}

# Split a vessel tree into chains of vertices, each running from a tip towards
# the root until it joins a chain found earlier (longest paths first, so the
# trunks are single chains).
tree_chains <- function(tree) {
  paths <- tree$paths[order(-lengths(tree$paths))]
  used <- logical(tree$root)
  chains <- list()
  for (p in paths) {
    p <- rev(p[p != tree$root])
    joins <- which(used[p])[1]
    if (!is.na(joins)) {
      if (joins == 1) next
      p <- p[seq_len(joins)]
    }
    used[p] <- TRUE
    chains[[length(chains) + 1]] <- p
  }
  chains
}

# One mesh of tubes along polylines: `paths` is a list of m x 3 matrices and
# `radii` a list of m radii. Returns vertices, faces (1-based) and normals.
tube_mesh <- function(paths, radii, sides = 8) {
  angle <- 2 * pi * (seq_len(sides) - 1) / sides
  vertices <- normals <- faces <- vector("list", length(paths))
  offset <- 0
  for (k in seq_along(paths)) {
    p <- paths[[k]]
    r <- radii[[k]]
    keep <- c(TRUE, rowSums(diff(p)^2) > 1e-12)
    p <- p[keep, , drop = FALSE]
    r <- r[keep]
    m <- nrow(p)
    if (m < 2) next
    tangent <- p[c(2:m, m), , drop = FALSE] - p[c(1, 1:(m - 1)), , drop = FALSE]
    tangent <- tangent / sqrt(rowSums(tangent^2))
    # Rings are oriented by parallel transport, so tubes don't twist
    u <- cross3(tangent[1, ], if (abs(tangent[1, 3]) < 0.9) c(0, 0, 1) else c(1, 0, 0))
    side1 <- side2 <- matrix(0, m, 3)
    for (i in seq_len(m)) {
      u <- unit3(u - sum(u * tangent[i, ]) * tangent[i, ])
      side1[i, ] <- u
      side2[i, ] <- cross3(tangent[i, ], u)
    }
    ring <- normal <- matrix(0, m * sides, 3)
    for (s in seq_len(sides)) {
      rows <- (seq_len(m) - 1) * sides + s
      normal[rows, ] <- cos(angle[s]) * side1 + sin(angle[s]) * side2
      ring[rows, ] <- p + r * normal[rows, ]
    }
    start <- rep((seq_len(m - 1) - 1) * sides, each = sides)
    s0 <- rep(seq_len(sides) - 1, m - 1)
    s1 <- (s0 + 1) %% sides
    a <- start + s0
    b <- start + s1
    faces[[k]] <- rbind(cbind(a, b, a + sides), cbind(b, b + sides, a + sides)) + offset + 1L
    vertices[[k]] <- ring
    normals[[k]] <- normal
    offset <- offset + nrow(ring)
  }
  list(vertices = do.call(rbind, vertices), faces = do.call(rbind, faces),
       normals = do.call(rbind, normals))
}

# Tubes for a vessel tree. Radii grow with the number of tips a vessel drains,
# tips taper, and vessels lie on the envelope of the brain (bridging sulci).
vessel_tubes <- function(geometry, tree, radius.range, power = 0.6, min.length = 6) {
  chains <- tree_chains(tree)
  chains <- chains[lengths(chains) >= min.length]
  if (length(chains) == 0) return(NULL)
  max_flow <- max(tree$flow[seq_len(nrow(geometry$vertices))])
  env <- geometry$envelope
  paths <- radii <- vector("list", length(chains))
  for (k in seq_along(chains)) {
    p <- chains[[k]]
    r <- radius.range[1] + diff(radius.range) * (tree$flow[p] / max_flow)^power
    if (tree$flow[p[1]] <= 1) {
      taper <- min(length(p), 6)
      r[seq_len(taper)] <- r[seq_len(taper)] * seq(0.25, 1, length.out = taper)
    }
    xyz <- env$vertices[p, , drop = FALSE] + env$normals[p, , drop = FALSE] * (env$height[p] + 0.9 * r)
    # Smooth the path, keeping its ends
    m <- nrow(xyz)
    if (m > 2) {
      for (it in 1:3) xyz[2:(m - 1), ] <- (xyz[1:(m - 2), ] + 2 * xyz[2:(m - 1), ] + xyz[3:m, ]) / 4
    }
    paths[[k]] <- xyz
    radii[[k]] <- r
  }
  tube_mesh(paths, radii)
}

# Surface measures used by the realistic style. `unit` is the size of a
# millimetre in a human-sized brain (see brainconn()).
brain_geometry <- function(mesh, unit) {
  vertices <- mesh$vertices
  faces <- mesh$faces
  edges <- mesh_edges(faces)
  edge_length <- sqrt(rowSums((vertices[edges[, 1], , drop = FALSE] -
                                 vertices[edges[, 2], , drop = FALSE])^2))
  # Smoothing steps reach about as far (in mm) on every mesh, whatever its resolution
  steps <- function(mm) {
    max(2L, min(200L, as.integer(round((mm * unit / mean(edge_length))^2))))
  }
  iterations <- list(envelope = steps(5.3), sulc = steps(9.5), noise = steps(7.5),
                     patches = steps(9.5))
  normals <- vertex_normals(vertices, faces)
  # Envelope: a smoothed surface, raised to the height of the nearby gyral crowns
  env_vertices <- smooth_vertex_values(vertices, edges, iterations$envelope)
  env_normals <- vertex_normals(env_vertices, faces)
  height <- rowSums((vertices - env_vertices) * env_normals)
  crowns <- dilate_vertex_values(height, edges, max(2L, as.integer(round(7.5 * unit / mean(edge_length)))))
  crowns <- smooth_vertex_values(pmax(crowns, height), edges, 4)[, 1]
  # Sulcal depth: height above a more smoothed surface (> 0 on gyri, < 0 in sulci), in mm
  smoother <- smooth_vertex_values(env_vertices, edges, iterations$sulc - iterations$envelope)
  sulc <- rowSums((vertices - smoother) * vertex_normals(smoother, faces)) / unit
  list(vertices = vertices, faces = faces, normals = normals, edges = edges,
       edge_length = edge_length, sulc = sulc, iterations = iterations,
       envelope = list(vertices = env_vertices, normals = env_normals,
                       height = pmax(crowns, height)))
}

# Vertices of `geometry` nearest to points given relative to the mesh's
# bounding box (-1 to 1 on each axis).
nearest_vertices <- function(geometry, relative) {
  v <- geometry$vertices
  centre <- colMeans(apply(v, 2, range))
  half <- apply(v, 2, function(x) diff(range(x))) / 2
  apply(relative, 1, function(p) which.min(colSums((t(v) - (centre + p * half))^2)))
}

# Vein and artery tube meshes for a brain.
brain_vessels <- function(geometry, unit) {
  v <- geometry$vertices
  n <- nrow(v)
  centre <- colMeans(apply(v, 2, range))
  half <- apply(v, 2, function(x) diff(range(x))) / 2
  rel <- sweep(sweep(v, 2, centre), 2, half, "/")
  # Veins drain to the dorsal midline, from tips on the lateral surfaces
  sinus <- which(abs(rel[, 1]) < 0.09 & rel[, 3] > 0.15 + 0.25 * abs(rel[, 2]))
  if (length(sinus) < 3) sinus <- order(abs(rel[, 1]) - rel[, 3])[seq_len(min(n, 30))]
  lateral <- which(geometry$normals[, 1] * sign(rel[, 1]) > -0.2 & abs(rel[, 1]) > 0.29)
  if (length(lateral) < 10) lateral <- seq_len(n)
  scale <- min(1, n / 80000)
  veins <- vessel_tree(geometry, sinus, sample(lateral, min(length(lateral), round(350 * scale))))
  # Arteries: middle, anterior and posterior cerebral arteries, both sides
  roots <- nearest_vertices(geometry, rbind(c(-0.58, 0.34, -0.46), c(0.58, 0.34, -0.46),
                                            c(-0.06, 0.55, -0.22), c(0.06, 0.55, -0.22),
                                            c(-0.29, -0.26, -0.46), c(0.29, -0.26, -0.46)))
  arteries <- vessel_tree(geometry, unique(roots), sample(n, min(n, round(3000 * scale))),
                          noise = 1.2, sulcus.cost = 0.1, gamma = 0.4)
  list(veins = vessel_tubes(geometry, veins, c(0.3, 2.3) * unit),
       arteries = vessel_tubes(geometry, arteries, c(0.12, 1.6) * unit, min.length = 3),
       sinus = sinus_tube(v[sinus, , drop = FALSE], centre[1], unit))
}

# The superior sagittal sinus: a vein along the top of the midline, from the
# front of the brain to the back, widening towards the back. The veins drain
# into it.
sinus_tube <- function(points, midline, unit, bins = 40) {
  if (nrow(points) < 3) return(NULL)
  y <- points[, 2]
  breaks <- seq(min(y), max(y), length.out = bins + 1)
  bin <- cut(y, breaks, include.lowest = TRUE, labels = FALSE)
  top <- tapply(points[, 3], bin, max)
  at <- (breaks[-1] + breaks[-(bins + 1)])[as.integer(names(top))] / 2
  if (length(top) < 3) return(NULL)
  # Front to back, with the height smoothed
  o <- order(-at)
  z <- stats::filter(as.numeric(top[o]), rep(1 / 5, 5), sides = 2)
  z[is.na(z)] <- as.numeric(top[o])[is.na(z)]
  r <- seq(0.8, 3.2, length.out = length(o)) * unit
  path <- cbind(midline, at[o], z + 0.6 * r)
  tube_mesh(list(path), list(r), sides = 12)
}

# Everything the realistic style needs for a brain surface: the mesh, its
# normals, texture coordinates and blood vessels. The last brain is cached, as
# making the vessels takes a few seconds.
realistic_parts <- function(mesh, unit) {
  key <- paste(nrow(mesh$vertices), nrow(mesh$faces), signif(sum(mesh$vertices), 10),
               signif(sum(mesh$vertices[, 1] * seq_len(nrow(mesh$vertices))), 10), unit)
  cached <- brainconn_env$realistic
  if (!is.null(cached) && identical(cached$key, key)) return(cached$parts)
  with_seed(1, {
    geometry <- brain_geometry(mesh, unit)
    patches <- smooth_vertex_values(stats::rnorm(nrow(mesh$vertices)), geometry$edges,
                                    geometry$iterations$patches)[, 1]
    vessels <- brain_vessels(geometry, unit)
    # Small random tilts of the surface normals, so the surface looks slightly
    # uneven and its highlights break up, as on real tissue
    bumps <- smooth_vertex_values(matrix(stats::rnorm(3 * nrow(mesh$vertices)), ncol = 3),
                                  geometry$edges, 2)
  })
  bumps <- bumps / max(stats::sd(bumps), 1e-12)
  patches <- (patches - mean(patches)) / max(stats::sd(patches), 1e-12)
  # Texture coordinates: sulcal depth across, colour patches down (see tissue_texture())
  uv <- cbind(pmin(pmax((geometry$sulc + 3) / 6, 0.02), 0.98),
              pmin(pmax(0.5 + 0.3 * patches, 0.02), 0.98))
  parts <- list(mesh = mesh, normals = geometry$normals, uv = uv, vessels = vessels,
                bumps = bumps, edges = geometry$edges, envelope = geometry$envelope)
  brainconn_env$realistic <- list(key = key, parts = parts)
  parts
}

# A texture for the brain surface, sampled by sulcal depth (across) and by
# slowly varying colour patches (down): deep red in sulci, pale pink on gyri.
tissue_texture <- function() {
  file <- file.path(tempdir(), "brainconn-tissue.png")
  if (file.exists(file)) return(file)
  pink <- grDevices::col2rgb(grDevices::colorRampPalette(
    c("#93322d", "#c75d55", "#e19a8e", "#f1c4b8"))(256)) / 255
  rose <- grDevices::col2rgb(grDevices::colorRampPalette(
    c("#8d3a3a", "#c06a66", "#dda59d", "#efcbbf"))(256)) / 255
  img <- array(0, c(64, 256, 3))
  for (row in 1:64) {
    t <- (row - 1) / 63
    for (ch in 1:3) img[row, , ch] <- (1 - t) * pink[ch, ] + t * rose[ch, ]
  }
  OpenImageR::writeImage(img, file)
  file
}

# The realistic style is path traced with rayrender (a suggested package).
check_rayrender <- function() {
  if (!requireNamespace("rayrender", quietly = TRUE)) {
    stop("style = \"realistic\" needs the rayrender package: install.packages(\"rayrender\"). ",
         "On macOS, rayrender also needs XQuartz (https://www.xquartz.org).", call. = FALSE)
  }
}

# An rgl-style mesh3d object, which rayrender reads.
as_mesh3d <- function(vertices, faces, normals = NULL, texcoords = NULL) {
  m <- list(vb = rbind(t(vertices), 1), it = t(faces), primitivetype = "triangle",
            material = list())
  if (!is.null(normals)) m$normals <- t(normals)
  if (!is.null(texcoords)) m$texcoords <- t(texcoords)
  structure(m, class = c("mesh3d", "shape3d"))
}

# The brain, its vessels and studio lights as a rayrender scene. The surface
# is glossy (it looks wet); the lights are large, soft rectangles (like a
# photographer's softboxes) that follow the camera: a key light above and to
# the right, a fill light on the left and a rim light behind the brain.
pathtraced_scene <- function(parts, camera, unit, vertices = parts$mesh$vertices,
                             normals = parts$normals, vessels = parts$vessels, spheres = NULL,
                             extra = NULL) {
  rr <- function(name) getExportedValue("rayrender", name)
  tube <- function(m, color) {
    rr("mesh3d_model")(as_mesh3d(m$vertices, m$faces, m$normals), override_material = TRUE,
                       material = rr("glossy")(color = color, gloss = 0.92, reflectance = 0.05))
  }
  normals <- normals + 0.12 * parts$bumps
  normals <- normals / sqrt(rowSums(normals^2))
  scene <- rr("mesh3d_model")(
    as_mesh3d(vertices, parts$mesh$faces, normals, parts$uv),
    override_material = TRUE,
    material = rr("glossy")(image_texture = tissue_texture(), gloss = 0.85, reflectance = 0.04))
  tubes <- list(list(vessels$veins, "#0e0a22"), list(vessels$sinus, "#0e0a22"),
                list(vessels$arteries, "#8e1510"))
  for (v in tubes) {
    if (!is.null(v[[1]])) scene <- rr("add_object")(scene, tube(v[[1]], v[[2]]))
  }
  # Extra shiny spheres (x, y, z, radius, color), e.g. nodes outside the brain
  for (i in seq_len(if (is.null(spheres)) 0 else nrow(spheres))) {
    scene <- rr("add_object")(scene, rr("sphere")(
      x = spheres$x[i], y = spheres$y[i], z = spheres$z[i], radius = spheres$radius[i],
      material = rr("glossy")(color = spheres$color[i], gloss = 0.9, reflectance = 0.06)))
  }
  # Anything else, e.g. a floor
  if (!is.null(extra)) scene <- rr("add_object")(scene, extra)
  softbox <- function(direction, width, height, intensity) {
    d <- unit3(direction)
    p <- camera$lookat + 400 * unit * d
    # Turn the rectangle (in the xy plane) to face the brain
    angle <- c(asin(-d[2]), atan2(d[1], d[3])) * 180 / pi
    rr("xy_rect")(x = p[1], y = p[2], z = p[3], xwidth = width * unit, ywidth = height * unit,
                  angle = c(angle, 0), flipped = TRUE,
                  material = rr("light")(intensity = intensity))
  }
  scene <- rr("add_object")(scene, softbox(camera$dir + 0.8 * camera$up + 0.5 * camera$right,
                                           220, 140, 7))
  scene <- rr("add_object")(scene, softbox(camera$dir - 0.9 * camera$right + 0.1 * camera$up,
                                           200, 200, 2.5))
  rr("add_object")(scene, softbox(-camera$dir + 0.9 * camera$up, 160, 160, 5))
}

# Path trace the brain from a fitted camera, on a transparent background.
# Returns an RGBA array like render_scene(), with the longest side `res`
# pixels; `samples` rays per pixel (more is smoother and slower). Next event
# estimation (sampling the lights directly) keeps the noise low, and what is
# left is smoothed by denoise_render().
render_pathtraced <- function(parts, camera, res, samples, unit, denoise = 2, ...) {
  aspect <- camera$height / camera$width
  width <- if (aspect <= 1) res else round(res / aspect)
  height <- if (aspect <= 1) round(res * aspect) else res
  scene <- pathtraced_scene(parts, camera, unit, ...)
  render <- function(samples, debug = "none") {
    img <- rayrender::render_scene(
      scene, width = width, height = height, samples = samples, fov = 0,
      ortho_dimensions = c(camera$width, camera$height), lookfrom = camera$lookfrom,
      lookat = camera$lookat, camera_up = camera$up, aperture = 0,
      tonemap = if (debug == "none") "hbd" else "raw", clamp_value = 10,
      integrator_type = "nee", transparent_background = TRUE, debug_channel = debug,
      plot_scene = FALSE, preview = FALSE, interactive = FALSE, progress = FALSE,
      verbose = FALSE)
    img <- unclass(img)
    if (length(dim(img)) == 2) img <- array(img, c(dim(img), 1))
    img
  }
  img <- render(samples)
  # Guides for the denoiser: surface normals, colours (albedo) and depth
  img[, , 1:3] <- denoise_render(img[, , 1:3, drop = FALSE], img[, , 4],
                                 normals = render(4, "normals")[, , 1:3, drop = FALSE],
                                 albedo = render(4, "color")[, , 1:3, drop = FALSE],
                                 depth = render(4, "depth")[, , 1], iterations = denoise)
  img <- array(pmin(pmax(img, 0), 1), dim = dim(img))
  if (camera$mirror) img <- img[, rev(seq_len(dim(img)[2])), , drop = FALSE]
  img
}

# Shift a matrix by (dy, dx) pixels, repeating the edge pixels.
shift_matrix <- function(m, dy, dx) {
  m[pmin(pmax(seq_len(nrow(m)) + dy, 1), nrow(m)), pmin(pmax(seq_len(ncol(m)) + dx, 1), ncol(m)),
    drop = FALSE]
}

# Smooth the remaining path-tracing noise with an edge-avoiding a-trous
# wavelet filter: each pixel is averaged with nearby pixels (further apart
# each iteration) that have a similar normal, colour (albedo), depth and
# rendered colour. So edges, the vessels and the shiny highlights stay sharp
# while the grain on smooth surfaces is averaged away.
denoise_render <- function(img, alpha, normals, albedo, depth, iterations = 2,
                           sigma.normal = 0.1, sigma.albedo = 0.003, sigma.depth = 1.5,
                           sigma.color = 0.06) {
  kernel <- c(1, 4, 6, 4, 1) / 16
  depth[!is.finite(depth)] <- 0
  for (it in seq_len(iterations) - 1) {
    step <- 2^it
    total <- array(0, dim(img))
    weights <- matrix(0, nrow(alpha), ncol(alpha))
    for (a in -2:2) {
      for (b in -2:2) {
        moved <- function(x) {
          for (ch in seq_len(dim(x)[3])) x[, , ch] <- shift_matrix(x[, , ch], a * step, b * step)
          x
        }
        near <- moved(img)
        d_normal <- rowSums((moved(normals) - normals)^2, dims = 2)
        d_albedo <- rowSums((moved(albedo) - albedo)^2, dims = 2)
        d_color <- rowSums((near - img)^2, dims = 2)
        d_depth <- abs(shift_matrix(depth, a * step, b * step) - depth)
        w <- kernel[a + 3] * kernel[b + 3] * exp(-d_normal / sigma.normal) *
          exp(-d_albedo / sigma.albedo) * exp(-d_color / (sigma.color^2 * step)) *
          exp(-d_depth / (sigma.depth * step)) * shift_matrix(alpha, a * step, b * step)
        for (ch in 1:3) total[, , ch] <- total[, , ch] + w * near[, , ch]
        weights <- weights + w
      }
    }
    for (ch in 1:3) {
      img[, , ch] <- ifelse(weights > 0, total[, , ch] / pmax(weights, 1e-12), img[, , ch])
    }
  }
  img
}

# Gaussian blur of a matrix (sigma in pixels), by FFT, with zero padding.
blur_image <- function(m, sigma) {
  pad <- ceiling(3 * sigma)
  h <- nrow(m) + 2 * pad
  w <- ncol(m) + 2 * pad
  z <- matrix(0, h, w)
  z[pad + seq_len(nrow(m)), pad + seq_len(ncol(m))] <- m
  kernel <- function(len) {
    d <- pmin(0:(len - 1), len - 0:(len - 1))
    k <- exp(-0.5 * (d / sigma)^2)
    k / sum(k)
  }
  out <- Re(stats::fft(stats::fft(z) * stats::fft(outer(kernel(h), kernel(w))), inverse = TRUE)) /
    (h * w)
  out[pad + seq_len(nrow(m)), pad + seq_len(ncol(m))]
}

# Darken colours by a factor between 0 and 1.
shade_colors <- function(colors, factor) {
  rgb <- grDevices::col2rgb(colors) / 255
  grDevices::rgb(t(rgb * rep(factor, each = 3)))
}

# The network as an image (RGBA) to lay over the brain. Parts of the network
# further from the camera are darker.
network_layer <- function(nodes, edges, edge.alpha, camera, res, quality, depth_points) {
  depth_of <- function(xyz) as.vector(sweep(xyz, 2, camera$lookat) %*% camera$dir)
  depth_range <- range(depth_of(depth_points))
  nearness <- function(xyz) {
    0.35 + 0.65 * pmin(pmax((depth_of(xyz) - depth_range[1]) / max(diff(depth_range), 1e-9), 0), 1)
  }
  node_near <- nearness(as.matrix(nodes[, c("x", "y", "z")]))
  nodes$color <- shade_colors(nodes$color, node_near)
  if (nrow(edges) > 0) {
    edges$color <- shade_colors(edges$color, (node_near[edges$from] + node_near[edges$to]) / 2)
  }
  render_scene(scene_objects(NULL, NULL, NULL, nodes, edges, edge.alpha), camera,
               res = res, quality = quality, bg.color = NA)
}

# Lay the network over the brain image, mostly opaque (it is inside the brain,
# so this shows it as if seen through the tissue), with a glow around it.
# `fade` (0 to 1) fades the network in. Works with premultiplied colours, so
# it also works on a transparent background. Returns an RGBA array.
composite_network <- function(img, network, res, fade = 1) {
  ghost <- 0.9 * fade
  glow_strength <- 1.5 * fade
  sigma <- max(1, res / 140)
  a_brain <- img[, , 4]
  a_net <- network[, , 4] * ghost
  alpha <- a_brain * (1 - a_net) + a_net
  glow_alpha <- 0
  out <- img
  for (ch in 1:3) {
    colour <- img[, , ch] * a_brain * (1 - a_net) + network[, , ch] * a_net
    glow <- pmin(pmax(glow_strength * blur_image(network[, , ch] * network[, , 4], sigma), 0), 1)
    out[, , ch] <- colour + glow - colour * glow
    glow_alpha <- pmax(glow_alpha, glow)
  }
  alpha <- alpha + glow_alpha * (1 - alpha)
  for (ch in 1:3) out[, , ch] <- ifelse(alpha > 0, pmin(out[, , ch] / pmax(alpha, 1e-6), 1), 0)
  out[, , 4] <- alpha
  pmin(pmax(out, 0), 1)
}

# Lay an RGBA image over a background colour (NA keeps it transparent).
over_background <- function(img, bg.color) {
  if (is.na(bg.color)) return(img)
  background <- grDevices::col2rgb(bg.color)[, 1] / 255
  for (ch in 1:3) img[, , ch] <- img[, , ch] * img[, , 4] + background[ch] * (1 - img[, , 4])
  img[, , 4] <- 1
  img
}

# Render one view in the realistic style: the path-traced brain with the
# network laid over it. Returns an RGBA array, like render_scene().
render_realistic <- function(parts, nodes, edges, edge.alpha, camera, res, quality, bg.color,
                             unit, depth_points) {
  network <- network_layer(nodes, edges, edge.alpha, camera, res, quality, depth_points)
  if (is.null(parts)) {
    img <- network
    img[] <- 0
  } else {
    img <- render_pathtraced(parts, camera, res, samples = 32 * quality, unit)
  }
  composite_network(over_background(img, bg.color), network, res)
}
