#' Animate nodes bouncing off a squishy brain (experimental)
#'
#' \code{brainconn_bounce} makes a GIF in the realistic style of
#' \code{\link{brainconn}}, in a white photo studio: a ball for every region
#' of the atlas drops onto the brain and bounces off it, and the brain squishes
#' and jiggles like jelly where they land. The regions in the network (those
#' with connections in \code{conmat}) stick where they land; the others roll
#' down the brain, or bounce off it, onto the floor and away. The connections
#' of the network then grow between the balls that stuck. It's just for fun,
#' e.g. for the first slide of a talk.
#'
#' Each ball lands on the top of the brain surface above its region. Every
#' frame is path traced (see \code{\link{brainconn}}), so making a GIF takes
#' several minutes. This needs the rayrender and gifski packages.
#'
#' @inheritParams brainconn
#' @param conmat An adjacency matrix, as in \code{\link{brainconn}}. Its regions with connections stick to the brain. If \code{NULL}, all regions stick.
#' @param file The GIF file to write.
#' @param view The camera angle, as in \code{\link{brainconn}}. Views from above (elevation above 0) work best, so the balls fall down the image.
#' @param node.size Size of the balls (diameter in mm for a human-sized brain).
#' @param edge.color A colour for the connections. Ignored when \code{edge.color.weighted = TRUE}.
#' @param edge.width Width of the connections (relative; 1 is a tube about 1 mm wide in a human-sized brain).
#' @param mesh The brain surface, as in \code{\link{brainconn}} (but not \code{NULL}).
#' @param squish How squishy the brain is: 0 for solid, 1 (the default) for jelly, and more for wobblier.
#' @param fps Frames per second.
#' @param res Size in pixels of the longest side of the GIF.
#' @param quality Rays per pixel when path tracing, in multiples of 32. Lower is faster and grainier.
#' @param seed Random seed for when each ball drops.
#'
#' @return The path of the GIF, invisibly.
#' @seealso \code{\link{brainconn}}, whose \code{style = "realistic"} this uses.
#' @examples
#' \dontrun{
#' brainconn_bounce("schaefer300_n7", example_unweighted_undirected, file = "bounce.gif")
#' }
#' @export
brainconn_bounce <- function(atlas,
                             conmat = NULL,
                             file = "brainconn-bounce.gif",
                             view = c(-125, 25),
                             node.size = 6,
                             node.color = "network",
                             edge.color = "#f2b632",
                             edge.width = 2,
                             edge.color.weighted = FALSE,
                             edge.palette = NULL,
                             edge.limits = NULL,
                             scale.edge.width = NULL,
                             thr = NULL,
                             uthr = NULL,
                             mesh = "ICBM152",
                             squish = 1,
                             fps = 15,
                             res = 600,
                             quality = 2,
                             seed = 1) {
  check_rayrender()
  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop("brainconn_bounce() needs the gifski package: install.packages(\"gifski\").",
         call. = FALSE)
  }
  if (is.null(mesh)) stop("brainconn_bounce() needs a brain surface (`mesh`).", call. = FALSE)
  if (is.character(view)) view <- match.arg(view, names(camera_views))

  # Every region drops; those with connections stick
  prepared <- prepare_network(resolve_atlas(atlas, parent.frame()), conmat, thr, uthr,
                              all.nodes = TRUE, node.size, node.color, edge.color, edge.width,
                              edge.color.weighted, edge.palette, edge.limits, scale.edge.width,
                              mesh)
  nodes <- prepared$nodes
  edges <- prepared$edges$table
  stick <- if (nrow(edges) == 0) rep(TRUE, nrow(nodes)) else
    seq_len(nrow(nodes)) %in% c(edges$from, edges$to)
  unit <- prepared$unit
  parts <- realistic_parts(prepared$mesh, unit)
  vertices <- parts$mesh$vertices
  plan <- with_seed(seed, bounce_plan(parts, nodes, stick))

  # The camera frames the brain, some room above it, where the balls fall
  # from, and a little of the floor around it
  top <- vertices[vertices[, 3] > stats::quantile(vertices[, 3], 0.8), , drop = FALSE]
  top[, 3] <- top[, 3] + 0.3 * plan$height
  floor <- sweep(sweep(vertices, 2, plan$middle) * c(1.15, 1.15, 0), 2,
                 c(plan$middle[1:2], plan$floor), "+")
  camera <- fit_camera(view_camera(view), rbind(vertices, top, floor))

  times <- seq(0, plan$end, by = 1 / fps)
  dir <- tempfile("brainconn-bounce")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  files <- file.path(dir, sprintf("frame%04d.png", seq_along(times)))
  progress <- if (interactive()) utils::txtProgressBar(max = length(times), style = 3)
  for (k in seq_along(times)) {
    t <- times[k]
    if (k > 1 && t > plan$still) {
      # Nothing moves any more: reuse the last frame
      file.copy(files[k - 1], files[k])
    } else {
      img <- bounce_frame(parts, plan, nodes, edges, camera, t, squish, res, quality, unit)
      OpenImageR::writeImage(img[, , 1:3], files[k])
    }
    if (!is.null(progress)) utils::setTxtProgressBar(progress, k)
  }
  if (!is.null(progress)) close(progress)
  size <- dim(OpenImageR::readImage(files[1]))
  gifski::gifski(files, gif_file = file, width = size[2], height = size[1], delay = 1 / fps,
                 progress = FALSE)
  invisible(file)
}

# When and where each ball lands, bounces, rolls and leaves. Times are in
# seconds. Each ball drops from `drop` above its landing point (out of the
# image) after a random delay, lands `fall` seconds later, and bounces, each
# bounce keeping `restitution` of the speed, until it comes to rest. Balls
# that `stick` stay there. The others leave: they roll downhill over the
# envelope of the brain (which bridges the sulci), speeding up, and fall off
# where the surface turns under, or, where they can't roll off, hop off. They
# then bounce on the floor and roll away out of the image.
bounce_plan <- function(parts, nodes, stick, fall = 0.7, spread = 2.5, restitution = 0.5) {
  v <- parts$mesh$vertices
  normals <- parts$normals
  n <- nrow(nodes)
  height <- diff(range(v[, 3]))
  extent <- max(apply(v, 2, function(x) diff(range(x))))
  middle <- colMeans(apply(v, 2, range))
  floor <- min(v[, 3])
  drop <- 1.1 * height
  g <- 2 * drop / fall^2
  speed <- g * fall
  # Landing points: the top of the brain surface above each region
  up <- which(normals[, 3] > 0.2)
  radius <- 0.025 * extent
  land <- vapply(seq_len(n), function(i) {
    d2 <- (v[up, 1] - nodes$x[i])^2 + (v[up, 2] - nodes$y[i])^2
    near <- up[d2 < radius^2]
    if (length(near) == 0) near <- up[which.min(d2)]
    near[which.max(v[near, 3])]
  }, integer(1))
  delay <- stats::runif(n, 0, spread)
  impacts <- list()
  rest <- numeric(n)
  for (i in seq_len(n)) {
    t <- delay[i] + fall
    s <- speed
    repeat {
      impacts[[length(impacts) + 1]] <- c(node = i, time = t, speed = s)
      s <- restitution * s
      if (s < 0.1 * speed) break
      t <- t + 2 * s / g
    }
    rest[i] <- t
  }
  impacts <- as.data.frame(do.call(rbind, impacts))
  centre <- v[land, , drop = FALSE] + normals[land, , drop = FALSE] * nodes$radius

  # Leaving: roll down and outwards over the envelope, from the landing point
  env <- parts$envelope
  surface <- env$vertices + env$normals * env$height
  outwards <- sqrt((surface[, 1] - middle[1])^2 + (surface[, 2] - middle[2])^2)
  downhill <- surface[, 3] - 0.3 * outwards
  e <- parts$edges
  neighbours <- split(c(e[, 2], e[, 1]), factor(c(e[, 1], e[, 2]), levels = seq_len(nrow(v))))
  path <- path_time <- floor_moves <- vector("list", n)
  gone <- rep(Inf, n)
  for (i in which(!stick)) {
    walk <- land[i]
    off <- FALSE
    for (step in 1:2000) {
      here <- walk[length(walk)]
      next_to <- neighbours[[here]]
      lowest <- next_to[which.min(downhill[next_to])]
      if (downhill[lowest] >= downhill[here]) break
      walk <- c(walk, lowest)
      if (env$normals[lowest, 3] < -0.1) {
        off <- TRUE
        break
      }
    }
    if (off && length(walk) >= 4) {
      xyz <- rbind(centre[i, ], surface[walk[-1], , drop = FALSE] +
                     env$normals[walk[-1], , drop = FALSE] * nodes$radius[i])
      m <- nrow(xyz)
      for (it in 1:4) {
        xyz[2:(m - 1), ] <- (xyz[1:(m - 2), ] + 2 * xyz[2:(m - 1), ] + xyz[3:m, ]) / 4
      }
      # A rolling ball speeds up as it goes down (5/7 of g, for a solid sphere)
      v_at <- sqrt((0.05 * speed)^2 + 2 * (5 / 7) * g * pmax(xyz[1, 3] - xyz[, 3], 0))
      seg <- sqrt(rowSums(diff(xyz)^2))
      times <- rest[i] + c(0, cumsum(seg / ((v_at[-1] + v_at[-m]) / 2)))
      direction <- unit3(xyz[m, ] - xyz[m - 1, ])
      velocity <- v_at[m] * direction
    } else {
      # Can't roll off: hop off, outwards and up
      xyz <- centre[i, , drop = FALSE]
      times <- rest[i]
      out <- c(centre[i, 1:2] - middle[1:2], 0)
      out <- if (sum(out^2) > 0) unit3(out) else c(1, 0, 0)
      velocity <- 0.35 * speed * out + c(0, 0, 0.3 * speed)
    }
    path[[i]] <- xyz
    path_time[[i]] <- times
    floor_moves[[i]] <- floor_motion(xyz[nrow(xyz), ], velocity, times[length(times)], g,
                                     floor + nodes$radius[i], middle, 1.5 * extent, speed)
    gone[i] <- floor_moves[[i]]$gone
  }
  settle <- max(c(rest[stick], gone[!stick]), 0) + 0.3
  list(land = land, point = v[land, , drop = FALSE], normal = normals[land, , drop = FALSE],
       centre = centre, stick = stick, delay = delay, fall = fall, drop = drop, g = g,
       speed = speed, restitution = restitution, impacts = impacts, rest = rest,
       height = height, floor = floor, bottom = floor, middle = middle, path = path,
       path_time = path_time, floor_moves = floor_moves, gone = gone,
       grow = c(settle, settle + 1.5), still = settle + 1.5, end = settle + 3)
}

# A ball leaving the brain at `start` (time `t0`) with `velocity`: it falls
# to the floor (where its centre is at height `level`), bounces a few times,
# and rolls away from the brain, until it is `away` from the middle of the
# brain (out of the image).
floor_motion <- function(start, velocity, t0, g, level, middle, away, speed) {
  vz <- velocity[3]
  drop <- max(start[3] - level, 0)
  t_floor <- (vz + sqrt(vz^2 + 2 * g * drop)) / g
  horizontal <- velocity[1:2]
  outwards <- start[1:2] - middle[1:2]
  if (sqrt(sum(horizontal^2)) < 0.25 * speed) {
    # Roll away from the brain at a reasonable speed
    dir <- if (sum(outwards^2) > 0) outwards / sqrt(sum(outwards^2)) else c(1, 0)
    horizontal <- 0.25 * speed * dir
  }
  landing <- start[1:2] + velocity[1:2] * t_floor
  # Solve |landing + horizontal * u - middle| = away for u
  a <- sum(horizontal^2)
  b <- 2 * sum((landing - middle[1:2]) * horizontal)
  c <- sum((landing - middle[1:2])^2) - away^2
  u <- max((-b + sqrt(max(b^2 - 4 * a * c, 0))) / (2 * a), 0)
  list(start = start, velocity = velocity, t0 = t0, t_floor = t_floor, level = level,
       landing = landing, horizontal = horizontal, bounce = 0.4 * (g * t_floor - vz),
       gone = t0 + t_floor + u)
}

# Height of a ball above where it rests, `u` seconds after it first hits a
# surface at speed `s`, bouncing with `restitution` until it stops.
bounce_after <- function(u, s, g, restitution, stop) {
  repeat {
    s <- restitution * s
    if (s < stop) return(0)
    flight <- 2 * s / g
    if (u < flight) return(s * u - 0.5 * g * u^2)
    u <- u - flight
  }
}

# Height of node i above its landing place at time t, while it falls and
# bounces on the brain.
bounce_height <- function(plan, i, t) {
  start <- plan$delay[i]
  if (t < start) return(plan$drop)
  first <- start + plan$fall
  if (t < first) return(plan$drop - 0.5 * plan$g * (t - start)^2)
  bounce_after(t - first, plan$speed, plan$g, plan$restitution, 0.1 * plan$speed)
}

# Centre of ball i at time t, or NULL once it has left the image. `on_brain`
# says whether the ball is still on the brain (and moves as it squishes).
node_position <- function(plan, i, t) {
  if (plan$stick[i] || t < plan$rest[i]) {
    return(list(xyz = plan$centre[i, ] + c(0, 0, bounce_height(plan, i, t)), on_brain = TRUE))
  }
  if (t >= plan$gone[i]) return(NULL)
  times <- plan$path_time[[i]]
  xyz <- plan$path[[i]]
  m <- length(times)
  if (t <= times[m] && m > 1) {
    k <- min(findInterval(t, times, rightmost.closed = TRUE), m - 1)
    f <- (t - times[k]) / max(times[k + 1] - times[k], 1e-9)
    return(list(xyz = xyz[k, ] * (1 - f) + xyz[k + 1, ] * f, on_brain = TRUE))
  }
  move <- plan$floor_moves[[i]]
  tau <- t - move$t0
  if (tau < move$t_floor) {
    return(list(xyz = move$start + move$velocity * tau + c(0, 0, -0.5 * plan$g * tau^2),
                on_brain = FALSE))
  }
  u <- tau - move$t_floor
  hop <- bounce_after(u, move$bounce / 0.4, plan$g, 0.4, 0.05 * plan$speed)
  list(xyz = c(move$landing + move$horizontal * u, move$level + hop), on_brain = FALSE)
}

# Move points (n x 3) as the brain squishes at time t: a dent where each ball
# landed, which springs back and wobbles, and the whole brain squashing and
# stretching like jelly (it stands on the floor, so it is anchored at the
# bottom).
squish_points <- function(points, plan, t, squish, unit) {
  if (squish == 0) return(points)
  hits <- plan$impacts[plan$impacts$time <= t & plan$impacts$time > t - 1.2, ]
  wobble <- 0
  for (j in seq_len(nrow(hits))) {
    tau <- t - hits$time[j]
    strength <- squish * hits$speed[j] / plan$speed
    i <- hits$node[j]
    depth <- 7 * unit * strength * exp(-5 * tau) * sin(2 * pi * 4 * tau)
    weight <- exp(-rowSums(sweep(points, 2, plan$point[i, ])^2) / (2 * (12 * unit)^2))
    points <- points - outer(depth * weight, plan$normal[i, ])
    wobble <- wobble + 0.06 / sqrt(nrow(plan$point)) * strength * exp(-3 * tau) *
      sin(2 * pi * 2.5 * tau)
  }
  wobble <- max(min(wobble, 0.1), -0.1)
  points[, 3] <- plan$bottom + (points[, 3] - plan$bottom) * (1 - wobble)
  points[, 1:2] <- sweep(sweep(points[, 1:2, drop = FALSE], 2, plan$middle[1:2]) * (1 + wobble / 2),
                         2, plan$middle[1:2], "+")
  points
}

# A connection between two balls: an arc over the brain (a quadratic Bezier
# curve bowing outwards from the middle of the brain), from `from` to a
# fraction `grown` of the way to `to`.
connection_arc <- function(from, to, middle, unit, grown = 1, points = 24) {
  centre <- (from + to) / 2
  out <- centre - middle
  out <- if (sum(out^2) > 0) unit3(out) else c(0, 0, 1)
  control <- centre + out * (0.35 * sqrt(sum((to - from)^2)) + 8 * unit)
  s <- seq(0, grown, length.out = max(2, round(points * grown)))
  outer((1 - s)^2, from) + outer(2 * (1 - s) * s, control) + outer(s^2, to)
}

# One frame of the animation, as an RGBA array.
bounce_frame <- function(parts, plan, nodes, edges, camera, t, squish, res, quality, unit) {
  squished <- function(p) squish_points(p, plan, t, squish, unit)
  vertices <- squished(parts$mesh$vertices)
  vessels <- lapply(parts$vessels, function(m) {
    if (is.null(m)) return(NULL)
    m$vertices <- squished(m$vertices)
    m
  })
  rr <- function(name) getExportedValue("rayrender", name)
  ease <- function(u) {
    u <- min(max(u, 0), 1)
    u * u * (3 - 2 * u)
  }
  grown <- ease((t - plan$grow[1]) / diff(plan$grow))
  # The balls, darker as the path tracer's tone mapping lightens colours. The
  # ones that stuck swell as their connections grow.
  shown <- list()
  for (i in seq_len(nrow(nodes))) {
    p <- node_position(plan, i, t)
    if (is.null(p)) next
    xyz <- if (p$on_brain) squished(matrix(p$xyz, 1))[1, ] else p$xyz
    shown[[length(shown) + 1]] <- data.frame(x = xyz[1], y = xyz[2], z = xyz[3],
                                             radius = nodes$radius[i] *
                                               (1 + 0.6 * grown * plan$stick[i]),
                                             color = shade_colors(nodes$color[i], 0.4),
                                             stringsAsFactors = FALSE)
  }
  spheres <- if (length(shown) > 0) do.call(rbind, shown)
  # A white studio floor, under the brain
  size <- 50 * plan$height
  extra <- rr("xy_rect")(x = plan$middle[1], y = plan$middle[2], z = plan$floor,
                         xwidth = size, ywidth = size,
                         material = rr("diffuse")(color = "white"))
  # The connections, growing between the balls that stuck
  if (grown > 0 && nrow(edges) > 0) {
    ends <- squished(plan$centre)
    for (colour in unique(edges$color)) {
      these <- which(edges$color == colour)
      arcs <- lapply(these, function(k) {
        connection_arc(ends[edges$from[k], ], ends[edges$to[k], ], plan$middle, unit, grown)
      })
      radii <- lapply(seq_along(these), function(k) rep(edges$radius[these[k]], nrow(arcs[[k]])))
      m <- tube_mesh(arcs, radii)
      extra <- rr("add_object")(extra, rr("mesh3d_model")(
        as_mesh3d(m$vertices, m$faces, m$normals), override_material = TRUE,
        material = rr("glossy")(color = shade_colors(colour, 0.6), gloss = 0.9,
                                reflectance = 0.06)))
    }
  }
  # The floor's soft shadows are noisier, so the denoiser does one more pass and smooths
  # the floor more
  img <- render_pathtraced(parts, camera, res, samples = 32 * quality, unit, denoise = 3,
                           smooth.floor = TRUE,
                           vertices = vertices,
                           normals = vertex_normals(vertices, parts$mesh$faces),
                           vessels = vessels, spheres = spheres, extra = extra)
  over_background(img, "white")
}
