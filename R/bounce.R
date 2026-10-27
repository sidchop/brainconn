#' Animate nodes bouncing off a squishy brain (experimental)
#'
#' \code{brainconn_bounce} makes a GIF in the realistic style of
#' \code{\link{brainconn}}: the nodes drop in from above and bounce off the
#' brain, which squishes where they land and jiggles like jelly. Some of them
#' roll down the brain and fall off; the others stick, then sink in, and the
#' network glows through the brain. It's just for fun, e.g. for the first slide
#' of a talk.
#'
#' Each node lands on the top of the brain surface above its own position.
#' Every frame is path traced (see \code{\link{brainconn}}), so making a GIF
#' takes a few minutes. This needs the rayrender and gifski packages.
#'
#' @inheritParams brainconn
#' @param file The GIF file to write.
#' @param view The camera angle, as in \code{\link{brainconn}}. Views from above (elevation above 0) work best, so the nodes fall down the image.
#' @param edge.color A colour for the edges, e.g. "white". Ignored when \code{edge.color.weighted = TRUE}.
#' @param mesh The brain surface, as in \code{\link{brainconn}} (but not \code{NULL}).
#' @param squish How squishy the brain is: 0 for solid, 1 (the default) for jelly, and more for wobblier.
#' @param roll The chance (0 to 1) that a node landing on a slope rolls off the brain rather than sticking.
#' @param fps Frames per second.
#' @param res Size in pixels of the longest side of the GIF.
#' @param quality Rays per pixel when path tracing, in multiples of 32. Lower is faster and grainier.
#' @param bg.color Background colour.
#' @param seed Random seed for when each node drops and which ones roll.
#'
#' @return The path of the GIF, invisibly.
#' @seealso \code{\link{brainconn}}, whose \code{style = "realistic"} is the last frame.
#' @examples
#' \dontrun{
#' brainconn_bounce("schaefer300_n7", example_unweighted_undirected, file = "bounce.gif")
#' }
#' @export
brainconn_bounce <- function(atlas,
                             conmat = NULL,
                             file = "brainconn-bounce.gif",
                             view = c(-125, 25),
                             node.size = 8,
                             node.color = "network",
                             all.nodes = FALSE,
                             edge.color = "#ffd98a",
                             edge.alpha = 0.8,
                             edge.width = 1,
                             edge.color.weighted = FALSE,
                             edge.palette = NULL,
                             edge.limits = NULL,
                             scale.edge.width = NULL,
                             thr = NULL,
                             uthr = NULL,
                             mesh = "ICBM152",
                             squish = 1,
                             roll = 0.5,
                             fps = 15,
                             res = 600,
                             quality = 2,
                             bg.color = "white",
                             seed = 1) {
  check_rayrender()
  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop("brainconn_bounce() needs the gifski package: install.packages(\"gifski\").",
         call. = FALSE)
  }
  if (is.null(mesh)) stop("brainconn_bounce() needs a brain surface (`mesh`).", call. = FALSE)
  if (is.na(bg.color)) stop("GIFs need a background colour (`bg.color`).", call. = FALSE)
  if (is.character(view)) view <- match.arg(view, names(camera_views))

  prepared <- prepare_network(resolve_atlas(atlas, parent.frame()), conmat, thr, uthr,
                              all.nodes, node.size, node.color, edge.color, edge.width,
                              edge.color.weighted, edge.palette, edge.limits, scale.edge.width,
                              mesh)
  nodes <- prepared$nodes
  unit <- prepared$unit
  parts <- realistic_parts(prepared$mesh, unit)
  vertices <- parts$mesh$vertices
  plan <- with_seed(seed, bounce_plan(parts, nodes, roll = roll))

  # The camera frames the brain and some room above it, where the nodes fall from
  top <- vertices[vertices[, 3] > stats::quantile(vertices[, 3], 0.8), , drop = FALSE]
  top[, 3] <- top[, 3] + 0.3 * plan$height
  camera <- fit_camera(view_camera(view), rbind(vertices, top))
  network <- network_layer(nodes, prepared$edges$table, edge.alpha, camera, res, 2, vertices)

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
      img <- bounce_frame(parts, plan, nodes, network, camera, t, squish, res, quality, unit,
                          bg.color)
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

# When and where each node lands, bounces and rolls. Times are in seconds.
# Each node drops from `drop` above its landing point (out of the image),
# after a random delay, and lands `fall` seconds later; each bounce keeps
# `restitution` of the speed, until the node comes to rest. Then, with chance
# `roll`, a node that can roll off the brain does so: it rolls downhill over
# the envelope of the brain (which bridges the sulci), speeding up, and falls
# off where the surface turns under.
bounce_plan <- function(parts, nodes, fall = 0.7, spread = 1.6, restitution = 0.5, roll = 0.5) {
  v <- parts$mesh$vertices
  normals <- parts$normals
  n <- nrow(nodes)
  height <- diff(range(v[, 3]))
  drop <- 1.1 * height
  g <- 2 * drop / fall^2
  speed <- g * fall
  bottom <- min(v[, 3])
  # Landing points: the top of the brain surface above each node
  up <- which(normals[, 3] > 0.2)
  radius <- 0.025 * max(apply(v, 2, function(x) diff(range(x))))
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

  # Rolling: steepest descent over the envelope, from the landing point
  env <- parts$envelope
  surface <- env$vertices + env$normals * env$height
  e <- parts$edges
  neighbours <- split(c(e[, 2], e[, 1]), factor(c(e[, 1], e[, 2]), levels = seq_len(nrow(v))))
  rolls <- stats::runif(n) < roll
  roller <- logical(n)
  path <- path_time <- exit_velocity <- vector("list", n)
  gone <- rep(Inf, n)
  for (i in which(rolls)) {
    walk <- land[i]
    off <- FALSE
    for (step in 1:2000) {
      here <- walk[length(walk)]
      next_to <- neighbours[[here]]
      lowest <- next_to[which.min(surface[next_to, 3])]
      if (surface[lowest, 3] >= surface[here, 3]) break
      walk <- c(walk, lowest)
      if (env$normals[lowest, 3] < -0.1) {
        off <- TRUE
        break
      }
    }
    if (!off || length(walk) < 5) next
    roller[i] <- TRUE
    xyz <- rbind(centre[i, ], surface[walk[-1], , drop = FALSE] +
                   env$normals[walk[-1], , drop = FALSE] * nodes$radius[i])
    m <- nrow(xyz)
    for (it in 1:4) xyz[2:(m - 1), ] <- (xyz[1:(m - 2), ] + 2 * xyz[2:(m - 1), ] + xyz[3:m, ]) / 4
    # A rolling ball speeds up as it goes down (5/7 of g, for a solid sphere)
    v_at <- sqrt((0.05 * speed)^2 + 2 * (5 / 7) * g * pmax(xyz[1, 3] - xyz[, 3], 0))
    seg <- sqrt(rowSums(diff(xyz)^2))
    path[[i]] <- xyz
    path_time[[i]] <- rest[i] + c(0, cumsum(seg / ((v_at[-1] + v_at[-m]) / 2)))
    direction <- xyz[m, ] - xyz[m - 1, ]
    exit_velocity[[i]] <- v_at[m] * direction / sqrt(sum(direction^2))
    # Free fall from the edge until well below the brain (out of the image)
    vz <- exit_velocity[[i]][3]
    below <- xyz[m, 3] - (bottom - height)
    gone[i] <- path_time[[i]][m] + (vz + sqrt(vz^2 + 2 * g * below)) / g
  }
  stuck <- !roller
  settle <- max(c(rest[stuck], gone[roller]), 0) + 0.6
  list(land = land, point = v[land, , drop = FALSE], normal = normals[land, , drop = FALSE],
       centre = centre, delay = delay, fall = fall, drop = drop, g = g, speed = speed,
       restitution = restitution, impacts = impacts, rest = rest, height = height,
       bottom = bottom, middle = colMeans(apply(v, 2, range)), roller = roller, path = path,
       path_time = path_time, exit_velocity = exit_velocity, gone = gone,
       sink = c(settle, settle + 1), still = settle + 1.6, end = settle + 2.8)
}

# Height of node i above its landing place at time t, while it falls and
# bounces.
bounce_height <- function(plan, i, t) {
  start <- plan$delay[i]
  if (t < start) return(plan$drop)
  hits <- plan$impacts[plan$impacts$node == i, ]
  if (t < hits$time[1]) return(plan$drop - 0.5 * plan$g * (t - start)^2)
  k <- max(which(hits$time <= t))
  if (k == nrow(hits)) return(0)
  launch <- plan$restitution * hits$speed[k]
  u <- t - hits$time[k]
  max(launch * u - 0.5 * plan$g * u^2, 0)
}

# Centre of node i at time t (before the brain squishes), or NULL once it has
# rolled off and fallen out of the image.
node_position <- function(plan, i, t) {
  if (!plan$roller[i] || t < plan$rest[i]) {
    return(plan$centre[i, ] + c(0, 0, bounce_height(plan, i, t)))
  }
  if (t >= plan$gone[i]) return(NULL)
  times <- plan$path_time[[i]]
  xyz <- plan$path[[i]]
  m <- length(times)
  if (t <= times[m]) {
    k <- findInterval(t, times, rightmost.closed = TRUE)
    f <- (t - times[k]) / max(times[k + 1] - times[k], 1e-9)
    return(xyz[k, ] * (1 - f) + xyz[k + 1, ] * f)
  }
  tau <- t - times[m]
  xyz[m, ] + plan$exit_velocity[[i]] * tau + c(0, 0, -0.5 * plan$g * tau^2)
}

# Move points (n x 3) as the brain squishes at time t: a dent where each node
# landed, which springs back and wobbles, and the whole brain squashing and
# stretching like jelly (anchored at the bottom).
squish_points <- function(points, plan, t, squish, unit) {
  if (squish == 0) return(points)
  hits <- plan$impacts[plan$impacts$time <= t & plan$impacts$time > t - 1.5, ]
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

# One frame of the animation, as an RGBA array on the background colour.
bounce_frame <- function(parts, plan, nodes, network, camera, t, squish, res, quality, unit,
                         bg.color) {
  squished <- function(p) squish_points(p, plan, t, squish, unit)
  vertices <- squished(parts$mesh$vertices)
  vessels <- lapply(parts$vessels, function(m) {
    if (is.null(m)) return(NULL)
    m$vertices <- squished(m$vertices)
    m
  })
  # Nodes: falling and bouncing, rolling off or resting on the surface, and
  # (those that stuck) sinking in
  ease <- function(u) {
    u <- min(max(u, 0), 1)
    u * u * (3 - 2 * u)
  }
  sinking <- ease((t - plan$sink[1]) / diff(plan$sink))
  spheres <- NULL
  if (sinking < 1) {
    shown <- list()
    for (i in seq_len(nrow(nodes))) {
      p <- node_position(plan, i, t)
      if (is.null(p)) next
      p <- squished(matrix(p, 1))[1, ]
      if (!plan$roller[i]) p <- p * (1 - sinking) + unlist(nodes[i, c("x", "y", "z")]) * sinking
      # Darker colours, as the path tracer's tone mapping lightens them
      shown[[length(shown) + 1]] <- data.frame(x = p[1], y = p[2], z = p[3],
                                               radius = nodes$radius[i],
                                               color = shade_colors(nodes$color[i], 0.4),
                                               stringsAsFactors = FALSE)
    }
    if (length(shown) > 0) spheres <- do.call(rbind, shown)
  }
  img <- render_pathtraced(parts, camera, res, samples = 32 * quality, unit,
                           vertices = vertices,
                           normals = vertex_normals(vertices, parts$mesh$faces),
                           vessels = vessels, spheres = spheres)
  composite_network(over_background(img, bg.color), network, res, fade = sinking)
}
