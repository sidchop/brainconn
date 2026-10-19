# A closed ellipsoid mesh (latitude/longitude grid), by default about the size
# of a rat brain: 12 mm wide, 22 mm long, 9 mm tall.
ellipsoid_mesh <- function(radii = c(6, 11, 4.5), n_lat = 16, n_lon = 24) {
  lat <- seq(-pi / 2, pi / 2, length.out = n_lat)[-c(1, n_lat)]
  lon <- seq(0, 2 * pi, length.out = n_lon + 1)[-(n_lon + 1)]
  grid <- expand.grid(lon = lon, lat = lat)
  vertices <- rbind(cbind(radii[1] * cos(grid$lat) * cos(grid$lon),
                          radii[2] * cos(grid$lat) * sin(grid$lon),
                          radii[3] * sin(grid$lat)),
                    c(0, 0, -radii[3]), c(0, 0, radii[3]))
  ring <- function(i) (i - 1) * n_lon + seq_len(n_lon)
  faces <- list()
  for (i in seq_len(length(lat) - 1)) {
    a <- ring(i); b <- ring(i + 1)
    a2 <- c(a[-1], a[1]); b2 <- c(b[-1], b[1])
    faces[[length(faces) + 1]] <- cbind(a, a2, b)
    faces[[length(faces) + 1]] <- cbind(a2, b2, b)
  }
  south <- nrow(vertices) - 1; north <- nrow(vertices)
  first <- ring(1); last <- ring(length(lat))
  faces[[length(faces) + 1]] <- cbind(south, c(first[-1], first[1]), first)
  faces[[length(faces) + 1]] <- cbind(north, last, c(last[-1], last[1]))
  list(vertices = vertices, faces = unname(do.call(rbind, faces)))
}

# A small atlas inside the default ellipsoid: two regions per hemisphere
rat_atlas <- data.frame(ROI.Name = c("L_front", "L_back", "R_front", "R_back"),
                        x.mni = c(-3, -3, 3, 3), y.mni = c(6, -6, 6, -6),
                        z.mni = c(1, 0, 1, 0), network = c("A", "B", "A", "B"))
rat_conmat <- matrix(c(0, 1, 1, 0,
                       1, 0, 0, 1,
                       1, 0, 0, 1,
                       0, 1, 1, 0), 4, 4)
