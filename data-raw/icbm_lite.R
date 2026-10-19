# A simplified ICBM152 surface (40,000 triangles instead of 163,840) for
# brainconn3D(): interactive plots rotate faster and pages are about a quarter
# of the size. Static renders with brainconn() still use the full mesh.
# Needs the Rvcg package (only for this script).
devtools::load_all()
full <- resolve_mesh("ICBM152")
m3d <- structure(list(vb = rbind(t(full$vertices), 1), it = t(full$faces)), class = "mesh3d")
lite <- Rvcg::vcgQEdecim(m3d, tarface = 40000, topo = TRUE, quality = TRUE, silent = TRUE)
icbm152_lite <- new_brain_mesh(round(t(lite$vb[1:3, ]), 3), t(lite$it))
usethis::use_data(icbm152_lite, internal = TRUE, overwrite = TRUE, compress = "xz")
