# Rat and macaque brain meshes and atlases for brainconn:
#   rat_mesh, rat_atlas:         Waxholm Space atlas of the Sprague Dawley rat
#                                brain v4 (Kleven et al. 2023), CC BY 4.0
#   macaque_mesh, macaque_atlas: NIMH Macaque Template v2 (NMT v2) with the CHARM
#                                level 2 parcellation (Jung et al. 2021), CC0
#
# Raw files are downloaded to data-raw/cache (not committed). Needs the RNifti
# and Rvcg packages (only for this script) and freesurferformats + gifti.
devtools::load_all()
cache <- file.path("data-raw", "cache")
dir.create(cache, showWarnings = FALSE)
download <- function(url, file) {
  path <- file.path(cache, file)
  if (!file.exists(path)) download.file(url, path, mode = "wb")
  path
}

# Simplify a mesh to about `faces` triangles (quadric edge collapse)
decimate <- function(mesh, faces) {
  m3d <- structure(list(vb = rbind(t(mesh$vertices), 1), it = t(mesh$faces)), class = "mesh3d")
  out <- Rvcg::vcgQEdecim(m3d, tarface = faces, topo = TRUE, quality = TRUE, silent = TRUE)
  new_brain_mesh(round(t(out$vb[1:3, ]), 3), t(out$it))
}


# Rat: Waxholm Space v4 via the BrainGlobe atlas package -----------------------
# BrainGlobe meshes are in micrometres on the atlas voxel grid in "asr"
# orientation (axes: anterior->posterior, superior->inferior, right->left).
# Convert to Waxholm Space (mm, RAS, origin at the decussation of the anterior
# commissure) using the WHS grid (512 x 1024 x 512 voxels of 39.0625 um).
rat_tar <- download("https://gin.g-node.org/BrainGlobe/atlases/raw/master/whs_sd_rat_39um_v1.2.tar.gz",
                    "whs_sd_rat_39um_v1.2.tar.gz")
rat_dir <- file.path(cache, "whs_sd_rat_39um_v1.2")
if (!dir.exists(rat_dir)) untar(rat_tar, exdir = cache)

read_obj <- function(file) {
  lines <- readLines(file)
  num <- function(prefix) {
    rows <- sub(paste0("^", prefix, " +"), "", lines[startsWith(lines, paste0(prefix, " "))])
    do.call(rbind, lapply(strsplit(rows, " +"), function(x) as.numeric(sub("/.*", "", x))))
  }
  list(vertices = num("v"), faces = num("f"))
}
to_waxholm <- function(a) {
  cbind(-9.53125 + (511 - a[, 3] / 39) * 0.0390625,
        -24.3359375 + (1023 - a[, 1] / 39) * 0.0390625,
        -9.6875 + (511 - a[, 2] / 39) * 0.0390625)
}
rat_obj <- function(id) {
  m <- read_obj(file.path(rat_dir, "meshes", paste0(id, ".obj")))
  m$vertices <- to_waxholm(m$vertices)
  m
}

# The brain surface comes from the label volume rather than the "Brain" mesh,
# which includes the optic, trigeminal, facial and vestibulocochlear nerves.
# The volume is read at half resolution (78 um), without the nerves, spinal cord
# and inner ear, and turned into a smoothed surface.
structures <- jsonlite::fromJSON(file.path(rat_dir, "structures.json"))
drop_parents <- c(41, 504, 1007, 121, 122, 45, 1049)
drop <- structures$id[vapply(structures$structure_id_path,
                             function(p) any(p %in% drop_parents), logical(1))]
pages <- seq(1, 1024, by = 2)
rows <- seq(1, 512, by = 2)
mask <- array(0L, c(length(pages), length(rows), length(rows)))
for (chunk in split(seq_along(pages), ceiling(seq_along(pages) / 32))) {
  imgs <- tiff::readTIFF(file.path(rat_dir, "annotation.tiff"), all = pages[chunk], as.is = TRUE)
  for (k in seq_along(chunk)) {
    lab <- imgs[[k]][rows, rows]
    mask[chunk[k], , ] <- (lab != 0 & !(lab %in% drop)) * 1L
  }
}
# Behind y = -16 mm only the periventricular grey and central canal remain,
# running alongside the (excluded) spinal cord, so the surface stops there
page_y <- -24.3359375 + (1023 - (pages - 1)) * 0.0390625
mask[page_y < -16, , ] <- 0L
surface <- Rvcg::vcgIsosurface(mask, threshold = 0.5, IJK2RAS = diag(4))
surface <- Rvcg::vcgIsolated(surface, silent = TRUE)   # keep the largest piece
surface <- Rvcg::vcgSmooth(surface, type = "taubin", iteration = 30)
rat_mesh <- decimate(new_brain_mesh(to_waxholm(t(surface$vb[1:3, ]) * 78), t(surface$it)), 40000)

# Regions (WHS structure ids) and the anatomical group used as their network
rat_regions <- data.frame(
  id = c(77, 1062, 1063, 1064, 1018, 1058, 1059, 1066, 1068, 1070, 444, 1019, 1056,
         1021, 1023, 1020, 501, 40, 93, 197, 1077, 1078, 193, 1085, 1089, 1090,
         1094, 1032, 1034, 1036, 1039, 58, 1047),
  network = c(rep("Cortex", 13), rep("Hippocampal region", 2), "Olfactory bulb",
              rep("Amygdala and septum", 3), rep("Basal ganglia", 4), rep("Thalamus", 5),
              "Hypothalamus", rep("Midbrain", 2), rep("Hindbrain", 2))
)
rat_atlas <- do.call(rbind, lapply(seq_len(nrow(rat_regions)), function(r) {
  id <- rat_regions$id[r]
  v <- rat_obj(id)$vertices
  info <- structures[structures$id == id, ]
  do.call(rbind, lapply(c("LH", "RH"), function(h) {
    side <- if (h == "LH") v[, 1] < 0 else v[, 1] > 0
    data.frame(ROI.Name = paste0(substr(h, 1, 1), "_", info$acronym),
               x.mni = mean(v[side, 1]), y.mni = mean(v[side, 2]), z.mni = mean(v[side, 3]),
               network = rat_regions$network[r], hemi = h, name = trimws(info$name),
               whs_id = id)
  }))
}))
rat_atlas <- rat_atlas[order(rat_atlas$hemi != "LH"), ]
rat_atlas[c("x.mni", "y.mni", "z.mni")] <- round(rat_atlas[c("x.mni", "y.mni", "z.mni")], 2)
rat_atlas$index <- seq_len(nrow(rat_atlas))
rownames(rat_atlas) <- NULL


# Macaque: NMT v2 symmetric template with CHARM level 2 -----------------------
nmt <- "https://templateflow.s3.amazonaws.com/tpl-NMT31Sym/"
nmt_file <- function(f) download(paste0(nmt, f), f)
lh <- read_brain_mesh(nmt_file("tpl-NMT31Sym_hemi-L_pial.surf.gii"))
rh <- read_brain_mesh(nmt_file("tpl-NMT31Sym_hemi-R_pial.surf.gii"))
lh <- decimate(lh, 25000)
rh <- decimate(rh, 25000)
macaque_mesh <- new_brain_mesh(rbind(lh$vertices, rh$vertices),
                               rbind(lh$faces, rh$faces + nrow(lh$vertices)))

charm2 <- RNifti::readNifti(nmt_file("tpl-NMT31Sym_res-2_atlas-CHARM_desc-level2_dseg.nii.gz"))
charm1 <- RNifti::readNifti(nmt_file("tpl-NMT31Sym_res-2_atlas-CHARM_desc-level1_dseg.nii.gz"))
hemi <- RNifti::readNifti(nmt_file("tpl-NMT31Sym_res-2_desc-hemi_mask.nii.gz"))
labels2 <- utils::read.delim(nmt_file("tpl-NMT31Sym_atlas-CHARM_desc-level2_dseg.tsv"))
labels1 <- utils::read.delim(nmt_file("tpl-NMT31Sym_atlas-CHARM_desc-level1_dseg.tsv"))
macaque_atlas <- do.call(rbind, lapply(c("LH", "RH"), function(h) {
  # hemi mask: 1 = right, 2 = left
  in_hemi <- hemi == if (h == "LH") 2 else 1
  do.call(rbind, lapply(seq_len(nrow(labels2)), function(r) {
    vox <- which(charm2 == labels2$Index[r] & in_hemi, arr.ind = TRUE)
    xyz <- RNifti::voxelToWorld(vox, charm2)
    parent <- as.integer(names(which.max(table(charm1[vox]))))
    lobe <- labels1$Abbreviation[labels1$Index == parent]
    data.frame(ROI.Name = paste0(substr(h, 1, 1), "_", labels2$Abbreviation[r]),
               x.mni = mean(xyz[, 1]), y.mni = mean(xyz[, 2]), z.mni = mean(xyz[, 3]),
               network = lobe, hemi = h,
               name = gsub("_", " ", labels2$Full_Name[r]), charm_id = labels2$Index[r])
  }))
}))
macaque_atlas[c("x.mni", "y.mni", "z.mni")] <- round(macaque_atlas[c("x.mni", "y.mni", "z.mni")], 2)
macaque_atlas$index <- seq_len(nrow(macaque_atlas))
rownames(macaque_atlas) <- NULL

usethis::use_data(rat_mesh, rat_atlas, macaque_mesh, macaque_atlas, overwrite = TRUE,
                  compress = "xz")
